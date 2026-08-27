skip_if_not_installed("xgboost")

# A baseline that deliberately omits one genuinely predictive feature,
# alongside a pure-noise feature and a near-duplicate of the useful one.
set.seed(19)
nn <- 40000
dscr <- data.frame(
  LEEFTIJD = round(runif(nn, 18, 80)),
  REGIO    = factor(sample(c("Stad", "Dorp"), nn, TRUE)),
  KM       = round(pmax(rlnorm(nn, log(12000), .5), 1000), -2),
  RUIS     = round(rnorm(nn), 2),
  Exposure = round(runif(nn, 0.3, 1), 3))
dscr$KM_PROXY <- dscr$KM + round(rnorm(nn, 0, 200))
dscr$AantalClaims <- rpois(nn, dscr$Exposure * exp(
  -1.6 + 0.02 * pmax(0, 30 - dscr$LEEFTIJD) +
    0.35 * (dscr$REGIO == "Stad") +
    0.45 * (log(dscr$KM) - log(12000))))          # KM matters, RUIS does not

m_scr <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
             family = poisson(), data = dscr)

res <- suppressWarnings(
  screen_features(m_scr, seed = 4, nrounds = 300, nthread = 2,
                  early_stopping_rounds = 20, n_shap = 0))

test_that("screen_features returns the documented structure", {
  expect_true(all(c("summary", "verdict", "features", "gain_note",
                    "correlated", "stats", "plot") %in% names(res)))
  expect_equal(nrow(res$summary), 3)
  expect_s3_class(res$plot, "plotly")
  expect_identical(res$stats$objective, "count:poisson")
  # no scorable model is handed back
  expect_false(any(vapply(res, inherits, logical(1), "xgb.Booster")))
})

test_that("a genuinely predictive feature outranks pure noise", {
  f <- res$features
  # KM and KM_PROXY are near-duplicates, so they split the signal between
  # them and which of the two comes first is arbitrary. What must hold is
  # that the mileage signal tops the list and that noise does not.
  expect_true(f$Feature[1] %in% c("KM", "KM_PROXY"))
  expect_gt(f$PermDeviance[f$Feature == "KM"], 0)
  expect_gt(f$PermDeviance[f$Feature == "KM_PROXY"], 0)
  expect_lt(f$PermDeviance[f$Feature == "RUIS"],
            f$PermDeviance[f$Feature == "KM"])
  # noise carries no usable signal, whatever Gain suggests
  expect_lt(f$PermDeviance[f$Feature == "RUIS"], 1)
})

test_that("features already in the baseline show no incremental value", {
  f <- res$features
  expect_true(all(f$InModel[f$Feature %in% c("LEEFTIJD", "REGIO")]))
  expect_lt(max(abs(f$PermDeviance[f$Feature %in% c("LEEFTIJD", "REGIO")])),
            f$PermDeviance[f$Feature == "KM"])
})

test_that("the verdict points at the missing main effect", {
  expect_match(res$verdict, "main effects")
  expect_lt(res$stats$pct_depth1, 0)
})

test_that("near-duplicate candidates are reported", {
  expect_s3_class(res$correlated, "data.frame")
  expect_true(any(res$correlated$VarX == "KM" &
                    res$correlated$VarY == "KM_PROXY"))
})

test_that("awkward columns are dropped instead of crashing", {
  # Every one of these used to trigger "contrasts can be applied only to
  # factors with 2 or more levels" once features = NULL picked them up
  d2 <- dscr
  d2$CONST_F  <- factor(rep("N", nn))          # constant factor
  d2$CONST_N  <- 1                             # constant numeric
  d2$CHR      <- sample(c("p", "q", "r"), nn, TRUE)   # character column
  d2$WIDE     <- factor(sample(seq_len(200), nn, TRUE))  # 200 levels
  d2$DATUM    <- as.Date("2024-01-01") + sample(365, nn, TRUE)
  m2 <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = d2)

  expect_warning(
    r2 <- screen_features(m2, seed = 2, nrounds = 100, nthread = 2,
                          early_stopping_rounds = 15, n_shap = 0),
    "constant")
  expect_s3_class(r2$features, "data.frame")
  # dropped
  expect_false(any(c("CONST_F", "CONST_N", "WIDE", "DATUM") %in%
                     r2$features$Feature))
  # a character column is usable and keeps a stable width across splits
  expect_true("CHR" %in% r2$features$Feature)
  expect_true("KM" %in% r2$features$Feature)
})

test_that("a single-level factor no longer breaks the design matrix", {
  levs <- .screen_levels(dscr, "REGIO")
  one  <- data.frame(REGIO = factor(rep("Stad", 5), levels = levels(dscr$REGIO)))
  m <- .screen_matrix(one, "REGIO", levs)
  expect_equal(ncol(m$x), nlevels(dscr$REGIO))   # width follows the level map
  expect_equal(sum(m$x), 5)                      # one indicator per row
})

test_that("an uncorrelatable pair does not corrupt the near-duplicate table", {
  # which(arr.ind) skips NA correlations while the matching logical mask
  # returns one NA per skipped pair, so the values shifted onto the wrong
  # pairs and phantom rows appeared with VarX/VarY recycled
  set.seed(31)
  n <- 1500
  dc <- data.frame(Exposure = runif(n, .5, 1))
  dc$A <- rnorm(n)
  dc$B <- dc$A * 3 + rnorm(n, 0, .01)             # genuine near-duplicate
  dc$P <- ifelse(seq_len(n) <= n / 2, rnorm(n), NA)  # no overlap with Q
  dc$Q <- ifelse(seq_len(n) >  n / 2, rnorm(n), NA)
  dc$AantalClaims <- rpois(n, dc$Exposure * .12)
  mc <- glm(AantalClaims ~ 1 + offset(log(Exposure)), family = poisson(),
            data = dc)

  r <- suppressWarnings(suppressMessages(screen_features(
    mc, features = c("A", "B", "P", "Q"), n_shap = 0, nrounds = 20,
    nthread = 2, seed = 1)))

  expect_equal(nrow(r$correlated), 1)
  expect_false(anyNA(r$correlated$Correlation))
  expect_setequal(c(r$correlated$VarX, r$correlated$VarY), c("A", "B"))
  expect_gt(abs(r$correlated$Correlation), 0.99)

  # and the pair that could not be compared is reported rather than hidden
  expect_warning(
    screen_features(mc, features = c("A", "B", "P", "Q"), n_shap = 0,
                    nrounds = 20, nthread = 2, seed = 1),
    "could not be correlated")
})

test_that("unsupported families and bad splits are refused", {
  m_id <- glm(AantalClaims ~ LEEFTIJD, family = gaussian(), data = dscr)
  expect_error(screen_features(m_id), "supported")
  expect_error(screen_features(m_scr, split = c(0.5, 0.2, 0.2)), "split")
})

test_that("max_rows defaults to every row and accepts Inf or NULL", {
  # the default used to cap at 1e6 and required NULL to lift it, which is
  # a convention you had to know about
  expect_identical(eval(formals(screen_features)$max_rows), Inf)

  set.seed(81)
  nn <- 3000
  dm <- data.frame(x = runif(nn), Exposure = round(runif(nn, .3, 1), 3))
  dm$z <- runif(nn)
  dm$AantalClaims <- rpois(nn, dm$Exposure * exp(-2 + dm$x))
  mm <- glm(AantalClaims ~ 1 + offset(log(Exposure)), poisson(), dm)
  run <- function(...) suppressWarnings(suppressMessages(
    screen_features(mm, features = c("x", "z"), n_shap = 0, nrounds = 20,
                    nthread = 2, seed = 1, ...)))

  expect_equal(run()$stats$n_rows_used, nn)          # default: all of them
  expect_equal(run(max_rows = NULL)$stats$n_rows_used, nn)   # still works
  expect_equal(run(max_rows = Inf)$stats$n_rows_used, nn)
  expect_equal(run(max_rows = 1000)$stats$n_rows_used, 1000)

  # and a nonsensical value is refused rather than silently ignored
  expect_error(run(max_rows = 0), "at least 1")
  expect_error(run(max_rows = c(10, 20)), "single number")
  expect_error(run(max_rows = "veel"), "single number")
})

test_that("a single candidate does not lose the run to xgb.importance", {
  # xgboost 1.7 cannot read the dump of a booster built on one column, and
  # xgb.importance is called after every expensive stage is done. Gain is
  # the column this function says not to rank on, so it degrades to NA.
  set.seed(82)
  nn <- 2500
  d1 <- data.frame(x = runif(nn), Exposure = round(runif(nn, .3, 1), 3))
  d1$AantalClaims <- rpois(nn, d1$Exposure * exp(-2 + d1$x))
  m1 <- glm(AantalClaims ~ 1 + offset(log(Exposure)), poisson(), d1)

  r <- suppressMessages(suppressWarnings(
    screen_features(m1, features = "x", n_shap = 0, nrounds = 20,
                    nthread = 2, seed = 1)))
  expect_equal(nrow(r$features), 1)
  expect_true(is.numeric(r$features$PermDeviance))
  expect_false(is.na(r$features$PermDeviance))     # the useful column survives
  expect_s3_class(r$plot, "plotly")
})
