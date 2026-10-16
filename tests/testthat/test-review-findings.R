# Regression tests from the external review of commit 58a9366 (October
# 2026). The first sixteen are the reviewer's own, translated; each failed on
# that commit for the reason in its name. The rest pin the behaviour that
# came with the fixes: intervals, the cautious double-lift verdict, `at`,
# standard errors and the scoring messages.

rv_book <- function(n = 20000, seed = 1, theta = Inf) {
  set.seed(seed)
  d <- data.frame(
    LEEFTIJD  = round(runif(n, 18, 85)),
    REGIO     = factor(sample(c("Noord", "Zuid", "West"), n, TRUE,
                              c(.5, .3, .2))),
    BRANDSTOF = factor(sample(c("Benzine", "Diesel", "Waterstof"), n, TRUE,
                              c(.7, .295, .005))),
    GEWICHT   = round(runif(n, 800, 2200), -1),
    BOEKJAAR  = sample(2021:2024, n, TRUE),
    Exposure  = round(runif(n, 0.1, 1), 3))
  eta <- log(0.08) + 0.6 * exp(-(d$LEEFTIJD - 18) / 8) +
    c(Noord = 0, Zuid = .2, West = .35)[as.character(d$REGIO)] +
    0.3 * log(d$GEWICHT / 1300)
  fr <- if (is.finite(theta)) rgamma(n, theta, theta) else 1
  d$AantalClaims <- rpois(n, d$Exposure * exp(eta) * fr)
  d$SCHADELAST <- ifelse(d$AantalClaims > 0,
                         d$AantalClaims * rgamma(n, 1.5, 1.5 / 2500), 0)
  d$PREMIE_OUD <- d$Exposure * 300 * exp(0.2 * (d$REGIO == "West"))
  d
}
rv_sev <- function(d) {
  s <- d[d$AantalClaims > 0, ]
  s$AvgLoss <- s$SCHADELAST / s$AantalClaims
  s
}
rv  <- rv_book()
rvs <- rv_sev(rv)
# Every warning an expression raises, so a test can look for one among
# several without the others leaking into the test output
rv_warns <- function(expr) {
  w <- character(0)
  withCallingHandlers(expr, warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  w
}
rv_trace <- function(p, nm) {
  b <- plotly::plotly_build(p)$x$data
  as.numeric(unlist(b[[which(vapply(b, function(t) identical(t$name, nm),
                                    logical(1)))[1]]]$y))
}

# The reviewer's sixteen ------------------------------------------------------

test_that("a level without claims does not drop the variable from the table", {
  d1 <- rv; z <- d1$BRANDSTOF == "Waterstof"
  d1$AantalClaims[z] <- 0; d1$SCHADELAST[z] <- 0
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + BRANDSTOF +
              offset(log(Exposure)), family = poisson(), data = d1)
  ms <- glm(AvgLoss ~ REGIO + BRANDSTOF, family = Gamma(link = "log"),
            data = rv_sev(d1), weights = AantalClaims)
  tb <- suppressWarnings(make_rating_table(mf, ms, data = d1))
  expect_true("BRANDSTOF" %in% tb$Variable)
  # the severity model never saw the level: an NA cell, said in a warning
  w <- tb[tb$Variable == "BRANDSTOF" & tb$Level == "Waterstof", ]
  expect_true(is.na(w$Factor_Severity))
  expect_true(is.na(w$Factor_Premium))
  expect_true(is.finite(w$Factor_Frequency))
  w <- rv_warns(make_rating_table(mf, ms, data = d1))
  expect_true(any(grepl("no severity factor for BRANDSTOF = Waterstof", w)))
  # and the level without claims is named among the thin ones
  expect_true(any(grepl("no claims at all (BRANDSTOF = Waterstof)", w,
                        fixed = TRUE)))
})

test_that("cut() in the formula does not give a table full of NA", {
  mf <- glm(AantalClaims ~ cut(LEEFTIJD, c(17, 24, 40, 65, 99)) + REGIO +
              offset(log(Exposure)), family = poisson(), data = rv)
  tb <- suppressWarnings(make_rating_table(mf, NULL, data = rv))
  expect_false(is.na(attr(tb, "intercept_frequency")))
  expect_false(anyNA(tb$Factor_Frequency[tb$Variable == "REGIO"]))
  # the bands are the levels, and intercept x factors is the prediction
  s <- tb[tb$Variable == "LEEFTIJD", ]
  expect_setequal(s$Level, c("(17,24]", "(24,40]", "(40,65]", "(65,99]"))
  nd <- data.frame(LEEFTIJD = 30, REGIO = factor("West", levels(rv$REGIO)),
                   Exposure = 1)
  f <- function(v, l) tb$Factor_Frequency[tb$Variable == v & tb$Level == l]
  expect_equal(attr(tb, "intercept_frequency") * f("LEEFTIJD", "(24,40]") *
                 f("REGIO", "West"),
               unname(predict(mf, nd, type = "response")), tolerance = 1e-10)
})

test_that("a logical predictor does not break the table", {
  d1 <- rv; d1$GARAGE <- d1$LEEFTIJD %% 3 == 0
  mf <- glm(AantalClaims ~ REGIO + GARAGE + offset(log(Exposure)),
            family = poisson(), data = d1)
  expect_no_error(tb <- make_rating_table(mf, NULL, data = d1))
  expect_setequal(tb$Level[tb$Variable == "GARAGE"], c("FALSE", "TRUE"))
})

test_that("na.exclude gives the same result as na.omit", {
  d1 <- rv; d1$GEWICHT[seq(1, nrow(d1), by = 50)] <- NA
  f <- AantalClaims ~ LEEFTIJD + REGIO + log(GEWICHT) + offset(log(Exposure))
  m_o <- glm(f, family = poisson(), data = d1, na.action = na.omit)
  m_x <- glm(f, family = poisson(), data = d1, na.action = na.exclude)
  expect_no_error(glm_diagnostics(m_x))
  expect_equal(glm_diagnostics(m_x)$Dispersion,
               glm_diagnostics(m_o)$Dispersion)
  expect_no_error(plot_glm_predictor(m_x, "LEEFTIJD"))
  expect_no_error(plot_glm_residuals(m_x, "LEEFTIJD"))
  h_o <- attr(plot_residual_heatmap(m_o, "LEEFTIJD", "REGIO"), "cells")
  h_x <- attr(plot_residual_heatmap(m_x, "LEEFTIJD", "REGIO"), "cells")
  expect_equal(h_x$Expected[order(h_x$gx, h_x$gy)],
               h_o$Expected[order(h_o$gx, h_o$gy)])
  # the Q-Q plot was silently wrong: fitted values recycled against y
  q_o <- attr(plot_glm_qq(m_o, seed = 3), "residuals")
  q_x <- attr(plot_glm_qq(m_x, seed = 3), "residuals")
  expect_equal(q_x, q_o)
})

test_that("trim does not take exposure out of the slices of a group variable", {
  mf <- glm(AantalClaims ~ LEEFTIJD * log(GEWICHT) + REGIO +
              offset(log(Exposure)), family = poisson(), data = rv)
  tb <- suppressWarnings(make_rating_table(mf, NULL, data = rv,
                                           trim = c(0.005, 0.995)))
  main <- sum(tb$Exposure[tb$Variable == "LEEFTIJD" & is.na(tb$Group)])
  int  <- sum(tb$Exposure[!is.na(tb$Group)])
  expect_equal(int, main, tolerance = 0.01)
})

test_that("the Tariff sheet instruction rebuilds the premium with an interaction", {
  skip_if_not_installed("openxlsx")
  mf <- glm(AantalClaims ~ LEEFTIJD * REGIO + offset(log(Exposure)),
            family = poisson(), data = rv)
  tb <- make_rating_table(mf, NULL, data = rv)
  f  <- tempfile(fileext = ".xlsx"); export_rating_table(tb, f)
  tf <- openxlsx::read.xlsx(f, sheet = "Tariff", startRow = 2)
  p  <- rv[1:200, ]; p1 <- p; p1$Exposure <- 1
  look <- function(k) tf$Factor_Frequency[match(k, tf$Key)]
  iv <- unique(tb$Variable[!is.na(tb$Group)])
  lit <- attr(tb, "intercept_frequency") *
    look(paste0("LEEFTIJD|", p$LEEFTIJD)) * look(paste0("REGIO|", p$REGIO)) *
    look(paste0(iv, "|", p$LEEFTIJD, "|", p$REGIO))
  expect_equal(lit, unname(predict(mf, newdata = p1, type = "response")),
               tolerance = 1e-6)
})

test_that("the intercepts on the Overview sheet are numbers, not text", {
  skip_if_not_installed("openxlsx")
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = rv)
  f <- tempfile(fileext = ".xlsx")
  export_rating_table(make_rating_table(mf, NULL, data = rv), f)
  v <- openxlsx::read.xlsx(f, sheet = "Overview", rows = 3, cols = 2,
                           colNames = FALSE)[1, 1]
  expect_true(is.numeric(v))
})

test_that("premium_impact contributions add up to the change in total premium", {
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = rv)
  ms <- glm(AvgLoss ~ REGIO, family = Gamma(link = "log"), data = rvs,
            weights = AantalClaims)
  im <- premium_impact(rv, mf, ms, old_premium_col = "PREMIE_OUD",
                       by = "REGIO", rebase = TRUE)
  expect_equal(sum(im$by_level$Contribution), 0, tolerance = 1e-6)
})

test_that("the PDP sits on the observed line with a bonus-malus offset", {
  d1 <- rv; set.seed(2)
  d1$BM <- sample(c(1, .85, .7, .55), nrow(d1), TRUE)
  eta <- log(0.08) + c(Noord = 0, Zuid = .2, West = .35)[as.character(d1$REGIO)]
  d1$AantalClaims <- rpois(nrow(d1), d1$Exposure * d1$BM * exp(eta))
  m <- glm(AantalClaims ~ REGIO + offset(log(Exposure)) + offset(log(BM)),
           family = poisson(), data = d1)
  p <- suppressMessages(make_pdp(m, d1, "REGIO"))
  expect_equal(rv_trace(p, "PDP (model)"), rv_trace(p, "Observed"),
               tolerance = 0.05)
})

test_that("the interaction scan is not fooled by overdispersion", {
  set.seed(9); n <- 6000
  d1 <- data.frame(A = factor(sample(1:5, n, TRUE)),
                   B = factor(sample(1:6, n, TRUE)),
                   C = factor(sample(1:4, n, TRUE)),
                   Exposure = runif(n, 5, 30))
  mu <- d1$Exposure * 0.1 * exp(0.1 * as.integer(d1$A))
  d1$AantalClaims <- rpois(n, mu * rgamma(n, 3, 3))   # dispersion ~1.9
  m <- glm(AantalClaims ~ A + B + C + offset(log(Exposure)),
           family = quasipoisson(), data = d1)
  di <- suppressMessages(detect_interactions(m, n_sim = 100, seed = 1))
  expect_true(all(di$P > 0.01))
  # the dispersion of a cell total, not the row-level Pearson one
  phi_row <- sum(residuals(m, "pearson")^2) / m$df.residual
  expect_gt(di$Dispersion[1], phi_row)
  expect_equal(di$Dispersion[1],
               sum((d1$AantalClaims - fitted(m))^2) / sum(fitted(m)) *
                 n / m$df.residual, tolerance = 1e-10)
  expect_message(detect_interactions(m, n_sim = 10, seed = 1), "overdispersed")
})

test_that("binned deviance residuals of a correct Poisson model stay in the band", {
  m <- glm(AantalClaims ~ LEEFTIJD + REGIO + log(GEWICHT) +
             offset(log(Exposure)), family = poisson(), data = rv)
  p <- plot_glm_residuals(m, residual_type = "deviance")
  b <- plotly::plotly_build(p)$x$data
  y <- rv_trace(p, "Mean residual")
  band <- as.numeric(unlist(b[[1]]$y))
  expect_lt(mean(abs(y) > band), 0.15)
  # the band follows the bin instead of being one number
  expect_gt(length(unique(round(band, 6))), 1)
})

test_that("screen_features does not take the loss amount as a candidate", {
  skip_if_not_installed("xgboost")
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = rv)
  sc <- suppressWarnings(suppressMessages(
    screen_features(mf, seed = 1, nrounds = 100, n_shap = 0, nthread = 1)))
  expect_false("SCHADELAST" %in% sc$features$Feature)
  expect_message(suppressWarnings(
    screen_features(mf, seed = 1, nrounds = 20, n_shap = 0, nthread = 1)),
    "left out of the candidates: SCHADELAST")
  # asked for by name it stays, with a warning
  expect_warning(suppressMessages(
    screen_features(mf, features = c("GEWICHT", "SCHADELAST"), seed = 1,
                    nrounds = 20, n_shap = 0, nthread = 1)),
    "outcome or volume column")
})

test_that("an unknown level at scoring is not reported as an offset problem", {
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = rv)
  n1 <- rv[1:20, ]; n1$REGIO <- as.character(n1$REGIO); n1$REGIO[1] <- "Oost"
  msg <- tryCatch(premium_impact(n1, mf, NULL, old_premium_col = "PREMIE_OUD"),
                  error = conditionMessage)
  expect_no_match(msg, "offset")
  expect_match(msg, "'REGIO' = Oost \\(1 row\\)")
  expect_match(msg, "new frequency model")
})

test_that("pricing_report writes a report for a small book too", {
  skip_if_not_installed("htmltools")
  d1 <- rv_book(n = 400, seed = 5)
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = d1)
  f <- tempfile(fileext = ".html")
  expect_no_error(suppressWarnings(suppressMessages(
    pricing_report(mf, NULL, d1, file = f, grid_res = 10))))
  expect_true(file.exists(f))
})

test_that("a glm.nb model works in make_pdp and the A/E plot", {
  skip_if_not_installed("MASS")
  m <- suppressWarnings(MASS::glm.nb(AantalClaims ~ LEEFTIJD + REGIO +
                                       offset(log(Exposure)), data = rv))
  expect_null(m$data)
  expect_no_error(suppressWarnings(make_pdp(m, rv, "REGIO")))
  expect_no_error(plot_glm_predictor(m, "REGIO"))
  # and its Q-Q plot gets quantile residuals rather than the fallback
  q <- plot_glm_qq(m, seed = 1)
  expect_identical(attr(q, "type"), "quantile")
})

test_that("a double lift between two severity models is weighted by claims", {
  m1 <- glm(AvgLoss ~ REGIO + log(GEWICHT), family = Gamma(link = "log"),
            data = rvs, weights = AantalClaims)
  m0 <- glm(AvgLoss ~ REGIO, family = Gamma(link = "log"), data = rvs,
            weights = AantalClaims)
  dl <- suppressWarnings(double_lift(rvs, model_sev_new = m1,
                                     model_sev_old = m0, rebase = FALSE))
  tb <- dl$table
  tot_ae <- sum(tb$ActualRate * tb$Exposure) /
    sum(tb$Exposure * tb$ActualRate / tb$AE_Old)
  expect_equal(tot_ae, 1, tolerance = 0.02)
  expect_identical(dl$stats$weight_col, "AantalClaims")
  expect_equal(sum(tb$Exposure), sum(rvs$AantalClaims))
})

# What came with the fixes ----------------------------------------------------

test_that("model_lift reports a bootstrap interval around the Gini", {
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = rv)
  l <- model_lift(mf, data = rv, actual_col = "AantalClaims", seed = 4)
  expect_length(l$gini_ci, 2)
  expect_true(l$gini_ci[1] < l$gini && l$gini < l$gini_ci[2])
  expect_identical(l$stats$gini_ci, l$gini_ci)
  # the same seed gives the same interval; n_boot = 0 switches it off
  expect_identical(model_lift(mf, data = rv, actual_col = "AantalClaims",
                              seed = 4)$gini_ci, l$gini_ci)
  l0 <- model_lift(mf, data = rv, actual_col = "AantalClaims", n_boot = 0)
  expect_true(all(is.na(l0$gini_ci)))
  expect_error(model_lift(mf, data = rv, n_boot = 1.5), "n_boot")
  expect_error(model_lift(mf, data = rv, conf_level = 0), "conf_level")
})

test_that("large losses that decide the Gini or the verdict are named", {
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = rv)
  mf0 <- glm(AantalClaims ~ REGIO + offset(log(Exposure)),
             family = poisson(), data = rv)
  ms <- glm(AvgLoss ~ REGIO, family = Gamma(link = "log"), data = rvs,
            weights = AantalClaims)
  # without large losses a modest interval is shown, not warned about
  expect_no_warning(model_lift(mf, ms, data = rv, seed = 1))
  # ten large losses carrying a third of the total
  d1 <- rv; set.seed(5)
  big <- sample(which(d1$AantalClaims > 0), 10)
  d1$SCHADELAST[big] <- 150000
  expect_gt(.top_share(d1$SCHADELAST), 0.10)
  expect_warning(model_lift(mf, ms, data = d1, seed = 1),
                 "ten largest records carry")
  w <- rv_warns(dl <- double_lift(d1, mf, ms, model_freq_old = mf0,
                                  model_sev_old = ms, seed = 1))
  expect_identical(dl$stats$winner, "inconclusive")
  expect_true(any(grepl("ten largest records carry", w)))
})

test_that("the double lift names a winner only when the interval excludes 0", {
  set.seed(77)
  nl <- 30000
  dl <- data.frame(LEEFTIJD = round(runif(nl, 18, 80)),
                   REGIO = factor(sample(c("Stad", "Dorp"), nl, TRUE)),
                   Exposure = round(runif(nl, .3, 1), 3))
  dl$AantalClaims <- rpois(nl, dl$Exposure * exp(
    -1.6 + 0.035 * pmax(0, 30 - dl$LEEFTIJD) + 0.45 * (dl$REGIO == "Stad")))
  m_good <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
                family = poisson(), data = dl)
  m_null <- glm(AantalClaims ~ 1 + offset(log(Exposure)),
                family = poisson(), data = dl)
  m_reg  <- glm(AantalClaims ~ REGIO + LEEFTIJD + offset(log(Exposure)),
                family = poisson(), data = dl[seq(1, nl, 2), ])
  clear <- double_lift(dl, model_freq_new = m_good, model_freq_old = m_null,
                       actual_col = "AantalClaims", seed = 1)
  expect_identical(clear$stats$winner, "new")
  expect_gt(clear$stats$mad_diff_ci[1], 0)
  expect_equal(clear$stats$mad_diff,
               clear$stats$mad_old - clear$stats$mad_new)
  # two models fitted on the same signal cannot be told apart; without
  # large losses behind it that is the verdict, not a warning
  expect_no_warning(close <- double_lift(dl, model_freq_new = m_good,
                                         model_freq_old = m_reg,
                                         actual_col = "AantalClaims",
                                         seed = 1))
  expect_identical(close$stats$winner, "inconclusive")
  expect_lte(close$stats$mad_diff_ci[1], 0)
  expect_gte(close$stats$mad_diff_ci[2], 0)
  # without the bootstrap the point values decide, as before
  pt <- double_lift(dl, model_freq_new = m_good, model_freq_old = m_reg,
                    actual_col = "AantalClaims", n_boot = 0)
  expect_true(pt$stats$winner %in% c("new", "old"))
  expect_true(all(is.na(pt$stats$mad_diff_ci)))
})

test_that("a severity model against a frequency model is refused", {
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = rv)
  ms <- glm(AvgLoss ~ REGIO, family = Gamma(link = "log"), data = rvs,
            weights = AantalClaims)
  expect_error(double_lift(rvs, model_sev_new = ms, model_freq_old = mf,
                           model_sev_old = ms),
               "one side is a severity model")
})

test_that("dropped rows are reported per reason", {
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = rv)
  d1 <- rv; d1$SCHADELAST[1:3] <- NA
  ms <- glm(AvgLoss ~ REGIO, family = Gamma(link = "log"), data = rvs,
            weights = AantalClaims)
  expect_warning(model_lift(mf, ms, data = d1, n_boot = 0),
                 "missing 'SCHADELAST': 3")
  w <- tryCatch(double_lift(d1, mf, ms, old_premium_col = "PREMIE_OUD",
                            n_boot = 0), warning = conditionMessage)
  expect_match(w, "missing 'SCHADELAST': 3")
  expect_no_match(w, "exposure")
})

test_that("at prices every policy at one accounting year", {
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + factor(BOEKJAAR) +
              offset(log(Exposure)), family = poisson(), data = rv)
  # against a premium column the year is pointed out ...
  expect_warning(premium_impact(rv, mf, old_premium_col = "PREMIE_OUD"),
                 "own year")
  # ... and at = list(BOEKJAAR = 2024) prices every row at that year
  im <- premium_impact(rv, mf, old_premium_col = "PREMIE_OUD",
                       at = list(BOEKJAAR = 2024))
  nd <- rv; nd$BOEKJAAR <- 2024; nd$Exposure <- 1
  expect_equal(im$policy$NewRate, unname(predict(mf, nd, type = "response")),
               tolerance = 1e-10)
  # both sides with the year is like for like, so no warning
  expect_no_warning(premium_impact(rv, mf, model_freq_old = mf))
  # a year the model never saw is named, with the advice to use 'at'
  n2 <- rv[1:10, ]; n2$BOEKJAAR <- 2025
  expect_error(premium_impact(n2, mf, old_premium_col = "PREMIE_OUD",
                              year_col = NULL),
               "'BOEKJAAR' = 2025.*'at'")
  expect_error(premium_impact(rv, mf, old_premium_col = "PREMIE_OUD",
                              at = list(2024), year_col = NULL), "named")
  expect_error(premium_impact(rv, mf, old_premium_col = "PREMIE_OUD",
                              at = list(NOPE = 1), year_col = NULL), "NOPE")
  expect_error(premium_impact(rv, mf, old_premium_col = "PREMIE_OUD",
                              at = list(REGIO = "Oost"), year_col = NULL),
               "not a valid value for 'REGIO'")
})

test_that("scoring outside the fitted range is pointed out", {
  mf <- glm(AantalClaims ~ ns(LEEFTIJD, 3) + REGIO + offset(log(Exposure)),
            family = poisson(), data = rv)
  n1 <- rv[1:50, ]; n1$LEEFTIJD[1:2] <- 99
  expect_warning(premium_impact(n1, mf, old_premium_col = "PREMIE_OUD"),
                 "'LEEFTIJD' \\(2 rows outside 18 to 85\\)")
})

test_that("the rating table carries standard errors of its factors", {
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = rv)
  tb <- make_rating_table(mf, NULL, data = rv)
  r <- tb[tb$Variable == "REGIO", ]
  # a categorical level against the reference: the coefficient's own SE
  expect_equal(r$SE_Frequency[r$Level == "West"],
               unname(sqrt(diag(vcov(mf)))["REGIOWest"]), tolerance = 1e-8)
  expect_equal(r$SE_Frequency[r$IsBase], 0)
  # a linear term: |x - base| times the slope's SE
  a <- tb[tb$Variable == "LEEFTIJD", ]
  i <- which(a$LevelNum == 60)
  expect_equal(a$SE_Frequency[i],
               abs(60 - a$LevelNum[a$IsBase]) *
                 unname(sqrt(diag(vcov(mf)))["LEEFTIJD"]), tolerance = 1e-8)
  # no severity model, no severity SE
  expect_true(all(is.na(tb$SE_Severity)))
})

test_that("the accounting year sits at its latest level by default", {
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + factor(BOEKJAAR) +
              offset(log(Exposure)), family = poisson(), data = rv)
  tb <- make_rating_table(mf, NULL, data = rv)
  expect_identical(tb$Level[tb$Variable == "BOEKJAAR" & tb$IsBase], "2024")
  tb0 <- make_rating_table(mf, NULL, data = rv, year_col = NULL)
  expect_identical(tb0$Level[tb0$Variable == "BOEKJAAR" & tb0$IsBase], "2021")
  tb1 <- make_rating_table(mf, NULL, data = rv, base = list(BOEKJAAR = "2022"))
  expect_identical(tb1$Level[tb1$Variable == "BOEKJAAR" & tb1$IsBase], "2022")
})

test_that("claims without payment are pointed out", {
  d1 <- rv; set.seed(12)
  nil <- d1$AantalClaims > 0 & runif(nrow(d1)) < 0.15
  d1$SCHADELAST[nil] <- 0
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = d1)
  ms <- glm(AvgLoss ~ REGIO, family = Gamma(link = "log"),
            data = rv_sev(d1)[rv_sev(d1)$SCHADELAST > 0, ],
            weights = AantalClaims)
  expect_warning(make_rating_table(mf, ms, data = d1), "without payment")
  expect_warning(model_lift(mf, ms, data = d1, n_boot = 0), "without payment")
})

test_that("a heatmap with every cell thin still builds", {
  d1 <- rv_book(n = 400, seed = 5)
  mf <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = d1)
  expect_warning(p <- plot_residual_heatmap(mf, "LEEFTIJD", "REGIO"),
                 "none is coloured")
  expect_no_error(plotly::plotly_build(p))
})

test_that("the report passes merge_vars and base to its rating table", {
  skip_if_not_installed("htmltools")
  d1 <- rv
  d1$REGIO_GRP <- factor(ifelse(d1$REGIO == "Noord", "Noord", "Rest"))
  mf <- glm(AantalClaims ~ REGIO + LEEFTIJD + offset(log(Exposure)),
            family = poisson(), data = d1)
  s1 <- rv_sev(d1)
  ms <- glm(AvgLoss ~ REGIO_GRP, family = Gamma(link = "log"), data = s1,
            weights = AantalClaims)
  f <- tempfile(fileext = ".html")
  rep_warn <- function(...) {
    w <- character(0)
    withCallingHandlers(
      suppressMessages(pricing_report(mf, ms, d1, file = f, grid_res = 10,
                                      include = "rating", ...)),
      warning = function(cnd) {
        w <<- c(w, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      })
    w
  }
  # without the declaration the report's table holds two blocks, and says so
  expect_true(any(grepl("looks like a regrouping", rep_warn())))
  # with it, one block, exactly as make_rating_table() builds it
  expect_false(any(grepl("looks like a regrouping",
                         rep_warn(merge_vars = list(REGIO = "REGIO_GRP")))))
  expect_true(file.exists(f))
})
