# The offset and the link are read off the model rather than assumed to be
# log(exposure) and a log link. These tests pin down both the cases that
# used to be wrong and the ones that must not change.

set.seed(51)
nol <- 5000
dol <- data.frame(R = factor(sample(c("N", "Z"), nol, TRUE)),
                  Exposure = round(runif(nol, .3, 1), 3))
dol$AantalClaims <- rpois(nol, dol$Exposure * exp(-2 + .4 * (dol$R == "Z")))
dol$Maanden <- dol$Exposure * 12
dol$SCHADELAST <- dol$AantalClaims * rgamma(nol, 3, scale = 700)

m_log <- glm(AantalClaims ~ R + offset(log(Exposure)), poisson(), dol)

test_that(".offset_value evaluates the model's own offset", {
  expect_equal(.offset_value(m_log, dol), log(dol$Exposure), tolerance = 1e-12)

  m_expr <- glm(AantalClaims ~ R + offset(log(Maanden / 12)), poisson(), dol)
  expect_equal(.offset_value(m_expr, dol), log(dol$Maanden / 12),
               tolerance = 1e-12)

  m_two <- glm(AantalClaims ~ R + offset(log(Exposure)) +
                 offset(rep(0.1, nol)), poisson(), dol)
  expect_equal(.offset_value(m_two, dol), log(dol$Exposure) + 0.1,
               tolerance = 1e-12)

  m_none <- glm(AantalClaims ~ R, poisson(), dol)
  expect_equal(.offset_value(m_none, dol), rep(0, nol))
})

test_that(".offset_is_log distinguishes a log offset from any other", {
  expect_true(.offset_is_log(m_log))
  expect_true(.offset_is_log(
    glm(AantalClaims ~ R + offset(log(Maanden / 12)), poisson(), dol)))
  expect_false(.offset_is_log(
    glm(AantalClaims ~ R + offset(Exposure), poisson(), dol)))
  expect_true(is.na(.offset_is_log(glm(AantalClaims ~ R, poisson(), dol))))
})

test_that("the usual log link with a log offset is unchanged", {
  # the whole point of deriving is that the common case must not move
  r <- .model_rate(m_log, dol, "Exposure", "t")
  manual <- exp(coef(m_log)[1] + coef(m_log)[2] * (dol$R == "Z"))
  expect_equal(r, as.vector(manual), tolerance = 1e-10)
  # and it still equals predicting at exposure = 1
  d1 <- dol; d1$Exposure <- 1
  expect_equal(r, as.numeric(predict(m_log, d1, type = "response")),
               tolerance = 1e-10)
})

test_that("an offset without a log is neutralised correctly", {
  # setting Exposure to 1 left the offset at 1 instead of 0, so every rate
  # came out a factor e too high
  m <- glm(AantalClaims ~ R + offset(Exposure), poisson(), dol)
  r <- .model_rate(m, dol, "Exposure", "t")
  manual <- exp(coef(m)[1] + coef(m)[2] * (dol$R == "Z"))
  expect_equal(r, as.vector(manual), tolerance = 1e-10)

  d1 <- dol; d1$Exposure <- 1
  old <- as.numeric(predict(m, d1, type = "response"))
  expect_equal(old / r, rep(exp(1), nol), tolerance = 1e-8)  # the old bug
})

test_that("an offset on an expression is neutralised correctly", {
  m <- glm(AantalClaims ~ R + offset(log(Maanden / 12)), poisson(), dol)
  r <- .model_rate(m, dol, "Exposure", "t")
  manual <- exp(coef(m)[1] + coef(m)[2] * (dol$R == "Z"))
  expect_equal(r, as.vector(manual), tolerance = 1e-10)

  d1 <- dol; d1$Maanden <- 1
  old <- as.numeric(predict(m, d1, type = "response"))
  expect_equal(r / old, rep(12, nol), tolerance = 1e-8)      # the old bug
})

test_that("a non-log link removes the offset and says what that means", {
  dg <- dol
  dg$y <- 5 + 2 * (dg$R == "Z") + dg$Exposure + rnorm(nol, 0, .3)
  m <- glm(y ~ R + offset(Exposure), gaussian(), dg)
  expect_warning(r <- .model_rate(m, dg, "Exposure", "t"),
                 "not a multiplicative exposure")
  manual <- coef(m)[1] + coef(m)[2] * (dg$R == "Z")
  expect_equal(suppressWarnings(.model_rate(m, dg, "Exposure", "t")),
               as.vector(manual), tolerance = 1e-10)
})

test_that("a model without an offset is untouched", {
  ds <- dol[dol$AantalClaims > 0, ]
  ds$AvgLoss <- ds$SCHADELAST / ds$AantalClaims
  m <- glm(AvgLoss ~ R, Gamma("log"), ds, weights = AantalClaims)
  expect_equal(.model_rate(m, ds, "Exposure", "t"),
               as.numeric(predict(m, ds, type = "response")),
               tolerance = 1e-12)
})

test_that("the rating table intercept follows the model's own offset", {
  # the factors are ratios and cancel the offset either way; the intercept
  # is the one number that carries it
  m <- glm(AantalClaims ~ R + offset(log(Maanden / 12)), poisson(), dol)
  tb <- suppressWarnings(make_rating_table(m, NULL, data = dol,
                                           exposure_col = "Exposure"))
  expect_equal(attr(tb, "intercept_frequency"),
               as.numeric(exp(coef(m)[1])), tolerance = 1e-10)

  # and the reconstruction identity still holds against a real prediction
  d1 <- dol[1, , drop = FALSE]
  d1$R <- factor("Z", levels(dol$R)); d1$Maanden <- 12   # offset = log(1) = 0
  s <- tb[tb$Variable == "R" & is.na(tb$Group), ]
  recon <- attr(tb, "intercept_frequency") *
           s$Factor_Frequency[match("Z", s$Level)]
  expect_equal(recon, as.numeric(predict(m, d1, type = "response")),
               tolerance = 1e-10)
})

test_that("make_pdp neutralises a non-log offset and keeps the collapse", {
  m <- glm(AantalClaims ~ R + offset(Exposure), poisson(), dol)
  p <- suppressWarnings(make_pdp(m, dol, "R", metric = "Frequency"))
  b <- plotly::plotly_build(p)
  pd <- NULL
  for (tr in b$x$data) if (identical(tr$name, "PDP (model)")) pd <- tr
  manual <- vapply(levels(dol$R), function(lv)
    as.numeric(exp(coef(m)[1] + coef(m)[2] * (lv == "Z"))), numeric(1))
  expect_equal(as.vector(pd$y), as.vector(manual[as.character(pd$x)]),
               tolerance = 1e-8)
})

test_that("exp(offset) is not read as an exposure when it is not one", {
  m <- glm(AantalClaims ~ R + offset(Exposure), poisson(), dol)
  expect_warning(plot_glm_predictor(m, "R"), "not a logarithm")
  expect_warning(.glm_ae_parts(m, "t"), "not a logarithm")
  # falls back to the prior weights rather than exp(Exposure)
  parts <- suppressWarnings(.glm_ae_parts(m, "t"))
  expect_identical(parts$exposure_label, "Weight")
})

test_that("an offset passed as an argument is neutralised too", {
  # glm(y ~ x, offset = log(E)) never reaches attr(terms, "offset"); it
  # survives only as the unevaluated call in model$call$offset, which is
  # exactly where predict.lm looks. Reading only the terms left the offset
  # in the prediction, silently, and 70% wrong.
  truth <- function(m) as.vector(exp(coef(m)[1] + coef(m)[2] * (dol$R == "Z")))

  m_arg <- glm(AantalClaims ~ R, poisson(), dol, offset = log(Exposure))
  expect_identical(attr(terms(m_arg), "offset"), NULL)   # not in the terms
  expect_identical(.offset_vars(m_arg), "Exposure")      # found anyway
  expect_true(.offset_is_log(m_arg))
  expect_equal(.model_rate(m_arg, dol, "Exposure", "t"), truth(m_arg),
               tolerance = 1e-10)

  # an expression, and one without a log, from the same place
  m_expr <- glm(AantalClaims ~ R, poisson(), dol, offset = log(Maanden / 12))
  expect_equal(.model_rate(m_expr, dol, "Exposure", "t"), truth(m_expr),
               tolerance = 1e-10)
  m_raw <- glm(AantalClaims ~ R, poisson(), dol, offset = Exposure)
  expect_false(.offset_is_log(m_raw))
  expect_equal(.model_rate(m_raw, dol, "Exposure", "t"), truth(m_raw),
               tolerance = 1e-10)
})

test_that("offsets from the formula and the argument add up", {
  # a model may carry both, and predict.lm sums them; so must we. The
  # argument's variables have to live in the data being scored, because
  # predict.lm evaluates model$call$offset against newdata alone.
  dd <- dol
  dd$const <- 0.1
  m <- glm(AantalClaims ~ R + offset(log(Exposure)), poisson(), dd,
           offset = const)
  eta_bare <- as.numeric(model.matrix(m) %*% coef(m))
  eta_pred <- as.numeric(predict(m, newdata = dd, type = "link"))
  # what predict adds is exactly what we take out
  expect_equal(.offset_value(m, dd), eta_pred - eta_bare, tolerance = 1e-10)
  expect_equal(.model_rate(m, dd, "Exposure", "t"), exp(eta_bare),
               tolerance = 1e-10)
  expect_setequal(.offset_vars(m), c("Exposure", "const"))
})

test_that("an offset that cannot be evaluated per row is refused", {
  # A pre-computed vector carries no expression to re-evaluate, only a
  # name, and what it holds belongs to the rows the model was fitted on.
  # Base R has the same limit - predict.lm evaluates the offset call
  # against newdata alone - so this must fail loudly rather than come back
  # with a prediction that still has the offset in it.
  dd <- dol
  dd$off <- log(dd$Exposure)
  m <- glm(AantalClaims ~ R, poisson(), dd, offset = off)
  expect_equal(.model_rate(m, dd, "Exposure", "t"),
               as.vector(exp(coef(m)[1] + coef(m)[2] * (dd$R == "Z"))),
               tolerance = 1e-10)

  # scoring data that does not carry the offset column at all
  expect_error(suppressWarnings(
    .model_rate(m, dd[, setdiff(names(dd), "off")], "Exposure", "t")),
    "could not be evaluated")

  # and a length that cannot line up with the rows being scored
  short <- dd[1:50, ]
  short$off <- log(dol$Exposure)[1:50]
  expect_equal(length(.offset_value(m, short)), 50L)
})

test_that("a second offset does not end up on the exposure axis", {
  # A known relativity carried as an offset - a bonus-malus scale - makes
  # exp(offset) equal to Exposure * BM. Dividing both series by that is
  # still like-for-like, but the axis then reads "per BM-adjusted
  # policy-year". Both series divide by the exposure itself instead, which
  # leaves the A/E untouched and the axis readable.
  set.seed(71)
  nb <- 15000
  db <- data.frame(R = factor(sample(c("N", "O", "Z"), nb, TRUE)),
                   L = round(runif(nb, 18, 80)),
                   Exposure = round(runif(nb, .25, 1), 3))
  db$BM <- ifelse(db$L < 30, 1.3, ifelse(db$L > 60, .6, .9))
  db$AantalClaims <- rpois(nb, db$Exposure * db$BM *
                             exp(-2.2 + .012 * db$L + .3 * (db$R == "Z")))
  db$SCHADELAST <- 0
  mb <- glm(AantalClaims ~ R + L + offset(log(Exposure)) + offset(log(BM)),
            poisson(), db)
  tr <- function(p, nm) {
    b <- plotly::plotly_build(p)
    for (t in b$x$data) if (identical(t$name, nm)) return(t)
    NULL
  }
  expect_setequal(.offset_vars(mb), c("Exposure", "BM"))
  expect_true(.offset_is_log(mb))

  p <- plot_glm_predictor(mb, "R")
  bar <- tr(p, "Exposure"); obs <- tr(p, "Observed"); pre <- tr(p, "Predicted")
  expect_equal(as.vector(bar$y),
               as.vector(tapply(db$Exposure, db$R, sum)[bar$x]),
               tolerance = 1e-8)
  expect_equal(as.vector(obs$y),
               as.vector((tapply(db$AantalClaims, db$R, sum) /
                          tapply(db$Exposure, db$R, sum))[obs$x]),
               tolerance = 1e-8)
  # and the comparison the plot exists for is unaffected
  expect_equal(as.vector(obs$y) / as.vector(pre$y), rep(1, length(obs$y)),
               tolerance = 1e-8)

  # the PDP average follows the same column as its own observed line
  pd <- tr(make_pdp(mb, db, "R", metric = "Frequency"), "PDP (model)")
  hand <- function(wt) vapply(levels(db$R), function(lv) {
    z <- db; z$R <- factor(lv, levels(db$R))
    weighted.mean(.predict_no_offset(mb, z), wt) }, numeric(1))
  expect_equal(as.vector(pd$y),
               as.vector(hand(db$Exposure)[as.character(pd$x)]),
               tolerance = 1e-8)
  # the two weightings really do differ here, so that was a live choice
  expect_false(isTRUE(all.equal(hand(db$Exposure),
                                hand(db$Exposure * db$BM))))

  # the premium chain still reconstructs, BM applied on top
  tb <- suppressWarnings(make_rating_table(mb, NULL, data = db))
  s <- tb[tb$Variable == "R" & is.na(tb$Group), ]
  sl <- tb[tb$Variable == "L" & is.na(tb$Group), ]
  nd <- data.frame(R = factor("Z", levels(db$R)), L = sl$LevelNum[5],
                   Exposure = 0.5, BM = 0.8)
  chain <- attr(tb, "intercept_frequency") *
           s$Factor_Frequency[match("Z", s$Level)] *
           sl$Factor_Frequency[5] * 0.8 * 0.5
  expect_equal(chain, as.numeric(predict(mb, nd, type = "response")),
               tolerance = 1e-10)
})

test_that("the exposure column falls back and can be named", {
  set.seed(72)
  nb <- 4000
  db <- data.frame(R = factor(sample(c("N", "Z"), nb, TRUE)),
                   Ex = round(runif(nb, .3, 1), 3))
  db$AantalClaims <- rpois(nb, db$Ex * .15)
  db$SCHADELAST <- 0
  mb <- glm(AantalClaims ~ R + offset(log(Ex)), poisson(), db)
  tr <- function(p) {
    b <- plotly::plotly_build(p)
    for (t in b$x$data) if (identical(t$name, "Exposure")) return(t)
    NULL
  }
  # no column called "Exposure": exp(offset) still recovers it
  expect_equal(as.vector(tr(suppressWarnings(plot_glm_predictor(mb, "R")))$y),
               as.vector(tapply(db$Ex, db$R, sum)), tolerance = 1e-8)
  # naming it is exact
  expect_equal(as.vector(tr(suppressWarnings(
                 plot_glm_predictor(mb, "R", exposure_col = "Ex")))$y),
               as.vector(tapply(db$Ex, db$R, sum)), tolerance = 1e-12)
})
