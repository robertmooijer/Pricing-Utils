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
