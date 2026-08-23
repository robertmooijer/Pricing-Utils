# A model that knows the truth must separate risk; a model that knows
# nothing must not. Those two ends bracket every lift statistic.

set.seed(77)
nl <- 40000
dl <- data.frame(
  LEEFTIJD = round(runif(nl, 18, 80)),
  REGIO    = factor(sample(c("Stad", "Dorp"), nl, TRUE)),
  Exposure = round(runif(nl, .3, 1), 3))
lin_l <- -1.6 + 0.035 * pmax(0, 30 - dl$LEEFTIJD) + 0.45 * (dl$REGIO == "Stad")
dl$AantalClaims <- rpois(nl, dl$Exposure * exp(lin_l))
dl$SCHADELAST   <- dl$AantalClaims * 1500

m_good <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
              family = poisson(), data = dl)
m_null <- glm(AantalClaims ~ 1 + offset(log(Exposure)),
              family = poisson(), data = dl)

test_that("lift separates a real model from an empty one", {
  g <- model_lift(m_good, actual_col = "AantalClaims", data = dl)
  # the intercept-only model predicts one rate for everyone, so the ten
  # bins are ten arbitrary slices of an identical prediction; that is
  # exactly what the tie warning is for
  expect_warning(z <- model_lift(m_null, actual_col = "AantalClaims",
                                 data = dl),
                 "only 1 distinct predicted value")

  expect_equal(nrow(g$table), 10)
  expect_gt(g$gini, 0.08)          # the real model orders risk
  expect_lt(abs(z$gini), 0.03)     # the intercept-only model cannot
  expect_gt(g$gini, z$gini)

  # the actual rate should rise across the bins of a working model
  expect_gt(g$table$ActualRate[10], g$table$ActualRate[1])
  expect_s3_class(g$plot, "plotly")
  expect_s3_class(g$plot_lorenz, "plotly")
})

test_that("bins hold roughly equal exposure", {
  g <- model_lift(m_good, actual_col = "AantalClaims", data = dl, n_bins = 5)
  expect_equal(nrow(g$table), 5)
  expect_lt(max(abs(g$table$ExposureShare - 0.2)), 0.01)
  expect_equal(sum(g$table$ExposureShare), 1, tolerance = 1e-8)
})

test_that("model_lift falls back to the model's own rows", {
  g <- model_lift(m_good, actual_col = "AantalClaims")
  expect_true(g$stats$in_sample)
  expect_equal(g$stats$n_rows, nl)
})

test_that("double lift prefers the model that is right where they differ", {
  dbl <- double_lift(dl, model_freq_new = m_good, model_freq_old = m_null,
                     actual_col = "AantalClaims")
  expect_equal(nrow(dbl$table), 10)
  expect_identical(dbl$stats$winner, "new")
  expect_lt(dbl$stats$mad_new, dbl$stats$mad_old)
  # the ratio must be increasing across the bins by construction
  expect_true(all(diff(dbl$table$RateRatio) > 0))
  expect_s3_class(dbl$plot, "plotly")
})

test_that("double lift against itself shows no winner and a flat A/E", {
  # comparing a model with itself gives every policy a ratio of exactly 1
  expect_warning(dbl <- double_lift(dl, model_freq_new = m_good,
                                    model_freq_old = m_good,
                                    actual_col = "AantalClaims"),
                 "only 1 distinct predicted value")
  expect_equal(dbl$table$AE_New, dbl$table$AE_Old, tolerance = 1e-10)
  expect_equal(dbl$stats$mad_new, dbl$stats$mad_old, tolerance = 1e-10)
  expect_lt(abs(dbl$stats$rate_level_change), 1e-10)
})

test_that("the rate level change is reported whether or not we rebase", {
  # the level difference belongs to the two tariffs, not to the decision to
  # rebase: deriving it from the scaling factor reported 0 for rebase =
  # FALSE, which is the one case where the difference is still on screen
  d2 <- dl
  d2$OldPrem <- .model_rate(m_good, dl, "Exposure", "t") * 0.8 * dl$Exposure
  f <- function(rb) double_lift(d2, model_freq_new = m_good,
                                old_premium_col = "OldPrem",
                                actual_col = "AantalClaims",
                                rebase = rb)$stats$rate_level_change
  expect_equal(f(TRUE),  0.25, tolerance = 1e-8)
  expect_equal(f(FALSE), 0.25, tolerance = 1e-8)
})

test_that("a portfolio without losses gives NA rather than NaN for the Gini", {
  d0 <- dl
  d0$AantalClaims <- 0
  g <- model_lift(m_good, actual_col = "AantalClaims", data = d0)
  expect_true(is.na(g$gini))
  expect_false(is.nan(g$gini))
  expect_s3_class(g$plot, "plotly")
})

test_that("mismatched units are caught", {
  # a frequency model compared against loss amounts: the overall A/E is
  # then off by orders of magnitude and the chart would be meaningless
  expect_warning(model_lift(m_good, data = dl, actual_col = "SCHADELAST"),
                 "not the same quantity")
  expect_warning(double_lift(dl, model_freq_new = m_good,
                             model_freq_old = m_null,
                             actual_col = "SCHADELAST"),
                 "not the same quantity")
  # matching units stay silent
  expect_silent(model_lift(m_good, data = dl, actual_col = "AantalClaims"))
})

test_that("lift functions validate their input", {
  expect_error(model_lift(), "at least one model")
  expect_error(model_lift(m_good, data = dl, n_bins = 1), "n_bins")
  expect_error(model_lift(m_good, data = dl, actual_col = "NOPE"), "NOPE")
  expect_error(double_lift(dl, model_freq_new = m_good), "old")
  # an existing column, so the mutual-exclusion check is what fires
  expect_error(double_lift(dl, model_freq_new = m_good,
                           model_freq_old = m_null,
                           old_premium_col = "Exposure"),
               "not both")
})
