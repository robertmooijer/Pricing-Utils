# y_range is available on every plotting function, unused by default.

# Helper: primary y-axis of a built plotly object
yax <- function(p) plotly::plotly_build(p)$x$layout$yaxis
xax <- function(p) plotly::plotly_build(p)$x$layout$xaxis

test_that("all plot functions auto-scale the y-axis by default", {
  expect_true(isTRUE(yax(make_plot(dat, "REGIO", "Frequency", ta_blue, "F",
                                   "color", FALSE))$autorange))
  expect_true(isTRUE(yax(make_pdp(m_freq, dat, "REGIO"))$autorange))
  expect_true(isTRUE(yax(make_rating_plot(tbl, "REGIO"))$autorange))
  expect_true(isTRUE(yax(make_rating_plot(tbl2, "LEEFTIJD:REGIO"))$autorange))
  expect_true(isTRUE(yax(plot_glm_residuals(m_freq))$autorange))
  expect_true(isTRUE(yax(plot_glm_predictor(m_freq, "REGIO"))$autorange))
})

test_that("all plot functions honour a fixed y_range", {
  rng <- c(0, 0.6)
  ps <- list(
    make_plot(dat, "REGIO", "Frequency", ta_blue, "F", "color", FALSE,
              y_range = rng),
    make_pdp(m_freq, dat, "REGIO", y_range = rng),
    make_rating_plot(tbl, "REGIO", y_range = rng),
    make_rating_plot(tbl2, "LEEFTIJD:REGIO", y_range = rng),
    plot_glm_residuals(m_freq, y_range = rng),
    plot_glm_predictor(m_freq, "REGIO", y_range = rng)
  )
  for (p in ps) {
    ya <- yax(p)
    expect_equal(as.numeric(unlist(ya$range)), rng)
    expect_false(isTRUE(ya$autorange))
  }
})

test_that("make_plot facet mode pins all facets to the fixed range", {
  p <- make_plot(dat, "REGIO", "Frequency", ta_blue, "F", "facet", TRUE,
                 y_range = c(0, 0.6))
  b <- plotly::plotly_build(p)$x$layout
  # ggplotly emits yaxis, yaxis2, ... per facet; each must span the range
  yaxes <- b[grep("^yaxis", names(b))]
  expect_gt(length(yaxes), 1)
  for (ya in yaxes) {
    r <- as.numeric(unlist(ya$range))
    if (length(r) == 2) {
      expect_lte(r[1], 0.01)
      expect_gte(r[2], 0.59)
    }
  }
})

test_that("premium_impact exposes x_range and y_range on its histogram", {
  imp <- premium_impact(dat, model_freq_new = m_freq, model_sev_new = m_sev,
                        model_freq_old = m_freq, model_sev_old = m_sev)
  expect_true(isTRUE(xax(imp$plot)$autorange))

  imp2 <- premium_impact(dat, model_freq_new = m_freq, model_sev_new = m_sev,
                         model_freq_old = m_freq, model_sev_old = m_sev,
                         x_range = c(-25, 25), y_range = c(0, 5000))
  expect_equal(as.numeric(unlist(xax(imp2$plot)$range)), c(-25, 25))
  expect_equal(as.numeric(unlist(yax(imp2$plot)$range)), c(0, 5000))
  # fixing the view must not change the statistics
  expect_equal(imp$stats$rate_change, imp2$stats$rate_change)
})

test_that("an invalid range is rejected everywhere", {
  expect_error(make_plot(dat, "REGIO", "Frequency", ta_blue, "F", "color",
                         FALSE, y_range = c(1, 1)), "y_range")
  expect_error(make_pdp(m_freq, dat, "REGIO", y_range = 3), "y_range")
  expect_error(make_rating_plot(tbl, "REGIO", y_range = c(2, 1)), "y_range")
  expect_error(plot_glm_residuals(m_freq, y_range = c(NA, 1)), "y_range")
  expect_error(premium_impact(dat, model_freq_new = m_freq,
                              model_freq_old = m_freq,
                              x_range = c(5, 5)), "x_range")
})
