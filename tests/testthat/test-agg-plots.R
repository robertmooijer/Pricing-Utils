test_that("agg_all works with custom column names and validates input", {
  dat2 <- dat
  names(dat2)[names(dat2) == "Exposure"] <- "EXPO"
  a <- agg_all(dat2, "REGIO", by_year = TRUE, exposure_col = "EXPO")
  expect_true(all(c("Exposure", "ClaimCount", "Loss",
                    "Frequency", "Severity", "Year") %in% names(a)))
  expect_error(agg_all(dat, "REGIO", FALSE, exposure_col = "DOES_NOT_EXIST"),
               "DOES_NOT_EXIST")
})

test_that("plot_glm_predictor returns plotly objects", {
  expect_s3_class(plot_glm_predictor(m_freq, "LEEFTIJD", n_bins = 30),
                  "plotly")
  expect_s3_class(plot_glm_predictor(m_sev, "LEEFTIJD", n_bins = 20),
                  "plotly")
})

test_that("plot_glm_predictor: y_range is unused by default and fixable", {
  # Default: the axis auto-scales (plotly may still fill in a computed
  # range on build, so assert on autorange, which is the actual switch).
  p_auto   <- plot_glm_predictor(m_freq, "LEEFTIJD", n_bins = 30)
  yax_auto <- plotly::plotly_build(p_auto)$x$layout$yaxis
  expect_true(isTRUE(yax_auto$autorange))

  # Fixed: the requested range is honoured and auto-scaling is off
  p_fixed   <- plot_glm_predictor(m_freq, "LEEFTIJD", n_bins = 30,
                                  y_range = c(0, 0.5))
  yax_fixed <- plotly::plotly_build(p_fixed)$x$layout$yaxis
  expect_equal(as.numeric(unlist(yax_fixed$range)), c(0, 0.5))
  expect_false(isTRUE(yax_fixed$autorange))

  # Two predictors with an identical y_range share the same axis
  p_a <- plot_glm_predictor(m_freq, "LEEFTIJD", n_bins = 30, y_range = c(0, 0.4))
  p_b <- plot_glm_predictor(m_freq, "REGIO", y_range = c(0, 0.4))
  expect_equal(
    as.numeric(unlist(plotly::plotly_build(p_a)$x$layout$yaxis$range)),
    as.numeric(unlist(plotly::plotly_build(p_b)$x$layout$yaxis$range)))

  expect_error(plot_glm_predictor(m_freq, "LEEFTIJD", y_range = c(1, 1)),
               "y_range")
})

test_that("make_rating_plot renders main effects and interactions", {
  expect_s3_class(make_rating_plot(tbl, "REGIO"), "plotly")
  expect_s3_class(make_rating_plot(tbl2, "LEEFTIJD:REGIO"), "plotly")
})

test_that("make_plot aggregates raw data internally", {
  p_raw <- make_plot(dat, "REGIO", "Frequency", ta_blue, "Frequency",
                     "color", TRUE)
  expect_s3_class(p_raw, "plotly")
  expect_s3_class(make_plot(dat, "REGIO", "Frequency", ta_blue, "Frequency",
                            "facet", TRUE), "plotly")
  expect_s3_class(make_plot(dat, "REGIO", "Severity", ta_gold, "Severity",
                            "color", FALSE), "plotly")

  # plotted values must equal the agg_all aggregation
  ovz <- agg_all(dat, "REGIO", by_year = TRUE)
  s <- ovz[ovz$Year == "2020", ]
  s <- s[order(s$REGIO), ]
  expect_equal(trace_y(p_raw, "2020"), s$Frequency)

  # invalid metric gives a clear match.arg error
  expect_error(make_plot(dat, "REGIO", "Frequentie", ta_blue, "y",
                         "color", FALSE),
               "Frequency")
})
