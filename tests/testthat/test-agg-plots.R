test_that("agg_all works with custom column names and validates input", {
  dat2 <- dat
  names(dat2)[names(dat2) == "Exposure"] <- "EXPO"
  a <- agg_all(dat2, "REGIO", by_year = TRUE, exposure_col = "EXPO")
  expect_true(all(c("Exposure", "ClaimCount", "Loss",
                    "Frequency", "Severity", "Year") %in% names(a)))
  expect_error(agg_all(dat, "REGIO", FALSE, exposure_col = "DOES_NOT_EXIST"),
               "DOES_NOT_EXIST")
})

test_that("the volume bars follow the metric's own denominator", {
  # a severity is a mean per claim, so exposure bars would say how much
  # business is in a level, not how many claims the mean rests on - and
  # those routinely run in opposite directions
  bar <- function(p) {
    b <- plotly::plotly_build(p)
    list(name = b$x$data[[1]]$name, y = as.vector(b$x$data[[1]]$y))
  }
  pf <- make_plot(dat, "REGIO", "Frequency", ta_blue, "F",
                  display = "color", by_year = FALSE)
  ps <- make_plot(dat, "REGIO", "Severity", ta_blue, "S",
                  display = "color", by_year = FALSE)

  expect_identical(bar(pf)$name, "Exposure")
  expect_identical(bar(ps)$name, "Number of claims")
  expect_equal(sort(bar(pf)$y), sort(as.vector(tapply(dat$Exposure,
                                                      dat$REGIO, sum))),
               tolerance = 1e-8)
  expect_equal(sort(bar(ps)$y), sort(as.vector(tapply(dat$AantalClaims,
                                                      dat$REGIO, sum))),
               tolerance = 1e-8)
  # the secondary axis is labelled accordingly
  expect_identical(plotly::plotly_build(ps)$x$layout$yaxis2$title,
                   "Number of claims")

  # facet mode uses the same column
  pff <- make_plot(dat, "REGIO", "Severity", ta_blue, "S",
                   display = "facet", by_year = TRUE)
  expect_s3_class(pff, "plotly")
})

test_that("grouping on a column agg_all creates is refused", {
  # grouping on "Exposure" produced a data.frame with two columns of that
  # name rather than an error, and every later lookup then picked one of
  # them at random
  d <- data.frame(Exposure = c(1, 1, 2), AantalClaims = c(0, 1, 2),
                  SCHADELAST = c(0, 500, 900))
  expect_error(agg_all(d, "Exposure", FALSE), "cannot group on")
  expect_error(agg_all(dat, "BOEKJAAR", by_year = TRUE), "by_year = FALSE")
})

test_that("a multi-column model term is refused with a usable message", {
  # model.frame() holds ns(LEEFTIJD, 4) as a matrix; without a check this
  # failed several frames down with "replacement has 0 rows"
  m <- glm(AantalClaims ~ ns(LEEFTIJD, 4) + offset(log(Exposure)),
           family = poisson(), data = dat)
  expect_error(plot_glm_predictor(m, "ns(LEEFTIJD, 4)"),
               "multi-column model term")
  expect_error(plot_glm_predictor(m, "ns(LEEFTIJD, 4)"), "LEEFTIJD")
  # the underlying column still works
  expect_s3_class(plot_glm_predictor(m, "LEEFTIJD"), "plotly")
})

test_that("plot_glm_predictor returns plotly objects", {
  expect_s3_class(plot_glm_predictor(m_freq, "LEEFTIJD", n_bins = 30),
                  "plotly")
  expect_s3_class(plot_glm_predictor(m_sev, "LEEFTIJD", n_bins = 20),
                  "plotly")
})

test_that("fewer distinct values than n_bins means no binning at all", {
  obs_x <- function(p) {
    tr <- Filter(function(t) identical(t$name, "Observed"),
                 plotly::plotly_build(p)$x$data)[[1]]
    sort(as.numeric(unlist(tr$x)))
  }
  uniq <- sort(unique(dat$LEEFTIJD))          # 63 integer values, 18..80

  # n_bins well above the number of distinct values: exact positions,
  # including the minimum (the case that used to drift off its own value)
  p_exact <- plot_glm_predictor(m_freq, "LEEFTIJD", n_bins = 150)
  expect_equal(obs_x(p_exact), uniq)
  expect_equal(min(obs_x(p_exact)), min(dat$LEEFTIJD))

  # exactly at the boundary: still unbinned
  expect_equal(obs_x(plot_glm_predictor(m_freq, "LEEFTIJD",
                                        n_bins = length(uniq))), uniq)

  # An extreme outlier must not shift the other points; with equal-width
  # bins it would drag every boundary along
  d_out <- dat
  d_out$LEEFTIJD[1] <- 999
  m_out <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
               family = poisson(), data = d_out)
  x_out <- obs_x(plot_glm_predictor(m_out, "LEEFTIJD", n_bins = 150))
  expect_true(all(uniq %in% x_out))
  expect_true(999 %in% x_out)

  # More distinct values than n_bins: binning kicks in, at most n_bins points
  p_binned <- plot_glm_predictor(m_freq, "LEEFTIJD", n_bins = 10)
  expect_lte(length(obs_x(p_binned)), 10)
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
