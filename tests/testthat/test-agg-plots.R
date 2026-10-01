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

test_that("observed points carry error bars that match the theory", {
  set.seed(61)
  nn <- 30000
  dc <- data.frame(R = factor(sample(c("N", "O", "Z"), nn, TRUE)),
                   LFT = round(runif(nn, 18, 80)),
                   Exposure = round(runif(nn, .3, 1), 3))
  dc$AantalClaims <- rpois(nn, dc$Exposure * exp(-2.2 + .3 * (dc$R == "Z")))
  mc <- glm(AantalClaims ~ R + offset(log(Exposure)), poisson(), dc)
  tr <- function(p, nm) {
    b <- plotly::plotly_build(p)
    for (t in b$x$data) if (identical(t$name, nm)) return(t)
    NULL
  }

  o <- tr(plot_glm_predictor(mc, "R"), "Observed")
  # counts: se = sqrt(phi * sum(V(mu))) / sum(exposure), phi fixed at 1
  hand <- tapply(seq_len(nn), dc$R, function(i)
    sqrt(sum(fitted(mc)[i])) / sum(dc$Exposure[i]))
  expect_equal(as.vector(o$error_y$array) / stats::qnorm(0.975),
               as.vector(hand[o$x]), tolerance = 1e-10)

  # a weighted Gamma uses the weighted-mean form and an estimated phi
  dsv <- dc[dc$AantalClaims > 0, ]
  dsv$Avg <- rgamma(nrow(dsv), 3, scale = 700)
  msv <- glm(Avg ~ R, Gamma("log"), dsv, weights = AantalClaims)
  os <- suppressWarnings(tr(plot_glm_predictor(msv, "R"), "Observed"))
  phi <- sum(residuals(msv, type = "pearson")^2) / msv$df.residual
  h2 <- tapply(seq_len(nrow(dsv)), dsv$R, function(i)
    sqrt(phi * sum(dsv$AantalClaims[i] * fitted(msv)[i]^2)) /
      sum(dsv$AantalClaims[i]))
  expect_equal(as.vector(os$error_y$array) / stats::qnorm(0.975),
               as.vector(h2[os$x]), tolerance = 1e-8)
})

test_that("the ci switches behave", {
  set.seed(62)
  nn <- 8000
  dc <- data.frame(R = factor(sample(c("N", "Z"), nn, TRUE)),
                   Exposure = round(runif(nn, .3, 1), 3))
  dc$AantalClaims <- rpois(nn, dc$Exposure * .12)
  mc <- glm(AantalClaims ~ R + offset(log(Exposure)), poisson(), dc)
  tr <- function(p) {
    b <- plotly::plotly_build(p)
    for (t in b$x$data) if (identical(t$name, "Observed")) return(t)
    NULL
  }
  # plotly leaves a stub error_y of its own, so the array is what counts
  expect_null(tr(plot_glm_predictor(mc, "R", ci = FALSE))$error_y$array)
  expect_false(is.null(tr(plot_glm_predictor(mc, "R"))$error_y$array))
  expect_true(all(tr(plot_glm_predictor(mc, "R", ci_level = 0.99))$error_y$array >
                  tr(plot_glm_predictor(mc, "R"))$error_y$array))
  expect_error(plot_glm_predictor(mc, "R", ci_level = 1.5), "between 0 and 1")
  expect_error(plot_glm_predictor(mc, "R", ci_level = 0), "between 0 and 1")
})

test_that("binned residuals are drawn as points, not a line", {
  set.seed(63)
  nn <- 6000
  dc <- data.frame(x = runif(nn), Exposure = round(runif(nn, .3, 1), 3))
  dc$AantalClaims <- rpois(nn, dc$Exposure * exp(-2 + dc$x))
  mc <- glm(AantalClaims ~ x + offset(log(Exposure)), poisson(), dc)
  b <- plotly::plotly_build(plot_glm_residuals(mc))
  mr <- NULL
  for (t in b$x$data) if (identical(t$name, "Mean residual")) mr <- t
  # plotly leaves a default line stub on every scatter trace, so the mode
  # is the switch that decides whether a line is actually drawn
  expect_identical(mr$mode, "markers")
  expect_false(grepl("lines", mr$mode, fixed = TRUE))
})

test_that("plot_glm_predictor evaluates another period when 'data' is given", {
  set.seed(63)
  nn <- 20000
  mk <- function(seed, bump) {
    set.seed(seed)
    d <- data.frame(R = factor(sample(c("N", "O", "Z"), nn, TRUE)),
                    LFT = round(runif(nn, 18, 80)),
                    Exposure = round(runif(nn, .3, 1), 3))
    d$AantalClaims <- rpois(nn, d$Exposure *
                              exp(-2.2 + .3 * (d$R == "Z") + bump * (d$R == "O")))
    d
  }
  d19 <- mk(63, 0)
  d20 <- mk(64, 0.5)            # "O" deteriorates in the new year
  mc  <- glm(AantalClaims ~ R + offset(log(Exposure)), poisson(), d19)
  tr <- function(p, nm) {
    b <- plotly::plotly_build(p)
    for (t in b$x$data) if (identical(t$name, nm)) return(t)
    NULL
  }

  p_new <- plot_glm_predictor(mc, "R", data = d20)
  expect_s3_class(p_new, "plotly")
  o <- tr(p_new, "Observed")
  e <- tr(p_new, "Predicted")
  # observed = that year's own rate; predicted = the tariff applied to it
  hand_o <- tapply(seq_len(nn), d20$R, function(i)
    sum(d20$AantalClaims[i]) / sum(d20$Exposure[i]))
  hand_e <- tapply(seq_len(nn), d20$R, function(i)
    sum(predict(mc, d20[i, ], type = "response")) / sum(d20$Exposure[i]))
  expect_equal(as.vector(unlist(o$y)), as.vector(hand_o[o$x]), tolerance = 1e-10)
  expect_equal(as.vector(unlist(e$y)), as.vector(hand_e[e$x]), tolerance = 1e-10)

  # in-sample the two series coincide on a level in the model; out of
  # sample they do not, which is the whole point of monitoring
  o0 <- tr(plot_glm_predictor(mc, "R"), "Observed")
  e0 <- tr(plot_glm_predictor(mc, "R"), "Predicted")
  expect_equal(as.vector(unlist(o0$y)), as.vector(unlist(e0$y)), tolerance = 1e-8)
  i_o <- which(o$x == "O")
  expect_gt(unlist(o$y)[i_o] / unlist(e$y)[i_o], 1.2)

  # the bars show the new period's exposure, not the old one's
  w <- tr(p_new, "Exposure")
  expect_equal(as.vector(unlist(w$y)),
               as.vector(tapply(d20$Exposure, d20$R, sum)[w$x]),
               tolerance = 1e-8)

  # a predictor outside the model is read from 'data' as well
  expect_s3_class(plot_glm_predictor(mc, "LFT", data = d20, n_bins = 20),
                  "plotly")
})

test_that("plot_glm_predictor on new data re-evaluates weights and offsets", {
  set.seed(65)
  nn <- 6000
  d <- data.frame(R = factor(sample(c("N", "Z"), nn, TRUE)),
                  Maanden = sample(1:12, nn, TRUE))
  d$AantalClaims <- rpois(nn, d$Maanden / 12 * .3) + 1L
  d$Avg <- rgamma(nn, 3, scale = 700) * ifelse(d$R == "Z", 1.2, 1)
  m_sev <- glm(Avg ~ R, Gamma("log"), d, weights = AantalClaims)
  d2 <- d[sample(nn, 3000), ]
  d2$Avg <- d2$Avg * 1.15                     # a severity trend of 15%

  tr <- function(p, nm) {
    b <- plotly::plotly_build(p)
    for (t in b$x$data) if (identical(t$name, nm)) return(t)
    NULL
  }
  o <- tr(plot_glm_predictor(m_sev, "R", data = d2), "Observed")
  hand <- tapply(seq_len(nrow(d2)), d2$R, function(i)
    weighted.mean(d2$Avg[i], d2$AantalClaims[i]))
  expect_equal(as.vector(unlist(o$y)), as.vector(hand[o$x]), tolerance = 1e-10)
  # the weight bars are the new period's claim counts
  w <- tr(plot_glm_predictor(m_sev, "R", data = d2), "Weight")
  expect_equal(as.vector(unlist(w$y)),
               as.vector(tapply(d2$AantalClaims, d2$R, sum)[w$x]),
               tolerance = 1e-8)

  # an offset passed as an argument rather than in the formula also follows
  m_f <- glm(AantalClaims ~ R, poisson(), d, offset = log(Maanden / 12))
  o2 <- tr(plot_glm_predictor(m_f, "R", data = d2, exposure_col = "Maanden"),
           "Observed")
  expect_equal(as.vector(unlist(o2$y)),
               as.vector(tapply(seq_len(nrow(d2)), d2$R, function(i)
                 sum(d2$AantalClaims[i]) / sum(d2$Maanden[i]))[o2$x]),
               tolerance = 1e-10)
})

test_that("unusable monitoring data is refused rather than guessed at", {
  set.seed(66)
  nn <- 4000
  d <- data.frame(R = factor(sample(c("N", "Z"), nn, TRUE)),
                  Exposure = runif(nn, .3, 1))
  d$AantalClaims <- rpois(nn, d$Exposure * .2)
  mc <- glm(AantalClaims ~ R + offset(log(Exposure)), poisson(), d)

  expect_error(plot_glm_predictor(mc, "R", data = d[, c("R", "Exposure")]),
               "AantalClaims")
  expect_error(plot_glm_predictor(mc, "R", data = d[0, ]), "at least one row")
  d_new <- d
  levels(d_new$R) <- c("N", "Z")
  d_new$R <- as.character(d_new$R)
  d_new$R[1] <- "O"                           # a level the model never saw
  expect_error(plot_glm_predictor(mc, "R", data = d_new), "new level")

  # rows with a missing value are dropped, exactly as glm() dropped them
  d_na <- d
  d_na$R[1:50] <- NA
  tr <- function(p, nm) {
    b <- plotly::plotly_build(p)
    for (t in b$x$data) if (identical(t$name, nm)) return(t)
    NULL
  }
  w <- tr(plot_glm_predictor(mc, "R", data = d_na), "Exposure")
  expect_equal(sum(unlist(w$y)), sum(d$Exposure[-(1:50)]), tolerance = 1e-8)
})
