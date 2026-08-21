# A model fitted on a data.table (or a tibble) must work everywhere. On a
# data.table `d[, cols, drop = FALSE]` is an expression, not a column
# selection, which used to abort screen_features(), make_pdp() and
# premium_impact() with "j (the 2nd argument inside [...]) is a single
# symbol but column name 'num_f' is not found".

set.seed(23)
ndt <- 20000
ddt <- data.table::data.table(
  LEEFTIJD = round(runif(ndt, 18, 80)),
  REGIO    = factor(sample(c("Stad", "Dorp"), ndt, TRUE)),
  KM       = round(pmax(rlnorm(ndt, log(12000), .4), 1000), -2),
  RUIS     = round(rnorm(ndt), 2),
  Exposure = round(runif(ndt, .3, 1), 3))
ddt[, AantalClaims := rpois(.N, Exposure * exp(
  -1.6 + 0.02 * pmax(0, 30 - LEEFTIJD) + 0.3 * (REGIO == "Stad") +
    0.4 * (log(KM) - log(12000))))]
ddt[, SCHADELAST := AantalClaims * 1500]

m_dt <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            family = poisson(), data = ddt)

test_that("model$data as a data.table is handled", {
  expect_true(data.table::is.data.table(m_dt$data))

  # the exact call that failed: the correlation check indexes columns
  r <- suppressWarnings(
    screen_features(m_dt, seed = 5, nrounds = 100,
                    early_stopping_rounds = 15, n_shap = 0))
  expect_s3_class(r$features, "data.frame")
  expect_true("KM" %in% r$features$Feature)
})

test_that("make_pdp and premium_impact accept data.table input", {
  # make_pdp collapses to unique profiles by indexing key columns
  expect_s3_class(make_pdp(m_dt, ddt, "REGIO", metric = "Frequency"), "plotly")

  # premium_impact indexes the `by` columns
  imp <- premium_impact(ddt, model_freq_new = m_dt, model_freq_old = m_dt,
                        by = "REGIO")
  expect_lt(max(abs(imp$policy$ChangePct)), 1e-8)
  expect_equal(nrow(imp$by_level), 2)
})

test_that("the cell-based tools accept data.table input", {
  expect_s3_class(plot_residual_heatmap(m_dt, "LEEFTIJD", "REGIO", n_bins = 6),
                  "plotly")
  r <- detect_interactions(m_dt, n_bins = 6, n_sim = 50, seed = 1)
  expect_s3_class(r, "data.frame")
})

test_that("make_rating_table accepts data.table input", {
  tb <- make_rating_table(m_dt, NULL, data = ddt)
  expect_true(all(c("LEEFTIJD", "REGIO") %in% tb$Variable))
  expect_true(any(tb$IsBase))
})
