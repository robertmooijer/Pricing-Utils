test_that("glm_diagnostics summarises both models", {
  diag <- glm_diagnostics(m_freq, m_sev)
  expect_equal(nrow(diag), 2)
  expect_true(all(c("Dispersion", "AIC", "DevianceExplained") %in%
                    names(diag)))
  # well-specified Poisson: dispersion near 1
  expect_true(abs(diag$Dispersion[diag$Model == "frequency"] - 1) < 0.1)
})

test_that("overdispersion raises a warning", {
  # negative binomial data fitted as Poisson: dispersion ~ 1 + mu/size >> 1.2
  d_od <- dat
  d_od$AC_od <- rnbinom(nrow(d_od), mu = d_od$Exposure * exp(lin),
                        size = 0.05)
  m_od <- glm(AC_od ~ REGIO + offset(log(Exposure)), family = poisson(),
              data = d_od)
  expect_warning(glm_diagnostics(m_od), "overdispersion")
})

test_that("binned residual plots render and stay within the band", {
  p_res <- plot_glm_residuals(m_freq)
  expect_s3_class(p_res, "plotly")
  expect_s3_class(plot_glm_residuals(m_freq, "LEEFTIJD"), "plotly")
  expect_s3_class(plot_glm_residuals(m_freq, "REGIO"), "plotly")

  # for a correct model most binned means must lie inside the band
  bt <- plotly::plotly_build(p_res)$x$data
  mr <- Filter(function(t) identical(t$name, "Mean residual"), bt)[[1]]
  bd <- Filter(function(t) identical(t$name, "±2·SE"), bt)[[1]]
  frac_in <- mean(abs(as.numeric(unlist(mr$y))) <=
                    as.numeric(unlist(bd$y)))
  expect_gt(frac_in, 0.7)
})
