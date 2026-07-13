test_that("identical models produce zero dislocation", {
  imp0 <- premium_impact(dat, model_freq_new = m_freq, model_sev_new = m_sev,
                         model_freq_old = m_freq, model_sev_old = m_sev,
                         by = "REGIO")
  expect_lt(max(abs(imp0$policy$ChangePct)), 1e-8)
  expect_equal(nrow(imp0$by_level), 3)
  expect_s3_class(imp0$plot, "plotly")
})

test_that("rebase separates rate-level change from dislocation", {
  # old premium column = 2x the new rate -> rate-level change -50%,
  # rebased dislocation 0
  d2 <- dat
  new_rate <- predict(m_freq, newdata = transform(d2, Exposure = 1),
                      type = "response") *
              predict(m_sev, newdata = d2, type = "response")
  d2$OldPrem <- 2 * new_rate * d2$Exposure
  imp2 <- premium_impact(d2, model_freq_new = m_freq, model_sev_new = m_sev,
                         old_premium_col = "OldPrem")
  expect_true(abs(imp2$stats$rate_change + 0.5) < 1e-10)
  expect_lt(max(abs(imp2$policy$ChangePct)), 1e-8)
  expect_equal(nrow(imp2$largest_increases), 10)
  expect_equal(nrow(imp2$largest_decreases), 10)
})
