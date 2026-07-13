test_that("make_pdp computes the exposure-weighted mean at exposure 1", {
  p_pdp <- make_pdp(m_freq, dat, "REGIO", metric = "Frequency")
  expect_s3_class(p_pdp, "plotly")

  # manual reference: exposure-weighted mean response prediction at Exposure = 1
  nd <- dat; nd$Exposure <- 1
  nd$REGIO <- factor("Zuid", levels(dat$REGIO))
  ref_zuid <- weighted.mean(predict(m_freq, newdata = nd, type = "response"),
                            exp(m_freq$offset))
  pd_dat <- plotly::plotly_build(p_pdp)$x$data
  pdp_trace <- Filter(function(t) identical(t$name, "PDP (model)"), pd_dat)[[1]]
  pdp_zuid <- as.numeric(unlist(pdp_trace$y))[match("Zuid",
                                                    unlist(pdp_trace$x))]
  expect_true(abs(pdp_zuid - ref_zuid) < 1e-8)

  # PDP level must be of the same order as the portfolio frequency
  obs_tot <- sum(dat$AantalClaims) / sum(dat$Exposure)
  expect_true(all(abs(as.numeric(unlist(pdp_trace$y)) / obs_tot - 1) < 1))
})

test_that("make_pdp works for severity and deprecates 'transform'", {
  expect_s3_class(make_pdp(m_sev, dat, "LEEFTIJD", metric = "Severity"),
                  "plotly")
  expect_warning(make_pdp(m_freq, dat, "REGIO", transform = exp),
                 "deprecated")
})
