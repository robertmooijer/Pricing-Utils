test_that("rating table builds, incl. inline factor(BOEKJAAR)", {
  expect_true(all(c("LEEFTIJD", "REGIO", "BOEKJAAR") %in% tbl$Variable))
  expect_true(all(tbl$Type[tbl$Variable == "BOEKJAAR"] == "categorical"))
})

test_that("every variable has exactly one base row with factor 1", {
  b <- tbl[tbl$IsBase & is.na(tbl$Group), ]
  expect_equal(nrow(b), length(unique(tbl$Variable[is.na(tbl$Group)])))
  expect_true(all(abs(c(b$Factor_Frequency, b$Factor_Severity) - 1) < 1e-10,
                  na.rm = TRUE))
})

test_that("intercept x factors reconstructs the model prediction exactly", {
  i_f   <- attr(tbl, "intercept_frequency")
  s_age <- tbl[tbl$Variable == "LEEFTIJD" & is.na(tbl$Group), ]
  age   <- s_age$LevelNum[35]           # arbitrary grid point
  test_row <- data.frame(LEEFTIJD = age,
                         REGIO = factor("Zuid", levels(dat$REGIO)),
                         BOEKJAAR = 2022, Exposure = 1)
  pred_direct <- predict(m_freq, newdata = test_row, type = "response")
  fac <- function(v, lvl) {
    s <- tbl[tbl$Variable == v & is.na(tbl$Group), ]
    s$Factor_Frequency[match(as.character(lvl), s$Level)]
  }
  recon <- i_f * s_age$Factor_Frequency[35] * fac("REGIO", "Zuid") *
           fac("BOEKJAAR", "2022")
  expect_true(abs(pred_direct - recon) < 1e-10)
})

test_that("intercept is per unit of exposure", {
  i_f <- attr(tbl, "intercept_frequency")
  base_pred <- predict(m_freq, newdata = data.frame(
    LEEFTIJD = median(dat$LEEFTIJD),
    REGIO = factor("Noord", levels(dat$REGIO)),
    BOEKJAAR = 2019, Exposure = 1), type = "response")
  expect_true(abs(i_f - base_pred) < 1e-10)
})

test_that("interaction rows have a unit base cell and unit uplift at reference", {
  d_int <- tbl2[tbl2$Variable == "LEEFTIJD:REGIO", ]
  expect_gt(nrow(d_int), 0)
  expect_true(abs(d_int$Factor_Frequency[d_int$IsBase] - 1) < 1e-10)
  u_ref <- d_int$Uplift_Frequency[d_int$Group == "Noord"]
  expect_true(all(abs(u_ref - 1) < 1e-8))
})

test_that("non-log link raises a warning", {
  m_inv <- glm(AvgLoss ~ REGIO, family = Gamma(link = "inverse"),
               data = d_sev, weights = AantalClaims)
  expect_warning(make_rating_table(NULL, m_inv, data = dat), "log link")
})

test_that("base_level = 'exposure' picks the largest level as reference", {
  tbl3 <- make_rating_table(m_freq, NULL, data = dat, base_level = "exposure")
  largest <- names(which.max(tapply(dat$Exposure, dat$REGIO, sum)))
  b3 <- tbl3[tbl3$Variable == "REGIO" & tbl3$IsBase, "Level"]
  expect_identical(b3, largest)
})

test_that("credibility and thin-cell columns are correct", {
  cc <- tbl$ClaimCount[!is.na(tbl$ClaimCount)]
  ok <- !is.na(tbl$ClaimCount)

  # Frequency: the plain Poisson standard
  expect_equal(tbl$Credibility_Frequency[ok], pmin(1, sqrt(cc / 1082)))

  # Severity/premium: derived from the claim-size CV, estimated from the
  # Gamma severity model's Pearson dispersion (CV^2 = phi)
  phi <- sum(residuals(m_sev, type = "pearson")^2) / m_sev$df.residual
  expect_equal(tbl$Credibility_Severity[ok], pmin(1, sqrt(cc / (1082 * phi))))
  expect_equal(tbl$Credibility_Premium[ok],
               pmin(1, sqrt(cc / (1082 * (1 + phi)))))

  # Severity always needs at least as many claims as frequency when CV > 1
  att <- attr(tbl, "credibility")
  expect_equal(att$cv, sqrt(phi))
  expect_identical(att$cv_source, "estimated from model_sev")
  expect_gt(att$full_cred_premium, att$full_cred_frequency)

  expect_identical(tbl$IsThin[!is.na(tbl$ClaimCount)], cc < 30)

  expect_warning(
    tbl_thin <- make_rating_table(m_freq, NULL, data = dat,
                                  min_claims = 1e6),
    "little standalone")
  expect_true(all(tbl_thin$IsThin[!is.na(tbl_thin$IsThin)]))
  expect_s3_class(make_rating_plot(tbl_thin, "REGIO"), "plotly")
})

test_that("severity credibility needs a CV and can be supplied explicitly", {
  # No severity model and no cv_severity: frequency only
  t_freq <- make_rating_table(m_freq, NULL, data = dat)
  expect_false(any(is.na(t_freq$Credibility_Frequency[
    !is.na(t_freq$ClaimCount)])))
  expect_true(all(is.na(t_freq$Credibility_Severity)))
  expect_true(all(is.na(t_freq$Credibility_Premium)))
  expect_identical(attr(t_freq, "credibility")$cv_source, "unavailable")

  # Explicit CV is honoured
  t_cv <- make_rating_table(m_freq, NULL, data = dat, cv_severity = 2)
  cc <- t_cv$ClaimCount[!is.na(t_cv$ClaimCount)]
  expect_equal(t_cv$Credibility_Severity[!is.na(t_cv$ClaimCount)],
               pmin(1, sqrt(cc / (1082 * 4))))
  expect_equal(attr(t_cv, "credibility")$cv, 2)
  expect_identical(attr(t_cv, "credibility")$cv_source, "user")

  # A non-Gamma severity model warns instead of guessing
  m_gauss <- glm(AvgLoss ~ REGIO, data = d_sev, weights = AantalClaims)
  expect_warning(make_rating_table(m_freq, m_gauss, data = dat),
                 "claim-size CV")

  expect_error(make_rating_table(m_freq, NULL, data = dat, cv_severity = -1),
               "cv_severity")
})
