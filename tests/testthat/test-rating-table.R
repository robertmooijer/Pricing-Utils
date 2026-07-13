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
  expect_equal(tbl$Credibility[!is.na(tbl$ClaimCount)],
               pmin(1, sqrt(cc / 1082)))
  expect_identical(tbl$IsThin[!is.na(tbl$ClaimCount)], cc < 30)

  expect_warning(
    tbl_thin <- make_rating_table(m_freq, NULL, data = dat,
                                  min_claims = 1e6),
    "little standalone")
  expect_true(all(tbl_thin$IsThin[!is.na(tbl_thin$IsThin)]))
  expect_s3_class(make_rating_plot(tbl_thin, "REGIO"), "plotly")
})
