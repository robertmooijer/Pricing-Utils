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

test_that("the thin-cell column is correct", {
  cc <- tbl$ClaimCount[!is.na(tbl$ClaimCount)]
  expect_identical(tbl$IsThin[!is.na(tbl$ClaimCount)], cc < 30)

  expect_warning(
    tbl_thin <- make_rating_table(m_freq, NULL, data = dat,
                                  min_claims = 1e6),
    "very little experience")
  expect_true(all(tbl_thin$IsThin[!is.na(tbl_thin$IsThin)]))
  expect_s3_class(make_rating_plot(tbl_thin, "REGIO"), "plotly")
})

test_that("continuous grids run in readable steps", {
  set.seed(8)
  ng <- 20000
  dg <- data.frame(
    LEEFTIJD = round(runif(ng, 18, 80)),
    GEWICHT  = round(pmin(pmax(rnorm(ng, 1350, 250), 800), 2400), -1),
    Exposure = round(runif(ng, .3, 1), 3))
  dg$AantalClaims <- rpois(ng, dg$Exposure * exp(-1.7 + .02 *
                                                   pmax(0, 30 - dg$LEEFTIJD)))
  mg <- glm(AantalClaims ~ ns(LEEFTIJD, 4) + ns(GEWICHT, 3) +
              offset(log(Exposure)), family = poisson(), data = dg)
  tg <- make_rating_table(mg, NULL, data = dg)
  lv <- function(t, v) t$LevelNum[t$Variable == v & is.na(t$Group)]

  # a whole-number column with few enough values is listed value by value
  age <- lv(tg, "LEEFTIJD")
  expect_equal(unique(diff(age)), 1)
  expect_true(all(age == round(age)))

  # a wide column gets a rounded step instead
  wt <- lv(tg, "GEWICHT")
  expect_equal(length(unique(diff(wt))), 1)
  step <- unique(diff(wt))
  expect_true(all(abs(wt / step - round(wt / step)) < 1e-9))  # round multiples

  # the base sits on the grid, so there is a row with factor exactly 1
  for (v in c("LEEFTIJD", "GEWICHT")) {
    s <- tg[tg$Variable == v & is.na(tg$Group), ]
    expect_equal(sum(s$IsBase), 1)
    expect_equal(s$Factor_Frequency[s$IsBase], 1)
  }
})

test_that("grid_step can be set globally or per variable", {
  set.seed(8)
  ng <- 15000
  dg <- data.frame(
    LEEFTIJD = round(runif(ng, 18, 80)),
    GEWICHT  = round(pmin(pmax(rnorm(ng, 1350, 250), 800), 2400), -1),
    Exposure = round(runif(ng, .3, 1), 3))
  dg$AantalClaims <- rpois(ng, dg$Exposure * exp(-1.6))
  mg <- glm(AantalClaims ~ LEEFTIJD + GEWICHT + offset(log(Exposure)),
            family = poisson(), data = dg)

  t1 <- make_rating_table(mg, NULL, data = dg,
                          grid_step = c(LEEFTIJD = 5, GEWICHT = 200))
  expect_equal(unique(diff(t1$LevelNum[t1$Variable == "LEEFTIJD"])), 5)
  expect_equal(unique(diff(t1$LevelNum[t1$Variable == "GEWICHT"])), 200)

  t2 <- make_rating_table(mg, NULL, data = dg, grid_step = 10)
  expect_equal(unique(diff(t2$LevelNum[t2$Variable == "GEWICHT"])), 10)
})

test_that("the rating table carries no credibility columns", {
  expect_false(any(grepl("^Credibility", names(tbl))))
  expect_null(attr(tbl, "credibility"))
})
