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
  # the accounting year sits at its latest level by default (RT-15)
  base_pred <- predict(m_freq, newdata = data.frame(
    LEEFTIJD = median(dat$LEEFTIJD),
    REGIO = factor("Noord", levels(dat$REGIO)),
    BOEKJAAR = 2023, Exposure = 1), type = "response")
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

  # a named list works the same as a named vector
  t3 <- make_rating_table(mg, NULL, data = dg,
                          grid_step = list(LEEFTIJD = 5, GEWICHT = 200))
  expect_equal(t3$LevelNum, t1$LevelNum)

  # naming only one variable leaves the rest on the automatic step
  t4 <- make_rating_table(mg, NULL, data = dg, grid_step = list(LEEFTIJD = 5))
  auto <- make_rating_table(mg, NULL, data = dg)
  expect_equal(unique(diff(t4$LevelNum[t4$Variable == "LEEFTIJD"])), 5)
  expect_equal(t4$LevelNum[t4$Variable == "GEWICHT"],
               auto$LevelNum[auto$Variable == "GEWICHT"])

  # a misspelled name is called out rather than ignored
  expect_warning(make_rating_table(mg, NULL, data = dg,
                                   grid_step = c(LEEFTJID = 5)),
                 "no continuous variable")
})

test_that("a wide integer column with few values is listed, not stepped", {
  # A coded column such as a sum insured holds a handful of whole numbers
  # spread over a million. Treating "few distinct values" as "step by 1"
  # built a grid over the whole range - a million rows - instead of the
  # five values that actually occur.
  set.seed(21)
  ng <- 3000
  dg <- data.frame(
    VERZBEDRAG = sample(c(0L, 100000L, 250000L, 500000L, 1000000L), ng, TRUE),
    LEEFTIJD   = round(runif(ng, 18, 80)),
    Exposure   = runif(ng, .3, 1))
  dg$AantalClaims <- rpois(ng, dg$Exposure * .1)
  mg <- glm(AantalClaims ~ VERZBEDRAG + LEEFTIJD + offset(log(Exposure)),
            family = poisson(), data = dg)
  tg <- make_rating_table(mg, NULL, data = dg)
  lv <- function(v) tg$LevelNum[tg$Variable == v & is.na(tg$Group)]

  expect_equal(lv("VERZBEDRAG"), sort(unique(dg$VERZBEDRAG)))
  expect_lt(nrow(tg), 200)
  # an age still runs year by year
  expect_equal(unique(diff(lv("LEEFTIJD"))), 1)
  # and there is still exactly one base row with a factor of 1
  b <- tg[tg$Variable == "VERZBEDRAG" & tg$IsBase, ]
  expect_equal(nrow(b), 1)
  expect_equal(b$Factor_Frequency, 1)
  expect_true(b$LevelNum %in% dg$VERZBEDRAG)
})

test_that("round large levels are not written in scientific notation", {
  # as.character(1e5) is "1e+05", which is unusable both as a tariff level
  # and as the lookup key the export builds from it
  set.seed(22)
  ng <- 2000
  dg <- data.frame(BEDRAG = sample(c(100000L, 200000L, 1000000L), ng, TRUE),
                   Exposure = runif(ng, .3, 1))
  dg$AantalClaims <- rpois(ng, dg$Exposure * .1)
  mg <- glm(AantalClaims ~ BEDRAG + offset(log(Exposure)),
            family = poisson(), data = dg)
  tg <- make_rating_table(mg, NULL, data = dg)
  lvl <- tg$Level[tg$Variable == "BEDRAG"]
  expect_false(any(grepl("e", lvl, fixed = TRUE)))
  expect_identical(lvl, c("100000", "200000", "1000000"))
  # Level and LevelNum must still describe the same value
  expect_equal(as.numeric(lvl), tg$LevelNum[tg$Variable == "BEDRAG"])
})

test_that("interaction cells carry exposure and claims for any variable type", {
  # Volume was filled only for categorical x categorical, so a continuous
  # rating factor crossed with a region left Exposure, ClaimCount and
  # IsThin empty - in the very cells where a portfolio runs thin
  set.seed(23)
  ng <- 8000
  dg <- data.frame(LEEFTIJD = round(runif(ng, 18, 70)),
                   REGIO = factor(sample(c("Noord", "Zuid"), ng, TRUE)),
                   Exposure = runif(ng, .4, 1))
  dg$AantalClaims <- rpois(ng, dg$Exposure * .15)
  mg <- glm(AantalClaims ~ LEEFTIJD * REGIO + offset(log(Exposure)),
            family = poisson(), data = dg)
  tg <- suppressWarnings(make_rating_table(mg, NULL, data = dg))
  ig <- tg[!is.na(tg$Group), ]

  expect_gt(nrow(ig), 0)
  expect_false(anyNA(ig$Exposure))
  expect_false(anyNA(ig$ClaimCount))
  expect_false(anyNA(ig$IsThin))
  # every policy is counted exactly once across the cells
  expect_equal(sum(ig$Exposure), sum(dg$Exposure), tolerance = 1e-8)
  expect_equal(sum(ig$ClaimCount), sum(dg$AantalClaims))

  # and a cell matches a hand count on the same grid
  g   <- sort(unique(ig$LevelNum))
  brk <- c(-Inf, g[-1] - diff(g) / 2, Inf)
  nz  <- dg$REGIO == "Noord"
  hand <- tapply(dg$Exposure[nz], findInterval(dg$LEEFTIJD[nz], brk), sum)
  cel <- ig[ig$Group == "Noord", ]
  cel <- cel[order(cel$LevelNum), ]
  expect_equal(cel$Exposure[as.integer(names(hand))], as.vector(hand),
               tolerance = 1e-8)
})

test_that("the rating table carries no credibility columns", {
  expect_false(any(grepl("^Credibility", names(tbl))))
  expect_null(attr(tbl, "credibility"))
})

test_that("the uplift columns are absent without an interaction", {
  # An uplift only exists relative to the two main effects it sits between,
  # so on a tariff with no interaction term at all it would be NA in every
  # row. Better not to ship the column than to ship it empty.
  expect_false(any(grepl("^Uplift", names(tbl))))
  expect_true(all(c("Group", "GroupVar") %in% names(tbl)))   # structure stays

  # with an interaction they are back, and filled on exactly those rows
  expect_true(all(c("Uplift_Frequency", "Uplift_Severity", "Uplift_Premium")
                  %in% names(tbl2)))
  expect_false(anyNA(tbl2$Uplift_Frequency[!is.na(tbl2$Group)]))
  expect_true(all(is.na(tbl2$Uplift_Frequency[is.na(tbl2$Group)])))

  # a variable without an interaction term gets no interaction rows at all
  expect_false(any(grepl("BOEKJAAR", tbl2$Variable[!is.na(tbl2$Group)])))
})

test_that("the export copes with a table that has no uplift columns", {
  skip_if_not_installed("openxlsx")
  f <- file.path(tempdir(), "no_uplift.xlsx")
  expect_silent(suppressWarnings(export_rating_table(tbl, f)))
  sh <- openxlsx::getSheetNames(f)
  expect_true("Tariff" %in% sh)
  d1 <- openxlsx::read.xlsx(f, sheet = "REGIO")
  expect_false(any(grepl("^Uplift", names(d1))))
  expect_s3_class(make_rating_plot(tbl, "REGIO"), "plotly")
})

test_that("the base can be chosen per variable, default unchanged", {
  set.seed(91)
  nb <- 20000
  db <- data.frame(LEEFTIJD = round(runif(nb, 18, 80)),
                   REGIO = factor(sample(c("Noord", "Zuid", "Randstad"), nb, TRUE)),
                   Exposure = round(runif(nb, .2, 1), 3))
  db$AantalClaims <- rpois(nb, db$Exposure *
                             exp(-2 + .01 * db$LEEFTIJD + .3 * (db$REGIO == "Randstad")))
  mb <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            poisson(), db)
  basis <- function(t, v) t[t$Variable == v & is.na(t$Group) & t$IsBase, ]

  t0 <- make_rating_table(mb, NULL, data = db)
  expect_identical(t0, make_rating_table(mb, NULL, data = db, base = NULL))
  expect_identical(basis(t0, "REGIO")$Level, "Noord")          # first level

  t1 <- make_rating_table(mb, NULL, data = db,
                          base = list(REGIO = "Zuid", LEEFTIJD = 40))
  expect_identical(basis(t1, "REGIO")$Level, "Zuid")
  expect_equal(basis(t1, "LEEFTIJD")$LevelNum, 40)
  expect_equal(basis(t1, "REGIO")$Factor_Frequency, 1)
  expect_equal(basis(t1, "LEEFTIJD")$Factor_Frequency, 1)
  # exactly one base row per variable, and no duplicate grid point at 40
  expect_equal(nrow(basis(t1, "LEEFTIJD")), 1)
  expect_equal(sum(t1$Variable == "LEEFTIJD" & t1$LevelNum == 40), 1)

  # a named vector works too, and overrides base_level for that variable
  t2 <- make_rating_table(mb, NULL, data = db, base_level = "exposure",
                          base = c(REGIO = "Zuid"))
  expect_identical(basis(t2, "REGIO")$Level, "Zuid")

  # a base between grid points is placed exactly, not snapped
  t3 <- make_rating_table(mb, NULL, data = db, base = list(LEEFTIJD = 40.5))
  expect_equal(basis(t3, "LEEFTIJD")$LevelNum, 40.5)

  # premiums do not move: same policy, both tables
  prem <- function(t, age, reg) {
    f <- function(v, l) t$Factor_Frequency[t$Variable == v & is.na(t$Group) &
                                           t$Level == l]
    attr(t, "intercept_frequency") * f("LEEFTIJD", age) * f("REGIO", reg)
  }
  expect_equal(prem(t0, "60", "Randstad"), prem(t1, "60", "Randstad"),
               tolerance = 1e-10)
  nd <- data.frame(LEEFTIJD = 60, REGIO = factor("Randstad", levels(db$REGIO)),
                   Exposure = 1)
  expect_equal(prem(t1, "60", "Randstad"),
               as.numeric(predict(mb, nd, type = "response")), tolerance = 1e-10)
})

test_that("a wrong base is refused or reported", {
  set.seed(92)
  nb <- 5000
  db <- data.frame(LEEFTIJD = round(runif(nb, 18, 80)),
                   REGIO = factor(sample(c("Noord", "Zuid"), nb, TRUE)),
                   Exposure = round(runif(nb, .2, 1), 3))
  db$AantalClaims <- rpois(nb, db$Exposure * .12)
  mb <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
            poisson(), db)
  expect_error(make_rating_table(mb, NULL, data = db, base = list(REGIO = "Oost")),
               "not a level")
  expect_error(make_rating_table(mb, NULL, data = db, base = list(LEEFTIJD = "oud")),
               "must be a number")
  expect_error(make_rating_table(mb, NULL, data = db, base = list(Exposure = 2)),
               "offset or exposure")
  expect_error(make_rating_table(mb, NULL, data = db, base = list("Zuid")),
               "must be named")
  expect_warning(make_rating_table(mb, NULL, data = db, base = list(REGOI = "Zuid")),
                 "ignored: REGOI")
  expect_warning(make_rating_table(mb, NULL, data = db, base = list(LEEFTIJD = 120)),
                 "outside the data")
})

test_that("two columns for one rating factor can be merged into one block", {
  set.seed(93)
  nm <- 30000
  dm <- data.frame(
    Stedelijkheid = factor(sample(1:5, nm, TRUE)),
    LEEFTIJD      = round(runif(nm, 18, 80)),
    Exposure      = round(runif(nm, .3, 1), 3))
  dm$Sted_groep <- factor(ifelse(dm$Stedelijkheid %in% c("4", "5"), "4-5",
                                 as.character(dm$Stedelijkheid)))
  dm$AantalClaims <- rpois(nm, dm$Exposure *
    exp(-2.2 + .15 * as.numeric(as.character(dm$Stedelijkheid))))
  dm$SCHADELAST <- ifelse(dm$AantalClaims > 0,
    dm$AantalClaims * rgamma(nm, 3, scale = 600 *
      ifelse(dm$Sted_groep == "4-5", 1.25, 1)), 0)
  mf <- glm(AantalClaims ~ Stedelijkheid + LEEFTIJD + offset(log(Exposure)),
            poisson(), dm)
  ds <- dm[dm$AantalClaims > 0, ]
  ds$AvgLoss <- ds$SCHADELAST / ds$AantalClaims
  ms <- glm(AvgLoss ~ Sted_groep, Gamma("log"), ds, weights = AantalClaims)
  mv <- list(Stedelijkheid = "Sted_groep")

  # undeclared: two blocks, each padded with neutral factors, and a hint
  expect_warning(t0 <- make_rating_table(mf, ms, data = dm),
                 "regrouping of another")
  expect_true(all(c("Stedelijkheid", "Sted_groep") %in% t0$Variable))
  expect_equal(t0$Factor_Severity[t0$Variable == "Stedelijkheid"], rep(1, 5))

  t1 <- make_rating_table(mf, ms, data = dm, merge_vars = mv)
  expect_false("Sted_groep" %in% t1$Variable)
  s1 <- t1[t1$Variable == "Stedelijkheid", ]
  expect_equal(nrow(s1), 5)
  # each level carries the severity factor of the group it falls in
  expect_equal(s1$Factor_Severity[s1$Level == "4"],
               s1$Factor_Severity[s1$Level == "5"])
  expect_equal(s1$Factor_Severity[s1$Level %in% c("4", "5")],
               rep(t0$Factor_Severity[t0$Variable == "Sted_groep" &
                                      t0$Level == "4-5"], 2))
  # and the frequency factor is untouched
  expect_equal(s1$Factor_Frequency,
               t0$Factor_Frequency[t0$Variable == "Stedelijkheid"])
  # volumes stay on the levels of the column a policy carries
  expect_equal(s1$Exposure,
               as.vector(tapply(dm$Exposure, dm$Stedelijkheid, sum)))

  # one block, one lookup, and the premium still reconstructs exactly
  nd <- data.frame(Stedelijkheid = factor("5", levels(dm$Stedelijkheid)),
                   Sted_groep = factor("4-5", levels(dm$Sted_groep)),
                   LEEFTIJD = 40, Exposure = 1, AantalClaims = 1)
  prem <- function(t) {
    f <- function(v, l) {
      s <- t[t$Variable == v & is.na(t$Group), ]
      s$Factor_Premium[match(l, s$Level)]
    }
    attr(t, "intercept_premium") * f("Stedelijkheid", "5") * f("LEEFTIJD", "40")
  }
  direct <- as.numeric(predict(mf, nd, type = "response") *
                       predict(ms, nd, type = "response"))
  expect_equal(prem(t1), direct, tolerance = 1e-10)
  # the undeclared table needs both blocks to get there, the merged one does not
  expect_equal(prem(t0) * t0$Factor_Premium[t0$Variable == "Sted_groep" &
                                            t0$Level == "4-5"],
               direct, tolerance = 1e-10)

  # a chosen base moves both columns, so the base row stays a real policy
  t2 <- make_rating_table(mf, ms, data = dm, merge_vars = mv,
                          base = list(Stedelijkheid = "5"))
  b2 <- t2[t2$Variable == "Stedelijkheid" & t2$IsBase, ]
  expect_identical(b2$Level, "5")
  expect_equal(c(b2$Factor_Frequency, b2$Factor_Severity), c(1, 1))
  # the intercept is that base policy: urbanisation 5 at the base age
  nd_b <- nd
  nd_b$LEEFTIJD <- attr(t2, "base_values")$LEEFTIJD
  expect_equal(attr(t2, "intercept_premium"),
               as.numeric(predict(mf, nd_b, type = "response") *
                          predict(ms, nd_b, type = "response")),
               tolerance = 1e-10)
  # and every premium is unchanged by that choice
  nd2 <- nd
  nd2$Stedelijkheid <- factor("2", levels(dm$Stedelijkheid))
  nd2$Sted_groep    <- factor("2", levels(dm$Sted_groep))
  p2 <- function(t) {
    f <- function(v, l) {
      s <- t[t$Variable == v & is.na(t$Group), ]
      s$Factor_Premium[match(l, s$Level)]
    }
    attr(t, "intercept_premium") * f("Stedelijkheid", "2") * f("LEEFTIJD", "40")
  }
  expect_equal(p2(t1), p2(t2), tolerance = 1e-10)
  expect_equal(p2(t2), as.numeric(predict(mf, nd2, type = "response") *
                                  predict(ms, nd2, type = "response")),
               tolerance = 1e-10)

  # a named vector is accepted as well
  expect_equal(make_rating_table(mf, ms, data = dm,
                                 merge_vars = c(Stedelijkheid = "Sted_groep")),
               t1)

  # the export writes one sheet for the merged variable, not two
  skip_if_not_installed("openxlsx")
  fp <- file.path(tempdir(), "merged.xlsx")
  export_rating_table(t1, fp)
  expect_true("Stedelijkheid" %in% openxlsx::getSheetNames(fp))
  expect_false("Sted_groep" %in% openxlsx::getSheetNames(fp))
})

test_that("a band of a continuous variable can be merged into it", {
  set.seed(94)
  nm <- 20000
  dm <- data.frame(LEEFTIJD = round(runif(nm, 18, 80)),
                   Exposure = round(runif(nm, .3, 1), 3))
  dm$LEEFTIJD_KL <- cut(dm$LEEFTIJD, c(17, 30, 50, 80),
                        labels = c("18-30", "31-50", "51-80"))
  dm$AantalClaims <- rpois(nm, dm$Exposure * .2)
  dm$SCHADELAST <- ifelse(dm$AantalClaims > 0,
    dm$AantalClaims * rgamma(nm, 3, scale = 600 *
      ifelse(dm$LEEFTIJD_KL == "18-30", 1.3, 1)), 0)
  mf <- glm(AantalClaims ~ LEEFTIJD + offset(log(Exposure)), poisson(), dm)
  ds <- dm[dm$AantalClaims > 0, ]
  ds$AvgLoss <- ds$SCHADELAST / ds$AantalClaims
  ms <- glm(AvgLoss ~ LEEFTIJD_KL, Gamma("log"), ds, weights = AantalClaims)

  t1 <- make_rating_table(mf, ms, data = dm,
                          merge_vars = list(LEEFTIJD = "LEEFTIJD_KL"),
                          grid_step = c(LEEFTIJD = 5))
  s <- t1[t1$Variable == "LEEFTIJD", ]
  expect_false("LEEFTIJD_KL" %in% t1$Variable)
  # a grid point takes the band it falls in, including the band edges
  band <- function(age) s$Factor_Severity[s$LevelNum == age]
  expect_equal(band(20), band(30))              # both in 18-30
  expect_equal(band(35), band(50))              # both in 31-50
  expect_equal(band(55), band(80))              # both in 51-80
  expect_false(isTRUE(all.equal(band(30), band(35))))
  nd <- data.frame(LEEFTIJD = 55,
                   LEEFTIJD_KL = factor("51-80", levels(dm$LEEFTIJD_KL)),
                   Exposure = 1, AantalClaims = 1)
  f <- function(v, l) {
    ss <- t1[t1$Variable == v & is.na(t1$Group), ]
    ss$Factor_Premium[match(l, ss$Level)]
  }
  expect_equal(attr(t1, "intercept_premium") * f("LEEFTIJD", "55"),
               as.numeric(predict(mf, nd, type = "response") *
                          predict(ms, nd, type = "response")),
               tolerance = 1e-10)
})

test_that("an interaction on a merged column is reported under one name", {
  set.seed(95)
  nm <- 25000
  dm <- data.frame(Sted = factor(sample(1:4, nm, TRUE)),
                   REGIO = factor(sample(c("Noord", "Zuid"), nm, TRUE)),
                   Exposure = round(runif(nm, .3, 1), 3))
  dm$Sted_groep <- factor(ifelse(dm$Sted %in% c("3", "4"), "3-4",
                                 as.character(dm$Sted)))
  dm$AantalClaims <- rpois(nm, dm$Exposure * .2)
  dm$SCHADELAST <- ifelse(dm$AantalClaims > 0,
                          dm$AantalClaims * rgamma(nm, 3, scale = 600), 0)
  mf <- glm(AantalClaims ~ Sted + REGIO + offset(log(Exposure)), poisson(), dm)
  ds <- dm[dm$AantalClaims > 0, ]
  ds$AvgLoss <- ds$SCHADELAST / ds$AantalClaims
  ms <- glm(AvgLoss ~ Sted_groep * REGIO, Gamma("log"), ds,
            weights = AantalClaims)

  t1 <- make_rating_table(mf, ms, data = dm,
                          merge_vars = list(Sted = "Sted_groep"))
  expect_false(any(grepl("Sted_groep", t1$Variable)))
  int <- t1[!is.na(t1$Group), ]
  expect_gt(nrow(int), 0)
  expect_identical(unique(int$Variable), "Sted:REGIO")
  # the interaction runs over the four levels of the finer column
  expect_setequal(unique(int$Level), levels(dm$Sted))
  expect_equal(int$Factor_Frequency[int$IsBase], 1)
  # the two grouped levels share the severity relativity
  sv <- function(l, g) int$Factor_Severity[int$Level == l & int$Group == g]
  expect_equal(sv("3", "Zuid"), sv("4", "Zuid"))
})

test_that("a merge declaration that cannot hold is refused", {
  set.seed(96)
  nm <- 5000
  dm <- data.frame(A = factor(sample(1:4, nm, TRUE)),
                   B = factor(sample(c("x", "y"), nm, TRUE)),
                   Exposure = runif(nm, .3, 1))
  dm$AantalClaims <- rpois(nm, dm$Exposure * .2)
  dm$A_groep <- factor(ifelse(dm$A %in% c("3", "4"), "3-4", as.character(dm$A)))
  mf <- glm(AantalClaims ~ A + offset(log(Exposure)), poisson(), dm)

  expect_error(make_rating_table(mf, NULL, data = dm,
                                 merge_vars = list(A = "B")),
               "not a regrouping")
  expect_error(make_rating_table(mf, NULL, data = dm,
                                 merge_vars = list(A = "A")),
               "onto itself")
  # a misspelled column is in neither the data nor a model, so there is
  # nothing to derive it from either
  expect_error(make_rating_table(mf, NULL, data = dm,
                                 merge_vars = list(A = "A_group")),
               "no model uses it")
  expect_error(make_rating_table(mf, NULL, data = dm,
                                 merge_vars = list(A = "A_groep",
                                                   B = "A_groep")),
               "regrouping of both")
  expect_error(make_rating_table(mf, NULL, data = dm,
                                 merge_vars = list("A_groep")),
               "must be named")
  # a valid relation on columns no model uses changes nothing, and says so
  dm$B_alles <- factor("alles")
  expect_warning(make_rating_table(mf, NULL, data = dm,
                                   merge_vars = list(B = "B_alles")),
                 "neither model uses")
})

test_that("a regrouped column missing from 'data' is taken from the model", {
  set.seed(97)
  nm <- 25000
  dm <- data.frame(Stedelijkheid = factor(sample(1:5, nm, TRUE)),
                   LEEFTIJD = round(runif(nm, 18, 80)),
                   Exposure = round(runif(nm, .3, 1), 3))
  dm$AantalClaims <- rpois(nm, dm$Exposure *
    exp(-2 + .15 * as.numeric(as.character(dm$Stedelijkheid))))
  dm$SCHADELAST <- ifelse(dm$AantalClaims > 0, dm$AantalClaims *
    rgamma(nm, 3, scale = 600 * ifelse(dm$Stedelijkheid %in% c("4", "5"),
                                       1.25, 1)), 0)
  ds <- dm[dm$AantalClaims > 0, ]
  ds$AvgLoss <- ds$SCHADELAST / ds$AantalClaims
  # the grouping is made next to the average claim, so only the severity
  # dataset has it
  ds$Sted_groep <- factor(ifelse(ds$Stedelijkheid %in% c("4", "5"), "4-5",
                                 as.character(ds$Stedelijkheid)))
  mf <- glm(AantalClaims ~ Stedelijkheid + LEEFTIJD + offset(log(Exposure)),
            poisson(), dm)
  ms <- glm(AvgLoss ~ Sted_groep, Gamma("log"), ds, weights = AantalClaims)
  mv <- list(Stedelijkheid = "Sted_groep")
  expect_false("Sted_groep" %in% names(dm))

  t1 <- make_rating_table(mf, ms, data = dm, merge_vars = mv)
  s1 <- t1[t1$Variable == "Stedelijkheid", ]
  expect_equal(nrow(s1), 5)
  expect_equal(s1$Factor_Severity[s1$Level == "4"],
               s1$Factor_Severity[s1$Level == "5"])

  # exactly what adding the column by hand would have given
  map <- unique(ms$data[, c("Stedelijkheid", "Sted_groep")])
  dm2 <- merge(dm, map, by = "Stedelijkheid", all.x = TRUE)
  expect_equal(t1, make_rating_table(mf, ms, data = dm2, merge_vars = mv))

  # and the caller's data frame is left alone
  expect_false("Sted_groep" %in% names(dm))

  nd <- data.frame(Stedelijkheid = factor("5", levels(dm$Stedelijkheid)),
                   Sted_groep = factor("4-5", levels(ds$Sted_groep)),
                   LEEFTIJD = 40, Exposure = 1, AantalClaims = 1)
  f <- function(v, l) {
    s <- t1[t1$Variable == v & is.na(t1$Group), ]
    s$Factor_Premium[match(l, s$Level)]
  }
  expect_equal(attr(t1, "intercept_premium") * f("Stedelijkheid", "5") *
                 f("LEEFTIJD", "40"),
               as.numeric(predict(mf, nd, type = "response") *
                          predict(ms, nd, type = "response")),
               tolerance = 1e-10)

  # a level with no claims at all has no group to derive, and is named
  ds3 <- ds[ds$Stedelijkheid != "1", ]
  ds3$Sted_groep <- droplevels(ds3$Sted_groep)
  ms3 <- glm(AvgLoss ~ Sted_groep, Gamma("log"), ds3, weights = AantalClaims)
  expect_error(make_rating_table(mf, ms3, data = dm, merge_vars = mv),
               "do not occur in the data the severity model")

  # a model without model$data, as glm.nb() leaves it, is read through the
  # data its call names (DATA-05)
  ms4 <- ms; ms4$data <- environment()
  expect_equal(make_rating_table(mf, ms4, data = dm, merge_vars = mv), t1)
  # but a model whose data cannot be found at all cannot be read
  ms5 <- ms4; ms5$call$data <- quote(no_such_data_frame)
  expect_error(make_rating_table(mf, ms5, data = dm, merge_vars = mv),
               "carries none")

  # a column that exists nowhere is not a merge at all
  expect_error(make_rating_table(mf, ms, data = dm,
                                 merge_vars = list(Stedelijkheid = "Sted_xyz")),
               "no model uses it")
})

test_that("a band missing from 'data' is derived per nearest observed value", {
  set.seed(98)
  nm <- 20000
  dm <- data.frame(LEEFTIJD = round(runif(nm, 18, 80)),
                   Exposure = round(runif(nm, .3, 1), 3))
  dm$AantalClaims <- rpois(nm, dm$Exposure * .25)
  dm$SCHADELAST <- ifelse(dm$AantalClaims > 0, dm$AantalClaims *
    rgamma(nm, 3, scale = 600 * ifelse(dm$LEEFTIJD <= 30, 1.3, 1)), 0)
  ds <- dm[dm$AantalClaims > 0, ]
  ds$AvgLoss <- ds$SCHADELAST / ds$AantalClaims
  ds$LEEFTIJD_KL <- cut(ds$LEEFTIJD, c(17, 30, 50, 80),
                        labels = c("18-30", "31-50", "51-80"))
  mf <- glm(AantalClaims ~ LEEFTIJD + offset(log(Exposure)), poisson(), dm)
  ms <- glm(AvgLoss ~ LEEFTIJD_KL, Gamma("log"), ds, weights = AantalClaims)

  t1 <- make_rating_table(mf, ms, data = dm,
                          merge_vars = list(LEEFTIJD = "LEEFTIJD_KL"),
                          grid_step = c(LEEFTIJD = 5))
  s <- t1[t1$Variable == "LEEFTIJD", ]
  band <- function(age) s$Factor_Severity[s$LevelNum == age]
  expect_equal(band(20), band(30))
  expect_equal(band(35), band(50))
  expect_equal(band(55), band(80))
  expect_false(isTRUE(all.equal(band(30), band(35))))
  expect_false("LEEFTIJD_KL" %in% names(dm))
})

test_that("one column name with two groupings is refused, not averaged over", {
  set.seed(99)
  nm <- 20000
  dm <- data.frame(MERK = factor(sample(c("A", "B", "C", "D"), nm, TRUE)),
                   Exposure = round(runif(nm, .3, 1), 3))
  dm$AantalClaims <- rpois(nm, dm$Exposure *
                             exp(-2 + .3 * (dm$MERK %in% c("C", "D"))))
  dm$SCHADELAST <- ifelse(dm$AantalClaims > 0, dm$AantalClaims *
    rgamma(nm, 3, scale = 600 * ifelse(dm$MERK == "D", 1.4, 1)), 0)
  sev_rows <- function(d) {
    s <- d[d$AantalClaims > 0, ]
    s$AvgLoss <- s$SCHADELAST / s$AantalClaims
    s
  }

  # the same grouping on both sides is fine and stays fine
  df <- dm; df$MERK_GROEP <- factor(ifelse(df$MERK %in% c("A", "B"), "G1", "G2"))
  ds <- sev_rows(df)
  mf <- glm(AantalClaims ~ MERK_GROEP + offset(log(Exposure)), poisson(), df)
  ms <- glm(AvgLoss ~ MERK_GROEP, Gamma("log"), ds, weights = AantalClaims)
  t_ok <- make_rating_table(mf, ms, data = dm,
                            merge_vars = list(MERK = "MERK_GROEP"))
  s <- t_ok[t_ok$Variable == "MERK", ]
  nd <- data.frame(MERK_GROEP = factor("G2", levels = c("G1", "G2")),
                   Exposure = 1, AantalClaims = 1)
  expect_equal(attr(t_ok, "intercept_premium") *
                 s$Factor_Premium[s$Level == "C"],
               as.numeric(predict(mf, nd, type = "response") *
                          predict(ms, nd, type = "response")),
               tolerance = 1e-10)

  # same labels, different membership: this used to produce a table whose
  # severity factors came from the frequency grouping, silently
  ds2 <- sev_rows(dm)
  ds2$MERK_GROEP <- factor(ifelse(ds2$MERK %in% c("A", "B", "C"), "G1", "G2"))
  ms2 <- glm(AvgLoss ~ MERK_GROEP, Gamma("log"), ds2, weights = AantalClaims)
  expect_error(make_rating_table(mf, ms2, data = dm,
                                 merge_vars = list(MERK = "MERK_GROEP")),
               "does not mean the same thing everywhere")
  expect_error(make_rating_table(mf, ms2, data = dm,
                                 merge_vars = list(MERK = "MERK_GROEP")),
               "'C' of 'MERK'")

  # different labels: refused with the same message rather than a bare
  # prediction failure further down
  ds3 <- sev_rows(dm)
  ds3$MERK_GROEP <- factor(ifelse(ds3$MERK %in% c("B", "C"), "B-C",
                                  as.character(ds3$MERK)))
  ms3 <- glm(AvgLoss ~ MERK_GROEP, Gamma("log"), ds3, weights = AantalClaims)
  expect_error(make_rating_table(mf, ms3, data = dm,
                                 merge_vars = list(MERK = "MERK_GROEP")),
               "does not mean the same thing everywhere")

  # the way out: two names, both declared
  df4 <- dm; df4$MERK_GR_F <- factor(ifelse(df4$MERK %in% c("A", "B"),
                                            "A-B", as.character(df4$MERK)))
  ds4 <- sev_rows(dm)
  ds4$MERK_GR_S <- factor(ifelse(ds4$MERK %in% c("B", "C"), "B-C",
                                 as.character(ds4$MERK)))
  mf4 <- glm(AantalClaims ~ MERK_GR_F + offset(log(Exposure)), poisson(), df4)
  ms4 <- glm(AvgLoss ~ MERK_GR_S, Gamma("log"), ds4, weights = AantalClaims)
  t4 <- make_rating_table(mf4, ms4, data = dm,
                          merge_vars = list(MERK = c("MERK_GR_F", "MERK_GR_S")))
  s4 <- t4[t4$Variable == "MERK", ]
  expect_equal(nrow(s4), 4)
  for (m in levels(dm$MERK)) {
    nd4 <- data.frame(
      MERK_GR_F = factor(if (m %in% c("A", "B")) "A-B" else m,
                         levels = levels(df4$MERK_GR_F)),
      MERK_GR_S = factor(if (m %in% c("B", "C")) "B-C" else m,
                         levels = levels(ds4$MERK_GR_S)),
      Exposure = 1, AantalClaims = 1)
    expect_equal(attr(t4, "intercept_premium") *
                   s4$Factor_Premium[s4$Level == m],
                 as.numeric(predict(mf4, nd4, type = "response") *
                            predict(ms4, nd4, type = "response")),
                 tolerance = 1e-10)
  }
})

test_that("two bandings of one continuous variable are kept apart", {
  set.seed(100)
  nm <- 20000
  dm <- data.frame(LEEFTIJD = round(runif(nm, 18, 80)),
                   Exposure = round(runif(nm, .3, 1), 3))
  dm$AantalClaims <- rpois(nm, dm$Exposure * .25)
  dm$SCHADELAST <- ifelse(dm$AantalClaims > 0, dm$AantalClaims *
    rgamma(nm, 3, scale = 600 * ifelse(dm$LEEFTIJD < 35, 1.3, 1)), 0)
  sev_rows <- function(d) {
    s <- d[d$AantalClaims > 0, ]
    s$AvgLoss <- s$SCHADELAST / s$AantalClaims
    s
  }
  band <- function(x, cut) cut(x, c(17, cut, 80), labels = c("LAAG", "HOOG"))

  # one name, same labels, different cut points: the strip between the two
  # cuts would get the wrong severity class
  df <- dm; df$LFT_KL <- band(df$LEEFTIJD, 40)
  ds <- sev_rows(dm); ds$LFT_KL <- band(ds$LEEFTIJD, 50)
  mf <- glm(AantalClaims ~ LFT_KL + offset(log(Exposure)), poisson(), df)
  ms <- glm(AvgLoss ~ LFT_KL, Gamma("log"), ds, weights = AantalClaims)
  expect_error(make_rating_table(mf, ms, data = dm,
                                 merge_vars = list(LEEFTIJD = "LFT_KL"),
                                 grid_step = c(LEEFTIJD = 5)),
               "does not mean the same thing everywhere")

  # two names: both bandings land in one block, on the real ages
  df2 <- dm; df2$LFT_F <- band(df2$LEEFTIJD, 40)
  ds2 <- sev_rows(dm); ds2$LFT_S <- band(ds2$LEEFTIJD, 50)
  mf2 <- glm(AantalClaims ~ LFT_F + offset(log(Exposure)), poisson(), df2)
  ms2 <- glm(AvgLoss ~ LFT_S, Gamma("log"), ds2, weights = AantalClaims)
  t2 <- make_rating_table(mf2, ms2, data = dm,
                          merge_vars = list(LEEFTIJD = c("LFT_F", "LFT_S")),
                          grid_step = c(LEEFTIJD = 5))
  s2 <- t2[t2$Variable == "LEEFTIJD", ]
  for (a in c(20, 45, 60)) {
    nd <- data.frame(LFT_F = band(a, 40), LFT_S = band(a, 50),
                     Exposure = 1, AantalClaims = 1)
    expect_equal(attr(t2, "intercept_premium") *
                   s2$Factor_Premium[s2$LevelNum == a],
                 as.numeric(predict(mf2, nd, type = "response") *
                            predict(ms2, nd, type = "response")),
                 tolerance = 1e-10)
  }
  # the age between the two cut points is where the two bandings disagree,
  # and it now carries the severity of its own band
  expect_equal(s2$Factor_Severity[s2$LevelNum == 45],
               s2$Factor_Severity[s2$LevelNum == 20])
  expect_false(isTRUE(all.equal(s2$Factor_Frequency[s2$LevelNum == 45],
                                s2$Factor_Frequency[s2$LevelNum == 20])))
})
