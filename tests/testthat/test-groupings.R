# add_groupings() adds the grouped columns a model was fitted on to a
# dataset that only has the source columns, reading the relation off the
# data each model carries. It shares its code with merge_vars in
# make_rating_table(), so the table and a predict() on new data agree.

set.seed(88)
ng <- 30000
dg <- data.frame(
  LEEFTIJD      = round(runif(ng, 18, 80)),
  REGIO         = factor(sample(c("Noord", "Zuid", "Randstad"), ng, TRUE)),
  Stedelijkheid = factor(sample(1:5, ng, TRUE)),
  MERK          = factor(sample(c("BMW", "MERCEDES", "NISSAN", "TOYOTA"), ng,
                                TRUE)),
  BM_KLASSE     = sample(1:5, ng, TRUE),
  Exposure      = round(runif(ng, .1, 1), 3))
ladder <- c(1, .9, .8, .7, .6)
dg$BM_KORTING <- ladder[dg$BM_KLASSE]
dg$AantalClaims <- rpois(ng, dg$Exposure * dg$BM_KORTING *
  exp(-2.1 + .012 * (dg$LEEFTIJD - 40) + .2 * (dg$MERK == "NISSAN")))
dg$SCHADELAST <- ifelse(dg$AantalClaims > 0, dg$AantalClaims *
  rgamma(ng, 3, scale = 700 * ifelse(dg$MERK == "MERCEDES", 1.4, 1)), 0)

gf <- dg
gf$MERK_GR_F <- factor(ifelse(gf$MERK %in% c("BMW", "MERCEDES"), "BMW_MERCEDES",
                              as.character(gf$MERK)))
g_freq <- glm(AantalClaims ~ ns(LEEFTIJD, 4) + MERK_GR_F + REGIO +
                offset(log(Exposure)) + offset(log(BM_KORTING)),
              family = poisson(), data = gf)
gs <- dg[dg$AantalClaims > 0, ]
gs$AvgLoss <- gs$SCHADELAST / gs$AantalClaims
gs$MERK_GR_S <- factor(ifelse(gs$MERK %in% c("BMW", "NISSAN"), "BMW_NISSAN",
                              as.character(gs$MERK)))
gs$Stedelijkheid_groep <- factor(ifelse(gs$Stedelijkheid %in% c("4", "5"), "4-5",
                                        as.character(gs$Stedelijkheid)))
gs$LFT_KL <- cut(gs$LEEFTIJD, c(17, 30, 50, 80),
                 labels = c("18-30", "31-50", "51-80"))
g_sev <- glm(AvgLoss ~ MERK_GR_S + Stedelijkheid_groep + LFT_KL,
             family = Gamma("log"), data = gs, weights = AantalClaims)

gmv <- list(Stedelijkheid = "Stedelijkheid_groep",
            MERK          = c("MERK_GR_F", "MERK_GR_S"),
            LEEFTIJD      = "LFT_KL")

new_pf <- function() {
  d <- data.frame(LEEFTIJD = c(22, 30, 31, 50, 64),
                  REGIO = c("Noord", "Zuid", "Randstad", "Noord", "Zuid"),
                  Stedelijkheid = c(1, 3, 4, 5, 2),
                  MERK = c("BMW", "MERCEDES", "NISSAN", "TOYOTA", "BMW"),
                  BM_KLASSE = c(1, 3, 5, 2, 4), stringsAsFactors = FALSE)
  d$Exposure <- 1
  d$BM_KORTING <- ladder[d$BM_KLASSE]
  d
}

test_that("the grouped columns are derived from the source columns", {
  nd <- add_groupings(new_pf(), g_freq, g_sev, merge_vars = gmv)
  expect_identical(as.character(nd$Stedelijkheid_groep),
                   c("1", "3", "4-5", "4-5", "2"))
  expect_identical(as.character(nd$MERK_GR_F),
                   c("BMW_MERCEDES", "BMW_MERCEDES", "NISSAN", "TOYOTA",
                     "BMW_MERCEDES"))
  expect_identical(as.character(nd$MERK_GR_S),
                   c("BMW_NISSAN", "MERCEDES", "BMW_NISSAN", "TOYOTA",
                     "BMW_NISSAN"))
  # the band edges land on the right side
  expect_identical(as.character(nd$LFT_KL),
                   c("18-30", "18-30", "31-50", "31-50", "51-80"))
  # with the types and levels the models were fitted on
  expect_identical(levels(nd$MERK_GR_S), levels(gs$MERK_GR_S))
  expect_identical(levels(nd$LFT_KL), levels(gs$LFT_KL))

  # and predict() runs on it, matching the rating table built from the
  # same declaration, BM applied on top
  prem <- as.numeric(predict(g_freq, nd, type = "response") *
                     predict(g_sev,  nd, type = "response"))
  tb <- make_rating_table(g_freq, g_sev, data = dg, merge_vars = gmv,
                          grid_step = c(LEEFTIJD = 1))
  fac <- function(v, l) {
    s <- tb[tb$Variable == v & is.na(tb$Group), ]
    s$Factor_Premium[match(as.character(l), s$Level)]
  }
  recon <- vapply(seq_len(nrow(nd)), function(i)
    attr(tb, "intercept_premium") * fac("LEEFTIJD", nd$LEEFTIJD[i]) *
      fac("REGIO", nd$REGIO[i]) * fac("Stedelijkheid", nd$Stedelijkheid[i]) *
      fac("MERK", nd$MERK[i]) * nd$BM_KORTING[i], numeric(1))
  expect_equal(recon, prem, tolerance = 1e-10)
})

test_that("anything that would be a guess is refused", {
  b <- new_pf(); b$Stedelijkheid[2] <- 6
  expect_error(add_groupings(b, g_freq, g_sev, merge_vars = gmv),
               "6 of 'Stedelijkheid' do not occur in the data the severity")
  b <- new_pf(); b$MERK[3] <- "TESLA"
  expect_error(add_groupings(b, g_freq, g_sev, merge_vars = gmv),
               "TESLA of 'MERK' do not occur in the data the frequency")
  # a column already present is checked, never overwritten
  b <- new_pf()
  b$MERK_GR_F <- c("BMW_MERCEDES", "MERCEDES", "NISSAN", "TOYOTA",
                   "BMW_MERCEDES")
  expect_error(add_groupings(b, g_freq, g_sev, merge_vars = gmv),
               "row 2 has 'MERK' = MERCEDES")
  b$MERK_GR_F[2] <- "BMW_MERCEDES"
  ok <- add_groupings(b, g_freq, g_sev, merge_vars = gmv)
  expect_identical(ok$MERK_GR_F, b$MERK_GR_F)

  # a numeric code is looked up exactly, not given a neighbour's group
  gs2 <- gs; gs2$Stedelijkheid <- as.numeric(as.character(gs2$Stedelijkheid))
  s2 <- glm(AvgLoss ~ MERK_GR_S + Stedelijkheid_groep + LFT_KL,
            family = Gamma("log"), data = gs2, weights = AantalClaims)
  b <- new_pf(); b$Stedelijkheid[2] <- 6
  expect_error(add_groupings(b, g_freq, s2, merge_vars = gmv),
               "do not occur")

  # the arguments themselves
  expect_error(add_groupings(new_pf(), merge_vars = gmv), "at least one model")
  expect_error(add_groupings(new_pf(), g_freq, g_sev), "'merge_vars' is required")
  expect_error(add_groupings(new_pf(), g_freq, g_sev,
                             merge_vars = list("LFT_KL")), "must be named")
  expect_error(add_groupings(new_pf(), g_freq, g_sev,
                             merge_vars = list(LEEFTIJD = "LEEFTIJD")),
               "onto itself")
  expect_error(add_groupings(new_pf(), g_freq, g_sev,
                             merge_vars = list(LEEFTIJD = "LFT_XX")),
               "no model uses it")
})

test_that("one name grouped two ways by the two models is refused", {
  f2 <- dg; f2$MG <- factor(ifelse(f2$MERK %in% c("BMW", "MERCEDES"), "G1", "G2"))
  s2 <- dg[dg$AantalClaims > 0, ]
  s2$AvgLoss <- s2$SCHADELAST / s2$AantalClaims
  s2$MG <- factor(ifelse(s2$MERK %in% c("BMW", "MERCEDES", "NISSAN"), "G1", "G2"))
  mf2 <- glm(AantalClaims ~ MG + offset(log(Exposure)), poisson(), f2)
  ms2 <- glm(AvgLoss ~ MG, Gamma("log"), s2, weights = AantalClaims)
  expect_error(add_groupings(new_pf(), mf2, ms2, merge_vars = list(MERK = "MG")),
               "does not mean the same thing everywhere: 'NISSAN' of 'MERK'")
  # the same grouping on both sides is fine
  s3 <- dg[dg$AantalClaims > 0, ]
  s3$AvgLoss <- s3$SCHADELAST / s3$AantalClaims
  s3$MG <- factor(ifelse(s3$MERK %in% c("BMW", "MERCEDES"), "G1", "G2"))
  ms3 <- glm(AvgLoss ~ MG, Gamma("log"), s3, weights = AantalClaims)
  nd <- add_groupings(new_pf(), mf2, ms3, merge_vars = list(MERK = "MG"))
  expect_identical(as.character(nd$MG), c("G1", "G1", "G2", "G2", "G1"))
})

test_that("a continuous source outside the fitted range takes the outer band", {
  b <- new_pf(); b$LEEFTIJD <- c(15, 30, 31, 50, 85)
  expect_warning(nd <- add_groupings(b, g_freq, g_sev, merge_vars = gmv),
                 "1 row above 80 got '51-80' and 1 row below 18 got '18-30'")
  expect_identical(as.character(nd$LFT_KL),
                   c("18-30", "18-30", "31-50", "31-50", "51-80"))
  # and the spline that also uses the age is named, with the remedy
  expect_warning(add_groupings(b, g_freq, g_sev, merge_vars = gmv),
                 "continuous term of the frequency model")
  expect_warning(add_groupings(b, g_freq, g_sev, merge_vars = gmv),
                 "pmin\\(pmax\\(LEEFTIJD, 18\\), 80\\)")
  # inside the range nothing is said
  expect_silent(add_groupings(new_pf(), g_freq, g_sev, merge_vars = gmv))
})

test_that("a data.table comes back as one, and the input is left alone", {
  dt <- data.table::as.data.table(new_pf())
  out <- add_groupings(dt, g_freq, g_sev, merge_vars = gmv)
  expect_true(data.table::is.data.table(out))
  expect_true(all(c("LFT_KL", "MERK_GR_F", "MERK_GR_S",
                    "Stedelijkheid_groep") %in% names(out)))
  expect_false("LFT_KL" %in% names(dt))
})
