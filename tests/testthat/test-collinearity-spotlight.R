# glm_collinearity() ---------------------------------------------------------

set.seed(91)
nc <- 20000
dc <- data.frame(
  LEEFTIJD = round(runif(nc, 18, 80)),
  GEWICHT  = round(pmin(pmax(rnorm(nc, 1350, 250), 800), 2400), -1),
  REGIO    = factor(sample(c("Stad", "Dorp", "Rand"), nc, TRUE)),
  Exposure = round(runif(nc, .3, 1), 3))
# a near-duplicate of GEWICHT: the pair must light up, the rest must not
dc$GEWICHT_PROXY <- dc$GEWICHT + round(rnorm(nc, 0, 25))
dc$AantalClaims <- rpois(nc, dc$Exposure * exp(
  -1.7 + 0.02 * pmax(0, 30 - dc$LEEFTIJD) + 0.0003 * (dc$GEWICHT - 1350)))

test_that("collinear terms are flagged and independent ones are not", {
  m_col <- glm(AantalClaims ~ LEEFTIJD + GEWICHT + GEWICHT_PROXY + REGIO +
                 offset(log(Exposure)), family = poisson(), data = dc)
  v <- suppressWarnings(glm_collinearity(m_col))

  expect_true(all(c("Term", "DF", "GVIF", "GVIF_scaled", "Flag") %in% names(v)))
  expect_setequal(v$Term, c("LEEFTIJD", "GEWICHT", "GEWICHT_PROXY", "REGIO"))
  expect_true(all(v$Term[v$Flag] %in% c("GEWICHT", "GEWICHT_PROXY")))
  expect_true(all(c("GEWICHT", "GEWICHT_PROXY") %in% v$Term[v$Flag]))
  expect_false(v$Flag[v$Term == "REGIO"])
  expect_false(is.unsorted(rev(v$GVIF_scaled)))  # sorted descending
  expect_warning(glm_collinearity(m_col), "scaled GVIF")
})

test_that("a clean model flags nothing", {
  m_ok <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
              family = poisson(), data = dc)
  v <- glm_collinearity(m_ok)
  expect_false(any(v$Flag))
  expect_true(all(v$GVIF_scaled < 2))
})

test_that("multi-column terms use the generalised form", {
  m_ns <- glm(AantalClaims ~ ns(LEEFTIJD, 4) + REGIO + offset(log(Exposure)),
              family = poisson(), data = dc)
  v <- glm_collinearity(m_ns)
  expect_equal(v$DF[v$Term == "ns(LEEFTIJD, 4)"], 4)
  expect_equal(v$DF[v$Term == "REGIO"], 2)
  # scaled version is the df-th root, so it must be far below the raw GVIF
  expect_lt(v$GVIF_scaled[v$Term == "ns(LEEFTIJD, 4)"],
            v$GVIF[v$Term == "ns(LEEFTIJD, 4)"])
})

test_that("splines and rank deficiency do not empty the table", {
  # Many correlated spline columns: det(R) underflows to zero long before
  # the model is singular, so the GVIF must be computed on log-determinants
  m_sp <- glm(AantalClaims ~ ns(LEEFTIJD, 5) + ns(GEWICHT, 3) + REGIO +
                offset(log(Exposure)), family = poisson(), data = dc)
  v_sp <- glm_collinearity(m_sp)
  expect_equal(nrow(v_sp), 3)
  expect_true(all(is.finite(v_sp$GVIF_scaled)))
  expect_false(any(v_sp$Flag))

  # A rank-deficient fit leaves an aliased coefficient; it is dropped with
  # a warning and the remaining terms still get a number
  m_rd <- glm(AantalClaims ~ ns(LEEFTIJD, 5) + REGIO + LEEFTIJD:REGIO +
                offset(log(Exposure)), family = poisson(), data = dc)
  skip_if(sum(is.na(coef(m_rd))) == 0, "fit was not rank deficient here")
  v_rd <- suppressWarnings(glm_collinearity(m_rd))
  expect_gt(nrow(v_rd), 1)
  expect_true(all(is.finite(v_rd$GVIF_scaled)))
  # the interaction and REGIO, which it contains, are reported, not flagged
  expect_warning(expect_warning(glm_collinearity(m_rd), "rank"),
                 "belong to an interaction")
  expect_true(all(is.na(v_rd$Flag[v_rd$InInteraction])))
  expect_setequal(v_rd$Term[v_rd$InInteraction], c("REGIO", "REGIO:LEEFTIJD"))
})

test_that("a GVIF that depends on the coding is not flagged", {
  # Centring the numeric side of an interaction leaves the fit identical,
  # and moves the GVIF of the factor's main effect, whose coefficients are
  # then measured at another value of GEWICHT (DIAG-07)
  m_a <- glm(AantalClaims ~ GEWICHT * REGIO + LEEFTIJD +
               offset(log(Exposure)), family = poisson(), data = dc)
  dc2 <- dc; dc2$GEWICHT <- dc2$GEWICHT - 1350
  m_b <- update(m_a, data = dc2)
  expect_equal(fitted(m_a), fitted(m_b), tolerance = 1e-8)
  g_a <- suppressWarnings(glm_collinearity(m_a))
  g_b <- suppressWarnings(glm_collinearity(m_b))
  ga <- g_a$GVIF_scaled[g_a$Term == "REGIO"]
  gb <- g_b$GVIF_scaled[g_b$Term == "REGIO"]
  expect_gt(ga / gb, 2)
  # so neither version flags the terms of the interaction, and a term
  # outside it still gets its flag
  int_terms <- c("GEWICHT", "REGIO", "GEWICHT:REGIO")
  expect_setequal(g_a$Term[g_a$InInteraction], int_terms)
  expect_true(all(is.na(g_a$Flag[g_a$Term %in% int_terms])))
  expect_true(all(is.na(g_b$Flag[g_b$Term %in% int_terms])))
  expect_identical(g_a$Flag[g_a$Term == "LEEFTIJD"], FALSE)
  expect_warning(glm_collinearity(m_a), "belong to an interaction")
})

test_that("a model with fewer than two terms returns nothing", {
  m1 <- glm(AantalClaims ~ LEEFTIJD + offset(log(Exposure)),
            family = poisson(), data = dc)
  expect_message(v <- glm_collinearity(m1), "fewer than two")
  expect_equal(nrow(v), 0)
})

# premium_impact(): contribution and spotlight --------------------------------

dc$HUIDIGE_PREMIE <- dc$Exposure * 250
m_new <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(Exposure)),
             family = poisson(), data = dc)

test_that("contributions sum to the portfolio change", {
  imp <- premium_impact(dc, model_freq_new = m_new,
                        old_premium_col = "HUIDIGE_PREMIE", by = "REGIO")
  expect_true(all(c("ExposureShare", "Contribution") %in% names(imp$by_level)))
  expect_equal(sum(imp$by_level$ExposureShare), 1, tolerance = 1e-8)

  # levels of one variable decompose the change in the total premium
  # (RATE-04): exactly 0 after a rebase, the rate-level change without one
  expect_equal(sum(imp$by_level$Contribution), 0, tolerance = 1e-8)
  imp_nr <- premium_impact(dc, model_freq_new = m_new,
                           old_premium_col = "HUIDIGE_PREMIE", by = "REGIO",
                           rebase = FALSE)
  expect_equal(sum(imp_nr$by_level$Contribution),
               100 * imp_nr$stats$rate_change, tolerance = 1e-8)

  # also when the old premium is not flat, which is where weighting the
  # mean change by exposure did not add up
  dc3 <- dc
  dc3$HUIDIGE_PREMIE <- dc3$Exposure * 250 * ifelse(dc3$REGIO == "Stad", 1.3, 1)
  imp3 <- premium_impact(dc3, model_freq_new = m_new,
                         old_premium_col = "HUIDIGE_PREMIE", by = "REGIO")
  expect_equal(sum(imp3$by_level$Contribution), 0, tolerance = 1e-8)
  bl <- imp3$by_level
  expect_equal(bl$Contribution, bl$PremiumShare * bl$TotalChangePct,
               tolerance = 1e-8)
  expect_equal(sum(bl$PremiumShare), 1, tolerance = 1e-8)
})

test_that("the spotlight reports a subset without filtering the book", {
  imp <- premium_impact(dc, model_freq_new = m_new,
                        old_premium_col = "HUIDIGE_PREMIE",
                        spotlight = REGIO == "Stad" & LEEFTIJD < 25)
  sel <- dc$REGIO == "Stad" & dc$LEEFTIJD < 25

  expect_false(is.null(imp$spotlight))
  expect_equal(imp$spotlight$n_rows, sum(sel))
  # the book itself is untouched: every row is still in $policy
  expect_equal(nrow(imp$policy), nrow(dc))
  expect_equal(imp$spotlight$exposure_share,
               sum(dc$Exposure[sel]) / sum(dc$Exposure), tolerance = 1e-8)
  expect_s3_class(imp$spotlight$summary, "data.frame")

  # a logical vector must give the same answer as the expression
  imp2 <- premium_impact(dc, model_freq_new = m_new,
                         old_premium_col = "HUIDIGE_PREMIE",
                         spotlight = sel)
  expect_equal(imp$spotlight$mean_change, imp2$spotlight$mean_change)
})

test_that("spotlight input is validated", {
  expect_error(premium_impact(dc, model_freq_new = m_new,
                              old_premium_col = "HUIDIGE_PREMIE",
                              spotlight = c(TRUE, FALSE)),
               "one TRUE/FALSE per row")
  expect_warning(premium_impact(dc, model_freq_new = m_new,
                                old_premium_col = "HUIDIGE_PREMIE",
                                spotlight = LEEFTIJD > 999),
                 "no usable rows")
})
