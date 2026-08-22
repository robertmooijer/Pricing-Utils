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

  # levels of one variable decompose the overall (rebased) change, which is
  # zero on average by construction of the rebase
  overall <- sum(imp$policy$ChangePct * imp$policy$Exposure) /
             sum(imp$policy$Exposure)
  expect_equal(sum(imp$by_level$Contribution), overall, tolerance = 1e-8)
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
