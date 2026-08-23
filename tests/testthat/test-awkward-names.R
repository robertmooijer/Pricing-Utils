# Non-syntactic column names and offsets that are not called "Exposure".
# Both used to fail silently or take the whole rating table down with them.

set.seed(41)
naw <- 15000
daw <- data.frame(
  REGIO      = factor(sample(c("Stad", "Dorp"), naw, TRUE)),
  LEEFTIJD   = round(runif(naw, 18, 80)),
  POLISJAREN = round(runif(naw, .3, 1), 3),
  check.names = FALSE)
daw[["AUTO GEWICHT"]] <- round(runif(naw, 800, 2000))
daw$Exposure <- daw$POLISJAREN
daw$AantalClaims <- rpois(naw, daw$POLISJAREN * exp(
  -1.7 + 0.3 * (daw$REGIO == "Stad") + 0.0002 * (daw[["AUTO GEWICHT"]] - 1400)))
daw$SCHADELAST <- daw$AantalClaims * 1500

test_that("a column name with a space survives the whole rating table", {
  m <- glm(AantalClaims ~ REGIO + `AUTO GEWICHT` + offset(log(Exposure)),
           family = poisson(), data = daw)
  tb <- make_rating_table(m, NULL, data = daw)

  # the awkward variable gets its own rows, and so do the others
  expect_true(all(c("REGIO", "AUTO GEWICHT") %in% tb$Variable))
  expect_gt(sum(tb$Variable == "AUTO GEWICHT"), 1)
  # and the factors are real numbers, not NA
  expect_false(any(is.na(tb$Factor_Frequency)))

  # the same name must not break the other entry points either
  expect_s3_class(make_rating_plot(tb, "AUTO GEWICHT"), "plotly")
  expect_s3_class(make_pdp(m, daw, "AUTO GEWICHT"), "plotly")
  r <- detect_interactions(m, n_bins = 5, n_sim = 30, seed = 1)
  expect_true(all(c("REGIO", "AUTO GEWICHT") %in% c(r$VarX, r$VarY)))
})

test_that("I() terms resolve to their underlying column", {
  m <- glm(AantalClaims ~ REGIO + LEEFTIJD + I(LEEFTIJD^2) +
             offset(log(Exposure)), family = poisson(), data = daw)
  expect_silent(tb <- make_rating_table(m, NULL, data = daw))
  expect_true("LEEFTIJD" %in% tb$Variable)
  # LEEFTIJD appears once, not once per term it occurs in
  expect_equal(length(unique(tb$Variable)), 2)
})

test_that("the intercept is per unit of exposure whatever the offset is called", {
  # exposure_col stays at its default while the model offsets on POLISJAREN
  m <- glm(AantalClaims ~ LEEFTIJD + REGIO + offset(log(POLISJAREN)),
           family = poisson(), data = daw)
  expect_warning(tb <- make_rating_table(m, NULL, data = daw), "offsets on")

  i_f <- attr(tb, "intercept_frequency")
  base_row <- data.frame(
    LEEFTIJD   = median(daw$LEEFTIJD),
    REGIO      = factor(levels(daw$REGIO)[1], levels(daw$REGIO)),
    POLISJAREN = 1)
  expect_equal(i_f, as.numeric(predict(m, newdata = base_row,
                                       type = "response")))
  # and specifically NOT the median-exposure version that used to come out
  base_row$POLISJAREN <- median(daw$POLISJAREN)
  expect_false(isTRUE(all.equal(
    i_f, as.numeric(predict(m, newdata = base_row, type = "response")))))
})

test_that("a Poisson model without an offset still gets the simulated null", {
  m <- glm(AantalClaims ~ LEEFTIJD + REGIO, family = poisson(), data = daw)
  r <- detect_interactions(m, n_bins = 5, n_sim = 30, seed = 1)
  expect_identical(unique(r$Method), "simulation")

  # a Gamma severity model genuinely cannot use it, and says why
  ds <- daw[daw$AantalClaims > 0, ]
  ds$Gem <- ds$SCHADELAST / ds$AantalClaims
  m_g <- glm(Gem ~ REGIO + LEEFTIJD, family = Gamma(link = "log"),
             data = ds, weights = AantalClaims)
  expect_message(r2 <- detect_interactions(m_g, n_bins = 5, n_sim = 30),
                 "not Poisson counts")
  expect_identical(unique(r2$Method), "chisq")
})

test_that("a trimmed grid excludes the policies it does not cover", {
  m <- glm(AantalClaims ~ `AUTO GEWICHT` + REGIO + offset(log(Exposure)),
           family = poisson(), data = daw)
  x <- daw[["AUTO GEWICHT"]]

  # untrimmed: every policy is counted somewhere
  tb_full <- make_rating_table(m, NULL, data = daw)
  s_full  <- tb_full[tb_full$Variable == "AUTO GEWICHT", ]
  expect_equal(sum(s_full$Exposure), sum(daw$Exposure), tolerance = 1e-8)

  # trimmed: the bars cover exactly the policies inside the grid range,
  # rather than sweeping the tails into the outer bins
  tb_trim <- make_rating_table(m, NULL, data = daw, trim = c(.01, .99))
  s_trim  <- tb_trim[tb_trim$Variable == "AUTO GEWICHT", ]
  inside  <- x >= min(s_trim$LevelNum) & x <= max(s_trim$LevelNum)
  expect_equal(sum(s_trim$Exposure), sum(daw$Exposure[inside]),
               tolerance = 1e-8)
  expect_lt(sum(s_trim$Exposure), sum(daw$Exposure))
})

test_that("export sheet names are deduplicated case-insensitively", {
  skip_if_not_installed("openxlsx")
  tb <- make_rating_table(
    glm(AantalClaims ~ REGIO + offset(log(Exposure)), family = poisson(),
        data = daw), NULL, data = daw)
  tb2 <- rbind(tb, transform(tb, Variable = "Regio"))
  f <- file.path(tempdir(), "case.xlsx")
  export_rating_table(tb2, f)
  sn <- openxlsx::getSheetNames(f)
  expect_equal(length(sn), length(unique(tolower(sn))))
})

test_that("export_rating_table accepts digits = 0", {
  skip_if_not_installed("openxlsx")
  tb <- make_rating_table(
    glm(AantalClaims ~ REGIO + offset(log(Exposure)), family = poisson(),
        data = daw), NULL, data = daw)
  f <- file.path(tempdir(), "digits0.xlsx")
  expect_silent(export_rating_table(tb, f, digits = 0))
  expect_true(file.exists(f))
})
