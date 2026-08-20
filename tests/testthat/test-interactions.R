# A GLM that is missing a known interaction must be caught by the cell-level
# tools, precisely because the one-way checks cannot see it.

make_missed_interaction <- function(n = 60000, seed = 11) {
  set.seed(seed)
  d <- data.frame(
    LEEFTIJD = round(runif(n, 18, 80)),
    REGIO    = factor(sample(c("Stad", "Rand", "Dorp"), n, TRUE)),
    Exposure = round(runif(n, 0.1, 1), 3))
  jong <- pmax(0, 30 - d$LEEFTIJD)
  lin <- -1.9 + 0.025 * jong +
    c(Stad = .25, Rand = .10, Dorp = 0)[as.character(d$REGIO)] +
    0.060 * jong * (d$REGIO == "Stad")          # the interaction
  d$AantalClaims <- rpois(n, d$Exposure * exp(lin))
  d$SCHADELAST   <- d$AantalClaims * 1500
  d
}

dsim <- make_missed_interaction()
m_gap <- glm(AantalClaims ~ ns(LEEFTIJD, 5) + REGIO + offset(log(Exposure)),
             family = poisson(), data = dsim)

test_that("one-way A/E is blind to the missing interaction", {
  # Score equations of a canonical link: fitted totals equal observed
  # totals for every level of a categorical variable in the model
  mu <- predict(m_gap, type = "response")
  ae <- tapply(seq_len(nrow(dsim)), dsim$REGIO,
               function(i) sum(dsim$AantalClaims[i]) / sum(mu[i]))
  expect_true(all(abs(ae - 1) < 1e-8))
})

test_that("detect_interactions ranks the true pair first", {
  r <- detect_interactions(m_gap, n_bins = 8, n_sim = 100, seed = 1)
  expect_s3_class(r, "data.frame")
  expect_true(all(c("VarX", "VarY", "Z", "P", "Method",
                    "MaxAE", "MaxAE_ExposureShare") %in% names(r)))
  expect_identical(r$Method[1], "simulation")
  expect_identical(sort(c(r$VarX[1], r$VarY[1])), c("LEEFTIJD", "REGIO"))
  expect_gt(r$Z[1], 5)
  expect_lt(r$P[1], 0.05)
  # the worst cell is materially, not just significantly, off
  expect_gt(abs(r$MaxAE[1] - 1), 0.15)
})

test_that("no interaction present means no signal", {
  set.seed(3)
  d <- dsim
  # same marginals, but the claims now follow a purely additive truth
  jong <- pmax(0, 30 - d$LEEFTIJD)
  lin  <- -1.9 + 0.025 * jong +
    c(Stad = .25, Rand = .10, Dorp = 0)[as.character(d$REGIO)]
  d$AantalClaims <- rpois(nrow(d), d$Exposure * exp(lin))
  m_ok <- glm(AantalClaims ~ ns(LEEFTIJD, 5) + REGIO + offset(log(Exposure)),
              family = poisson(), data = d)
  r <- detect_interactions(m_ok, n_bins = 8, n_sim = 100, seed = 1)
  expect_lt(r$Z[1], 4)          # nothing stands out
  expect_gt(r$P[1], 0.01)
})

test_that("plot_residual_heatmap returns a plot plus its cell table", {
  p <- plot_residual_heatmap(m_gap, "LEEFTIJD", "REGIO", n_bins = 8)
  expect_s3_class(p, "plotly")

  cells <- attr(p, "cells")
  expect_s3_class(cells, "data.frame")
  expect_true(all(c("AE", "Claims", "Exposure", "IsThin") %in% names(cells)))

  # the young/Stad corner must be the underpriced one
  young <- levels(cells$gx)[1]
  ae_young <- cells$AE[as.character(cells$gx) == young]
  names(ae_young) <- as.character(cells$gy)[as.character(cells$gx) == young]
  expect_gt(ae_young[["Stad"]], 1.10)
  expect_lt(ae_young[["Dorp"]], 1.00)
})

test_that("thin cells are not coloured and z_range is validated", {
  d <- dsim
  d$RAAR <- factor(ifelse(seq_len(nrow(d)) <= 20, "Zeldzaam", "Normaal"))
  m <- glm(AantalClaims ~ REGIO + RAAR + offset(log(Exposure)),
           family = poisson(), data = d)
  p <- plot_residual_heatmap(m, "REGIO", "RAAR", min_claims = 30)
  cells <- attr(p, "cells")
  expect_true(any(cells$IsThin))

  # a thin cell keeps its A/E in the table but is blank in the plotted z
  z <- plotly::plotly_build(p)$x$data[[1]]$z
  expect_true(any(is.na(unlist(z))))

  expect_error(plot_residual_heatmap(m_gap, "LEEFTIJD", "REGIO",
                                     z_range = c(2, 1)), "z_range")
})
