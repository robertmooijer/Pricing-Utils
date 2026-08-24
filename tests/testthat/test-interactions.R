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

test_that("each link pins a different quantity to exactly 1", {
  # The documented blind spot rests on the score equations weighting each
  # observation by (dmu/deta)/V(mu). That weight is 1 for a canonical link,
  # leaving sum(x*(y - mu)) = 0, so the A/E is exact. Gamma with a log link
  # is NOT canonical and pins the weighted mean of the RATIO instead. These
  # numbers are what the documentation claims, so they are pinned here.
  set.seed(31)
  nn <- 40000
  dd <- data.frame(L = round(runif(nn, 18, 80)),
                   R = factor(sample(c("N", "O", "Z"), nn, TRUE)),
                   Exposure = round(runif(nn, .3, 1), 3))
  dd$AantalClaims <- rpois(nn, dd$Exposure *
                           exp(-2.3 + .015 * dd$L + .3 * (dd$R == "Z")))
  dd$SCHADELAST <- dd$AantalClaims * rgamma(nn, 3, scale = 700)
  dsv <- dd[dd$AantalClaims > 0, ]
  dsv$Avg <- dsv$SCHADELAST / dsv$AantalClaims

  ae   <- function(y, mu, w, g) max(abs(tapply(seq_along(y), g, function(i)
            sum(w[i] * y[i]) / sum(w[i] * mu[i])) - 1))
  mean_ratio <- function(y, mu, w, g) max(abs(tapply(seq_along(y), g,
            function(i) sum(w[i] * y[i] / mu[i]) / sum(w[i])) - 1))

  # Poisson log: canonical, so the TOTALS match exactly
  mp <- glm(AantalClaims ~ L + R + offset(log(Exposure)), poisson(), dd)
  w1 <- rep(1, nn)
  expect_lt(ae(dd$AantalClaims, fitted(mp), w1, dd$R), 1e-10)
  expect_gt(mean_ratio(dd$AantalClaims, fitted(mp), w1, dd$R), 1e-4)

  # Gamma log: not canonical, so the mean RATIO matches instead
  mg <- glm(Avg ~ L + R, Gamma("log"), dsv, weights = AantalClaims)
  wg <- dsv$AantalClaims
  expect_lt(mean_ratio(dsv$Avg, fitted(mg), wg, dsv$R), 1e-6)
  expect_gt(ae(dsv$Avg, fitted(mg), wg, dsv$R), 1e-6)

  # Gamma inverse: canonical again, so the totals match and the ratio does not
  mi <- glm(Avg ~ L + R, Gamma("inverse"), dsv, weights = AantalClaims)
  expect_lt(ae(dsv$Avg, fitted(mi), wg, dsv$R), 1e-8)
  expect_gt(mean_ratio(dsv$Avg, fitted(mi), wg, dsv$R), 1e-6)

  # A saturated model is blind whatever the link, which is the second route
  ms <- glm(AantalClaims ~ R, poisson(link = "identity"), dd,
            start = c(mean(dd$AantalClaims), 0, 0))
  expect_lt(ae(dd$AantalClaims, fitted(ms), w1, dd$R), 1e-10)
})

test_that("the severity residue is far too small to price on", {
  # The claim is that a severity model is blind in practice, not by
  # identity: a real omitted interaction has to move the one-way A/E by
  # something you could see, and it does not.
  set.seed(31)
  nn <- 40000
  dd <- data.frame(L = round(runif(nn, 18, 80)),
                   R = factor(sample(c("N", "O", "Z"), nn, TRUE)),
                   Exposure = round(runif(nn, .3, 1), 3))
  dd$AantalClaims <- rpois(nn, dd$Exposure * exp(-2.3 + .015 * dd$L))
  dsv <- dd[dd$AantalClaims > 0, ]
  dsv$Avg <- rgamma(nrow(dsv), 3, scale = 700)
  wg <- dsv$AantalClaims
  ae <- function(y, mu) max(abs(tapply(seq_along(y), dsv$R, function(i)
          sum(wg[i] * y[i]) / sum(wg[i] * mu[i])) - 1))

  m0 <- glm(Avg ~ L + R, Gamma("log"), dsv, weights = AantalClaims)
  base <- ae(dsv$Avg, fitted(m0))

  # now plant a 35% interaction the model is not given
  dsv$Avg2 <- dsv$Avg * ifelse(dsv$R == "Z" & dsv$L > 50, 1.35, 1)
  m1 <- glm(Avg2 ~ L + R, Gamma("log"), dsv, weights = AantalClaims)
  planted <- ae(dsv$Avg2, fitted(m1))

  expect_lt(base, 0.001)        # numerical floor, ~0.01%
  expect_lt(planted, 0.01)      # a 35% effect still moves it under 1%
  expect_gt(planted, base)      # it is visible in principle

  # and the cell-level test finds what the margins hide
  r <- suppressWarnings(suppressMessages(
    detect_interactions(m1, vars = c("L", "R"), n_bins = 4, n_sim = 99)))
  expect_gt(max(abs(r$MaxAE - 1), na.rm = TRUE), 10 * planted)
})
