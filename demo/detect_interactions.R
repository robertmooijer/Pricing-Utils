# ─────────────────────────────────────────────────────────────────────
# Demo: a GLM that is missing an interaction, and how to catch it
#
# The portfolio is simulated so that young drivers are worse everywhere,
# and *extra* bad in the city - a genuine interaction. The GLM that gets
# fitted has both main effects but no interaction term.
#
# The point of the demo: every standard one-way check on that GLM comes
# back clean. For a categorical variable that is in the model the A/E is
# exactly 1.000 at every level, because the score equations of a canonical
# link force the fitted totals to match the observed totals. A missing
# interaction cancels out in the margins by construction, so no amount of
# one-way plotting will reveal it. The cells do.
#
# Run from the project root:  Rscript demo/detect_interactions.R
# ─────────────────────────────────────────────────────────────────────

library(pricingtoolsRmO)

set.seed(7)
n <- 400000

dat <- data.frame(
  LEEFTIJD  = round(runif(n, 18, 80)),
  REGIO     = factor(sample(c("Stad", "Rand", "Dorp", "Platteland"), n, TRUE,
                            prob = c(.30, .25, .25, .20))),
  GEWICHT   = round(pmin(pmax(rnorm(n, 1350, 250), 800), 2400), -1),
  BOEKJAAR  = sample(2020:2024, n, TRUE),
  Exposure  = round(runif(n, 0.1, 1), 3)
)

# --- The truth: young drivers are extra bad in the city ---------------
reg  <- c(Stad = 0.25, Rand = 0.10, Dorp = 0.05, Platteland = 0)
jong <- pmax(0, 30 - dat$LEEFTIJD)               # 0 from age 30 onwards
lin  <- -1.9 +
  0.025 * jong +                                  # young drivers, everywhere
  0.010 * pmax(0, dat$LEEFTIJD - 65) +            # the elderly
  0.00025 * (dat$GEWICHT - 1350) +
  reg[as.character(dat$REGIO)] +
  0.050 * jong * (dat$REGIO == "Stad")            # <<< THE INTERACTION
dat$AantalClaims <- rpois(n, dat$Exposure * exp(lin))
dat$SCHADELAST   <- dat$AantalClaims * 1500

cat(sprintf("Portfolio: %s policies, %s claims, frequency %.3f\n\n",
            format(n, big.mark = ",", scientific = FALSE),
            format(sum(dat$AantalClaims), big.mark = ",", scientific = FALSE),
            sum(dat$AantalClaims) / sum(dat$Exposure)))

# --- The model the actuary fits: both main effects, no interaction ----
m <- glm(AantalClaims ~ ns(LEEFTIJD, 5) + REGIO + ns(GEWICHT, 3) +
           factor(BOEKJAAR) + offset(log(Exposure)),
         family = poisson(), data = dat)

# --- 1. The standard one-way checks: all clean ------------------------
mu <- predict(m, type = "response")
cat("A/E per REGIO (exactly 1 by construction):\n")
print(round(tapply(seq_len(n), dat$REGIO,
                   function(i) sum(dat$AantalClaims[i]) / sum(mu[i])), 4))

band <- cut(dat$LEEFTIJD, c(17, 24, 29, 39, 54, 69, 85))
cat("\nA/E per age band:\n")
print(round(tapply(seq_len(n), band,
                   function(i) sum(dat$AantalClaims[i]) / sum(mu[i])), 4))

# --- 2. The interaction scan ------------------------------------------
cat("\n── detect_interactions() ─────────────────────────────\n")
scan <- detect_interactions(m, n_bins = 10, n_sim = 200, seed = 1)
print(scan[, c("VarX", "VarY", "Claims", "Deviance", "DF", "Z", "P",
               "MaxAE", "MaxAE_ExposureShare")], row.names = FALSE,
      digits = 3)

# --- 3. Look at the winner --------------------------------------------
cat("\n── plot_residual_heatmap() on the top pair ───────────\n")
p <- plot_residual_heatmap(m, scan$VarX[1], scan$VarY[1], n_bins = 12)
cells <- attr(p, "cells")
cat("Worst cells (A/E furthest from 1, enough claims):\n")
ok <- cells[!cells$IsThin, ]
print(head(ok[order(-abs(ok$AE - 1)),
              c("gx", "gy", "Claims", "Exposure", "AE")], 6),
      row.names = FALSE, digits = 3)

# --- 4. Confirm: does adding the term help out of sample? -------------
cat("\n── The GLM decides ──────────────────────────────────\n")
i_tr <- sample(n, 0.7 * n)
tr <- dat[i_tr, ]; te <- dat[-i_tr, ]
dev <- function(y, mu) 2 * sum(ifelse(y > 0, y * log(y / mu), 0) - (y - mu))
f0 <- AantalClaims ~ ns(LEEFTIJD, 5) + REGIO + ns(GEWICHT, 3) +
  factor(BOEKJAAR) + offset(log(Exposure))
m0 <- glm(f0, family = poisson(), data = tr)
m1 <- glm(update(f0, . ~ . + ns(LEEFTIJD, 5):REGIO),
          family = poisson(), data = tr)
d0 <- dev(te$AantalClaims, predict(m0, newdata = te, type = "response"))
d1 <- dev(te$AantalClaims, predict(m1, newdata = te, type = "response"))
cat(sprintf("Out-of-sample deviance  without: %.0f\n                        with   : %.0f  (%+.2f%%)\n",
            d0, d1, 100 * (d1 - d0) / d0))
cat("\nA candidate only counts if it improves out-of-sample fit AND the\n",
    "resulting factor pattern is explainable. The scan proposes; the GLM decides.\n",
    sep = "")
