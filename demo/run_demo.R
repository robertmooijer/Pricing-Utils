# ─────────────────────────────────────────────────────────────────────
# pricingtoolsRmO — demo
#
# Simulates a realistic motor portfolio (100k policies, Dutch column
# names matching the toolkit defaults), fits a frequency and a severity
# model, and walks the whole workflow: diagnose the models, compare them
# at portfolio level, build the tariff, and measure what it does to the
# book. Produces:
#
#   demo/pricing_report.html   full interactive HTML report
#   demo/rating_table.xlsx     formatted Excel rating workbook
#   demo/premium_impact.html   dislocation histogram vs the "current" tariff
#   demo/lift.html             lift chart and double lift
#
# For the interaction tooling (detect_interactions, plot_residual_heatmap
# and feature screening) see demo/detect_interactions.R, which is built
# around a portfolio with a known missing interaction.
#
# Built-in demo talking points:
#   * BRANDSTOF = "Waterstof" is deliberately rare (~10-15 claims):
#     shows the IsThin flag and the dimmed markers.
#   * A true LEEFTIJD × REGIO interaction (young drivers worse in the
#     Randstad): shows the interaction plot and the Uplift columns.
#   * The "current" tariff only differentiates by REGIO: the premium
#     impact shows how the new model redistributes premium.
#
# Install the package once, then run from the project root:
#   remotes::install_github("robertmooijer/Pricing-Utils")
#   Rscript demo/run_demo.R
# ─────────────────────────────────────────────────────────────────────

library(pricingtoolsRmO)   # ns()/bs() are re-exported by the package

set.seed(2026)
n <- 100000

dat <- data.frame(
  LEEFTIJD  = pmin(pmax(round(rnorm(n, 46, 15)), 18), 85),
  REGIO     = factor(sample(c("Noord","Oost","Zuid","West","Randstad"), n, TRUE,
                            prob = c(.15, .18, .22, .20, .25))),
  GEWICHT   = round(pmin(pmax(rnorm(n, 1350, 250), 800), 2400), -1),
  BRANDSTOF = factor(sample(c("Benzine","Diesel","Elektrisch","Waterstof"), n, TRUE,
                            prob = c(.623, .25, .125, .002))),
  BOEKJAAR  = sample(2020:2024, n, TRUE),
  Exposure  = round(runif(n, 0.05, 1), 3)
)

# True frequency: young/old drivers, region, weight, fuel, small year trend,
# plus a REAL young-driver × Randstad interaction
reg_f  <- c(Noord = 0, Oost = .05, Zuid = .12, West = .08, Randstad = .28)
fuel_f <- c(Benzine = 0, Diesel = .10, Elektrisch = -.05, Waterstof = 0)
lin_f <- -2.1 +
  0.035 * (30 - pmin(dat$LEEFTIJD, 30)) +
  0.012 * (pmax(dat$LEEFTIJD, 65) - 65) +
  reg_f[as.character(dat$REGIO)] +
  0.00025 * (dat$GEWICHT - 1350) +
  fuel_f[as.character(dat$BRANDSTOF)] +
  0.02 * (dat$BOEKJAAR - 2020) +
  0.012 * (30 - pmin(dat$LEEFTIJD, 30)) * (dat$REGIO == "Randstad")
dat$AantalClaims <- rpois(n, dat$Exposure * exp(lin_f))

# True severity: weight and fuel (EVs/hydrogen more expensive per claim)
fuel_s <- c(Benzine = 0, Diesel = .05, Elektrisch = .35, Waterstof = .45)
mu_s   <- 1800 * exp(0.0004 * (dat$GEWICHT - 1350) +
                     fuel_s[as.character(dat$BRANDSTOF)])
dat$SCHADELAST <- ifelse(dat$AantalClaims > 0,
                         rgamma(n, shape = 1.6, rate = 1.6 / mu_s) *
                           dat$AantalClaims, 0)

# "Current" tariff: crude, only REGIO-differentiated
reg_old <- c(Noord = .90, Oost = .95, Zuid = 1.05, West = 1.00, Randstad = 1.15)
dat$HUIDIGE_PREMIE <- dat$Exposure * 260 * reg_old[as.character(dat$REGIO)]

cat("Portfolio:", format(n, big.mark = ",", scientific = FALSE), "policies,",
    format(sum(dat$AantalClaims), big.mark = ","), "claims, frequency",
    round(sum(dat$AantalClaims) / sum(dat$Exposure), 4), "\n\n")

# ── Models ───────────────────────────────────────────────────────────
m_freq <- glm(AantalClaims ~ ns(LEEFTIJD, 5) + REGIO + ns(GEWICHT, 3) +
                BRANDSTOF + factor(BOEKJAAR) + LEEFTIJD:REGIO +
                offset(log(Exposure)),
              family = poisson(), data = dat)

d_sev <- dat[dat$AantalClaims > 0, ]
d_sev$AvgLoss <- d_sev$SCHADELAST / d_sev$AantalClaims
m_sev <- glm(AvgLoss ~ ns(GEWICHT, 3) + BRANDSTOF,
             family = Gamma(link = "log"), data = d_sev,
             weights = AantalClaims)

cat("── Model diagnostics ──────────────────────────────\n")
print(glm_diagnostics(m_freq, m_sev), digits = 4)

# Collinearity is about interpretation, not prediction: a term that is
# largely explained by the others still forecasts fine, but its rating
# factors cannot be read on their own.
# This model carries both ns(LEEFTIJD, 5) and LEEFTIJD:REGIO, so the
# interaction and the region main effect share information - which is
# exactly what the generalised VIF is meant to surface.
cat("\nCollinearity (generalised VIF per term):\n")
print(glm_collinearity(m_freq), row.names = FALSE, digits = 4)
cat("\n")

# ── Does the model separate risk, and does it beat the incumbent? ─────
cat("── Portfolio-level comparison ─────────────────────\n")
lift <- model_lift(m_freq, m_sev, data = dat, actual_col = "SCHADELAST")
cat(sprintf("Gini: %.3f  (0 = no discrimination)\n", lift$gini))
print(lift$table, row.names = FALSE, digits = 4)

# The current tariff only knows REGIO, so the two disagree sharply about
# young city drivers - which is exactly where a double lift decides.
dbl <- double_lift(dat, model_freq_new = m_freq, model_sev_new = m_sev,
                   old_premium_col = "HUIDIGE_PREMIE",
                   actual_col = "SCHADELAST")
cat(sprintf("\nDouble lift - mean |A/E - 1|: new %.3f vs old %.3f -> %s wins\n",
            dbl$stats$mad_new, dbl$stats$mad_old, dbl$stats$winner))
print(dbl$table, row.names = FALSE, digits = 4)

htmltools::save_html(htmltools::tagList(lift$plot, lift$plot_lorenz,
                                        dbl$plot),
                     "demo/lift.html", libdir = "lift_files")
cat("\nWritten: demo/lift.html\n\n")

# ── Rating table + Excel export ──────────────────────────────────────
tbl <- make_rating_table(m_freq, m_sev, data = dat,
                         base_level = "exposure",
                         trim = c(0.005, 0.995))
cat("Base premium per exposure unit:",
    round(attr(tbl, "intercept_premium"), 2), "\n")
thin <- tbl[tbl$IsThin %in% TRUE & is.na(tbl$Group) & tbl$Type == "categorical",
            c("Variable", "Level", "Exposure", "ClaimCount")]
cat("Thin categorical levels (IsThin):\n"); print(thin, row.names = FALSE)
cat("\n")

export_rating_table(tbl, "demo/rating_table.xlsx")
cat("Written: demo/rating_table.xlsx\n\n")

# ── Premium impact vs the current tariff ─────────────────────────────
# The spotlight is the segment this tariff change is really about: young
# drivers in the city, who the old REGIO-only tariff underprices.
imp <- premium_impact(dat,
                      model_freq_new  = m_freq,
                      model_sev_new   = m_sev,
                      old_premium_col = "HUIDIGE_PREMIE",
                      by = c("REGIO", "BRANDSTOF"),
                      spotlight = REGIO == "Randstad" & LEEFTIJD < 25)
cat("── Premium impact ─────────────────────────────────\n")
print(imp$summary, row.names = FALSE)

cat("\nSpotlight - young drivers in the Randstad:\n")
print(imp$spotlight$summary, row.names = FALSE)

cat("\nContribution per level (share x change; sums to the book change):\n")
print(imp$by_level, row.names = FALSE, digits = 3)
htmltools::save_html(imp$plot, "demo/premium_impact.html",
                     libdir = "premium_impact_files")
cat("\nWritten: demo/premium_impact.html\n\n")

# ── A second offset: a bonus-malus scale ─────────────────────────────
# A BM scale is usually GIVEN rather than estimated: the discount ladder
# is a commercial decision, and the GLM is asked to price everything else
# around it. That is what a second offset is for.
#
#     glm(claims ~ ... + offset(log(Exposure)) + offset(log(BM)))
#
# The block below is self-contained: it extends the portfolio with a BM
# class, redraws the claims so the discount is real, and then walks a
# single policy from base premium to quote.

cat("── A second offset: bonus-malus ───────────────────
")

bm_scale <- c(M = 1.40, "0" = 1.00, "5" = 0.85, "10" = 0.70, "15" = 0.55)

dat_bm <- dat
# No-claim years accumulate, so BM tracks age: the weighting question this
# raises is only visible when the two are related.
p_bm <- function(age) {
  w <- rbind(M  = 0.30 - 0.0030 * (age - 18),
             `0`  = 0.35 - 0.0025 * (age - 18),
             `5`  = 0.20 + 0.0010 * (age - 18),
             `10` = 0.10 + 0.0025 * (age - 18),
             `15` = 0.05 + 0.0020 * (age - 18))
  w[w < 0.01] <- 0.01
  sweep(w, 2, colSums(w), "/")
}
pk <- p_bm(dat_bm$LEEFTIJD)
dat_bm$BM_KLASSE <- factor(
  vapply(seq_len(n), function(i) sample(names(bm_scale), 1, prob = pk[, i]),
         character(1)),
  levels = names(bm_scale))
dat_bm$BM <- unname(bm_scale[as.character(dat_bm$BM_KLASSE)])

# Redraw claims with the discount as a genuine rate effect
dat_bm$AantalClaims <- rpois(n, dat_bm$Exposure * dat_bm$BM * exp(lin_f))
dat_bm$SCHADELAST <- ifelse(dat_bm$AantalClaims > 0,
                            rgamma(n, shape = 1.6, rate = 1.6 / mu_s) *
                              dat_bm$AantalClaims, 0)

m_bm <- glm(AantalClaims ~ ns(LEEFTIJD, 5) + REGIO + ns(GEWICHT, 3) +
              BRANDSTOF + offset(log(Exposure)) + offset(log(BM)),
            family = poisson(), data = dat_bm)

d_sev_bm <- dat_bm[dat_bm$AantalClaims > 0, ]
d_sev_bm$AvgLoss <- d_sev_bm$SCHADELAST / d_sev_bm$AantalClaims
m_sev_bm <- glm(AvgLoss ~ ns(GEWICHT, 3) + BRANDSTOF, family = Gamma("log"),
                data = d_sev_bm, weights = AantalClaims)

cat("BM scale (given, not estimated):
")
print(data.frame(Klasse = names(bm_scale), Factor = unname(bm_scale),
                 Polissen = as.vector(table(dat_bm$BM_KLASSE))),
      row.names = FALSE)

# ── What the rating table gives you ──────────────────────────────────
# .model_rate() removes BOTH offsets, so intercept and factors are the
# tariff BEFORE the discount. BM is applied afterwards, which is exactly
# how you would want to publish it.
tbl_bm <- make_rating_table(m_bm, m_sev_bm, data = dat_bm,
                            base_level = "exposure", trim = c(0.005, 0.995))
cat(sprintf("
Base premium per policy-year, BEFORE BM: %.2f
",
            attr(tbl_bm, "intercept_premium")))

# ── One policy, all the way through ──────────────────────────────────
# Deliberately off the base on every variable, so every line of the quote
# actually does something. The base is the largest-exposure level here
# (base_level = "exposure"), i.e. Randstad and Benzine.
polis <- data.frame(LEEFTIJD = 24,
                    REGIO = factor("Noord", levels(dat$REGIO)),
                    GEWICHT = 2000,
                    BRANDSTOF = factor("Elektrisch", levels(dat$BRANDSTOF)),
                    BM_KLASSE = factor("M", levels(names(bm_scale))),
                    BM = bm_scale[["M"]], Exposure = 0.5)

fac <- function(v, val) {
  s <- tbl_bm[tbl_bm$Variable == v & is.na(tbl_bm$Group), ]
  if (s$Type[1] == "continuous")
    s$Factor_Premium[which.min(abs(s$LevelNum - as.numeric(val)))]
  else s$Factor_Premium[match(as.character(val), s$Level)]
}
stap <- c(
  "Base premium (per policy-year, before BM)" = attr(tbl_bm, "intercept_premium"),
  "x LEEFTIJD = 24"          = fac("LEEFTIJD", 24),
  "x REGIO = Noord"          = fac("REGIO", "Noord"),
  "x GEWICHT = 2000"         = fac("GEWICHT", 2000),
  "x BRANDSTOF = Elektrisch" = fac("BRANDSTOF", "Elektrisch"),
  "x BM class M (malus)"     = polis$BM,
  "x Exposure = 0.5 year"    = polis$Exposure)
cat("
Quote for one policy, step by step:
")
print(data.frame(Step = names(stap), Factor = round(unname(stap), 4),
                 Running = round(cumprod(unname(stap)), 2)), row.names = FALSE)

direct <- as.numeric(predict(m_bm, polis, type = "response")) *
          as.numeric(predict(m_sev_bm, polis, type = "response"))
verschil <- abs(prod(stap) - direct)
cat(sprintf("
Chain: %.2f   predict(): %.2f   difference: %.2e
",
            prod(stap), direct, verschil))
cat(if (verschil < 1e-8)
      paste0("Both LEEFTIJD and GEWICHT sit on a grid point here, so the two
",
             "agree to floating-point precision.
")
    else sprintf(paste0("The table stores factors on a grid, so a value ",
                        "between two grid points
costs %.2f of rounding. ",
                        "Lower grid_step to shrink it.
"), verschil))

# ── What the axis divides by ─────────────────────────────────────────
# exp(offset) is Exposure x BM here. Both series divide by the exposure
# itself, so the axis stays in claims per policy-year.
byreg <- aggregate(cbind(Exposure, AantalClaims, mu) ~ REGIO,
                   data = transform(dat_bm, mu = fitted(m_bm)), FUN = sum)
byreg$obs_freq  <- byreg$AantalClaims / byreg$Exposure
byreg$pred_freq <- byreg$mu / byreg$Exposure
byreg$per_ExBM  <- byreg$AantalClaims /
  aggregate(I(Exposure * BM) ~ REGIO, dat_bm, sum)[, 2]
cat("
One-way over REGIO. The package divides by exposure (obs_freq), not
",
    "by exposure x BM (per_ExBM) - the A/E is the same either way, but only
",
    "the first is a frequency you can quote.
", sep = "")
print(byreg[, c("REGIO", "Exposure", "obs_freq", "pred_freq", "per_ExBM")],
      row.names = FALSE, digits = 4)

# ── Validating the BM layer itself ───────────────────────────────────
# BM carries no estimated parameter, so its A/E is NOT pinned to 1 the way
# a fitted categorical term is. That makes this a real test of whether the
# commercial ladder matches the experience.
ae_per_class <- function(model, data) {
  a <- aggregate(cbind(AantalClaims, mu) ~ BM_KLASSE,
                 data = transform(data, mu = fitted(model)), FUN = sum)
  a$AE <- a$AantalClaims / a$mu
  a
}
cat("
A/E per BM class, scale as used in pricing:
")
print(ae_per_class(m_bm, dat_bm)[, c("BM_KLASSE", "AantalClaims", "AE")],
      row.names = FALSE, digits = 4)
cat("All near 1: this ladder matches the experience, which it should -
",
    "the claims above were generated with exactly these factors.
", sep = "")

# The same check on a ladder that is too FLAT: half the discount, half the
# malus. Nothing else changes, and the A/E now runs across the classes.
bm_flat <- 1 + (bm_scale - 1) / 2
dat_flat <- dat_bm
dat_flat$BM <- unname(bm_flat[as.character(dat_flat$BM_KLASSE)])
m_flat <- glm(AantalClaims ~ ns(LEEFTIJD, 5) + REGIO + ns(GEWICHT, 3) +
                BRANDSTOF + offset(log(Exposure)) + offset(log(BM)),
              family = poisson(), data = dat_flat)
cat("
Same portfolio, but priced on a ladder half as steep:
")
flat_tab <- cbind(ae_per_class(m_flat, dat_flat)[, c("BM_KLASSE", "AE")],
                  Gebruikt = unname(bm_flat), Zou_moeten = unname(bm_scale))
flat_tab$Ratio <- flat_tab$Zou_moeten / flat_tab$Gebruikt
print(flat_tab, row.names = FALSE, digits = 4)
cat("The A/E now falls from the malus class to the best one. The malus is
",
    "too mild, so those policies bring more claims than they are charged
",
    "for (A/E above 1), and the discounts are too stingy, so the best
",
    "classes bring fewer (A/E below 1). Note the last column: the A/E per
",
    "class tracks the ratio of the factor that SHOULD have been used to the
",
    "one that was, up to the intercept absorbing the overall level. That
",
    "gradient is the signal; a ladder that fits shows none.
", sep = "")

htmltools::save_html(plot_glm_predictor(m_bm, "BM_KLASSE"),
                     "demo/bm_check.html", libdir = "bm_check_files")
cat("
Written: demo/bm_check.html

")

# ── Full HTML report ─────────────────────────────────────────────────
pricing_report(m_freq, m_sev, dat,
               file  = "demo/pricing_report.html",
               title = "Demo — Motor portfolio GLM pricing",
               by_year = FALSE, grid_res = 40,
               base_level = "exposure", trim = c(0.005, 0.995))
cat("\nDemo complete. Open demo/pricing_report.html in a browser.\n")
