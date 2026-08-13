# ─────────────────────────────────────────────────────────────────────
# GLM Utils — demo
#
# Simulates a realistic motor portfolio (100k policies, Dutch column
# names matching the toolkit defaults), fits a frequency and a severity
# model, and produces every deliverable of the toolkit:
#
#   demo/pricing_report.html   full interactive HTML report
#   demo/rating_table.xlsx     formatted Excel rating workbook
#   demo/premium_impact.html   dislocation histogram vs the "current" tariff
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
cat("\n")

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
imp <- premium_impact(dat,
                      model_freq_new  = m_freq,
                      model_sev_new   = m_sev,
                      old_premium_col = "HUIDIGE_PREMIE",
                      by = c("REGIO", "BRANDSTOF"))
cat("── Premium impact ─────────────────────────────────\n")
print(imp$summary, row.names = FALSE)
cat("\nMean change per level:\n")
print(imp$by_level, row.names = FALSE, digits = 3)
htmltools::save_html(imp$plot, "demo/premium_impact.html",
                     libdir = "premium_impact_files")
cat("\nWritten: demo/premium_impact.html\n\n")

# ── Full HTML report ─────────────────────────────────────────────────
pricing_report(m_freq, m_sev, dat,
               file  = "demo/pricing_report.html",
               title = "Demo — Motor portfolio GLM pricing",
               by_year = FALSE, grid_res = 40,
               base_level = "exposure", trim = c(0.005, 0.995))
cat("\nDemo complete. Open demo/pricing_report.html in a browser.\n")
