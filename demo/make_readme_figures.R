# ─────────────────────────────────────────────────────────────────────
# Regenerate the static figures used in README.md
#
# The plots are interactive plotly widgets, which GitHub cannot render in
# a README. This script renders each widget in headless Chrome and saves
# a PNG to man/figures/. Requires: webshot2 (+ a Chrome installation).
#
# Run from the project root:  Rscript demo/make_readme_figures.R
# ─────────────────────────────────────────────────────────────────────

library(pricingtoolsRmO)

stopifnot(requireNamespace("webshot2", quietly = TRUE),
          requireNamespace("htmlwidgets", quietly = TRUE))

out_dir <- "man/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Render one plotly widget to a PNG via headless Chrome.
# selfcontained = FALSE keeps the JS dependencies in a sibling folder,
# which avoids needing pandoc; Chrome loads them from disk just fine.
snap <- function(p, name, width = 900, height = 460) {
  # The interactive mode bar is clutter in a static screenshot
  p <- plotly::config(p, displayModeBar = FALSE)
  html <- file.path(tempdir(), "figs", paste0(name, ".html"))
  dir.create(dirname(html), showWarnings = FALSE, recursive = TRUE)
  htmlwidgets::saveWidget(p, html, selfcontained = FALSE)
  png <- file.path(out_dir, paste0("README-", name, ".png"))
  webshot2::webshot(paste0("file://", normalizePath(html, winslash = "/")),
                    file = png, vwidth = width, vheight = height,
                    delay = 2, zoom = 1.5)
  cat("written:", png, "-",
      round(file.info(png)$size / 1024), "KB\n")
  invisible(png)
}

# ── Portfolio (same structure as run_demo.R, smaller for speed) ───────
set.seed(2026)
n <- 40000

dat <- data.frame(
  LEEFTIJD  = pmin(pmax(round(rnorm(n, 46, 15)), 18), 85),
  REGIO     = factor(sample(c("Noord", "Oost", "Zuid", "West", "Randstad"),
                            n, TRUE, prob = c(.15, .18, .22, .20, .25))),
  GEWICHT   = round(pmin(pmax(rnorm(n, 1350, 250), 800), 2400), -1),
  BRANDSTOF = factor(sample(c("Benzine", "Diesel", "Elektrisch", "Waterstof"),
                            n, TRUE, prob = c(.623, .25, .125, .002))),
  BOEKJAAR  = sample(2020:2024, n, TRUE),
  Exposure  = round(runif(n, 0.05, 1), 3)
)

reg_f  <- c(Noord = 0, Oost = .05, Zuid = .12, West = .08, Randstad = .28)
fuel_f <- c(Benzine = 0, Diesel = .10, Elektrisch = -.05, Waterstof = 0)
lin_f <- -2.1 +
  0.035 * (30 - pmin(dat$LEEFTIJD, 30)) +
  0.012 * (pmax(dat$LEEFTIJD, 65) - 65) +
  reg_f[as.character(dat$REGIO)] +
  0.00025 * (dat$GEWICHT - 1350) +
  fuel_f[as.character(dat$BRANDSTOF)] +
  0.012 * (30 - pmin(dat$LEEFTIJD, 30)) * (dat$REGIO == "Randstad")
dat$AantalClaims <- rpois(n, dat$Exposure * exp(lin_f))

fuel_s <- c(Benzine = 0, Diesel = .05, Elektrisch = .35, Waterstof = .45)
mu_s   <- 1800 * exp(0.0004 * (dat$GEWICHT - 1350) +
                     fuel_s[as.character(dat$BRANDSTOF)])
dat$SCHADELAST <- ifelse(dat$AantalClaims > 0,
                         rgamma(n, shape = 1.6, rate = 1.6 / mu_s) *
                           dat$AantalClaims, 0)

reg_old <- c(Noord = .90, Oost = .95, Zuid = 1.05, West = 1.00,
             Randstad = 1.15)
dat$HUIDIGE_PREMIE <- dat$Exposure * 260 * reg_old[as.character(dat$REGIO)]

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

tbl <- make_rating_table(m_freq, m_sev, data = dat,
                         base_level = "exposure", trim = c(0.005, 0.995))

# ── Figures ──────────────────────────────────────────────────────────
snap(make_plot(dat, "REGIO", "Frequency", ta_blue,
               y_label = "Frequency", display = "color", by_year = TRUE),
     "oneway")

snap(plot_glm_predictor(m_freq, "LEEFTIJD", n_bins = 40),
     "actual-vs-expected")

snap(make_pdp(m_freq, dat, "LEEFTIJD", metric = "Frequency", grid_res = 40),
     "pdp")

snap(make_rating_plot(tbl, "LEEFTIJD"), "rating-factors")

# The interaction row is named after R's own term ordering (which may
# differ from the order you typed), so look it up in the table.
ivar <- unique(tbl$Variable[!is.na(tbl$Group)])[1]
snap(make_rating_plot(tbl, ivar, metric = "Premium"), "interaction")

snap(plot_glm_residuals(m_freq), "residuals")

imp <- premium_impact(dat, model_freq_new = m_freq, model_sev_new = m_sev,
                      old_premium_col = "HUIDIGE_PREMIE",
                      by = c("REGIO", "BRANDSTOF"))
snap(imp$plot, "premium-impact")

# Shared y-axis illustration: two predictors on one fixed range
rng <- c(0, 0.55)
snap(plot_glm_predictor(m_freq, "LEEFTIJD", n_bins = 40, y_range = rng),
     "yrange-age", width = 640, height = 380)
snap(plot_glm_predictor(m_freq, "REGIO", y_range = rng),
     "yrange-regio", width = 640, height = 380)

cat("\nAll figures written to", out_dir, "\n")
