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

# Render a data.frame excerpt as a styled table image, mirroring the
# highlighting used by export_rating_table(): base row in light blue,
# thin rows (few claims) greyed out and italic.
snap_table <- function(df, name, digits = 3, vwidth = 1800) {
  h <- htmltools::tags
  # Format per column type, as export_rating_table() does: counts with a
  # thousands separator and no decimals, factors with decimals.
  # Decide the format per COLUMN, so a whole-number column never picks up
  # spurious decimals and a large one keeps its thousands separator
  col_fmt <- vapply(names(df), function(nm) {
    x <- df[[nm]]
    if (!is.numeric(x)) return("chr")
    fin <- x[is.finite(x)]
    if (!length(fin)) return("chr")
    if (all(abs(fin - round(fin)) < 1e-9)) "int"
    else if (max(abs(fin)) >= 1000) "big" else "dec"
  }, character(1))
  fm <- function(x, nm) {
    if (is.logical(x)) return(ifelse(is.na(x), "", ifelse(x, "TRUE", "FALSE")))
    switch(col_fmt[[nm]],
      chr = as.character(x),
      int = format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE),
      big = formatC(x, format = "f", digits = 1, big.mark = ","),
      dec = formatC(x, format = "f", digits = digits))
  }
  rows <- lapply(seq_len(nrow(df)), function(i) {
    cls <- if (isTRUE(df$IsBase[i])) "base"
           else if (isTRUE(df$IsThin[i])) "thin" else ""
    h$tr(class = cls,
         lapply(seq_along(df), function(j) h$td(fm(df[i, j], names(df)[j]))))
  })
  page <- h$div(
    h$style(htmltools::HTML("
      body{margin:0;background:#fff;font-family:'Segoe UI',Arial,sans-serif;}
      .wrap{display:inline-block;padding:14px;}
      table{border-collapse:collapse;font-size:13px;}
      th{background:#00365E;color:#fff;padding:7px 11px;text-align:left;
         white-space:nowrap;}
      td{border-bottom:1px solid #D0D8E0;padding:6px 11px;white-space:nowrap;}
      tr.base td{background:#A8C8E0;font-weight:bold;}
      tr.thin td{color:#999;font-style:italic;}")),
    h$div(class = "wrap",
          h$table(h$thead(h$tr(lapply(names(df), h$th))), h$tbody(rows))))

  html <- file.path(tempdir(), "figs", paste0(name, ".html"))
  dir.create(dirname(html), showWarnings = FALSE, recursive = TRUE)
  htmltools::save_html(page, html)
  png <- file.path(out_dir, paste0("README-", name, ".png"))
  # The viewport must be wider than the table, otherwise the crop to
  # .wrap still clips the right-hand columns.
  webshot2::webshot(paste0("file://", normalizePath(html, winslash = "/")),
                    file = png, selector = ".wrap", zoom = 2, delay = 1,
                    vwidth = vwidth)
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

# Categorical variable: BRANDSTOF has a deliberately rare level
# ("Waterstof"), which make_rating_plot() dims as a thin cell.
snap(make_rating_plot(tbl, "BRANDSTOF"), "rating-plot-categorical",
     width = 800)

# The rating table itself, styled like the Excel export
snap_table(
  tbl[tbl$Variable == "BRANDSTOF" & is.na(tbl$Group),
      c("Level", "IsBase", "Exposure", "ClaimCount", "IsThin",
        "Factor_Frequency", "Factor_Severity", "Factor_Premium")],
  "rating-table")

# The interaction row is named after R's own term ordering (which may
# differ from the order you typed), so look it up in the table.
ivar <- unique(tbl$Variable[!is.na(tbl$Group)])[1]
snap(make_rating_plot(tbl, ivar, metric = "Premium"), "interaction")

snap(plot_glm_residuals(m_freq), "residuals")

# Interaction scan. Cell-level analysis needs volume: the portfolio above
# has ~3k claims, which spread over 60 cells is mostly noise. So this
# figure uses a bigger portfolio, with the model deliberately fitted
# WITHOUT the interaction that is in the data.
set.seed(7)
n2 <- 400000
d2 <- data.frame(
  LEEFTIJD = round(runif(n2, 18, 80)),
  REGIO    = factor(sample(c("Stad", "Rand", "Dorp", "Platteland"), n2, TRUE,
                           prob = c(.30, .25, .25, .20))),
  Exposure = round(runif(n2, 0.1, 1), 3))
jong2 <- pmax(0, 30 - d2$LEEFTIJD)
lin2  <- -1.9 + 0.025 * jong2 + 0.010 * pmax(0, d2$LEEFTIJD - 65) +
  c(Stad = .25, Rand = .10, Dorp = .05, Platteland = 0)[as.character(d2$REGIO)] +
  0.050 * jong2 * (d2$REGIO == "Stad")          # the missing interaction
d2$AantalClaims <- rpois(n2, d2$Exposure * exp(lin2))

m_gap <- glm(AantalClaims ~ ns(LEEFTIJD, 5) + REGIO + offset(log(Exposure)),
             family = poisson(), data = d2)
snap(plot_residual_heatmap(m_gap, "LEEFTIJD", "REGIO", n_bins = 10),
     "residual-heatmap", width = 800)

# Portfolio-level comparison. Both sides must predict the same quantity as
# actual_col, so this compares two FREQUENCY models against claim counts:
# the full one against a region-only tariff.
m_thin <- glm(AantalClaims ~ REGIO + offset(log(Exposure)),
              family = poisson(), data = dat)
snap(model_lift(m_freq, data = dat, actual_col = "AantalClaims")$plot,
     "lift", width = 800)
snap(double_lift(dat, model_freq_new = m_freq, model_freq_old = m_thin,
                 actual_col = "AantalClaims")$plot,
     "double-lift", width = 800)

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

## ── Figures for the functions that return a table or a file ──────────

# Screenshot of an arbitrary HTML page, viewport only, for the report
snap_page <- function(file, name, width = 1100, height = 850) {
  png <- file.path(out_dir, paste0("README-", name, ".png"))
  webshot2::webshot(paste0("file://", normalizePath(file, winslash = "/")),
                    file = png, vwidth = width, vheight = height,
                    delay = 3, zoom = 1.5, cliprect = "viewport")
  cat("written:", png, "-", round(file.info(png)$size / 1024), "KB\n")
  invisible(png)
}

# agg_all(): the aggregate that make_plot() builds internally
snap_table(head(agg_all(dat, "REGIO", by_year = TRUE), 8), "agg-all")

# glm_diagnostics(): one row per model
snap_table(glm_diagnostics(m_freq, m_sev), "diagnostics")

# glm_collinearity(): a deliberately duplicated predictor, so the pair
# lights up and the independent terms do not
dat$GEWICHT_PROXY <- dat$GEWICHT + round(rnorm(nrow(dat), 0, 30))
m_col <- glm(AantalClaims ~ LEEFTIJD + GEWICHT + GEWICHT_PROXY + REGIO +
               BRANDSTOF + offset(log(Exposure)),
             family = poisson(), data = dat)
snap_table(suppressWarnings(glm_collinearity(m_col)), "collinearity")

# detect_interactions(): the scan on the model that is missing one
snap_table(detect_interactions(m_gap, n_bins = 8, n_sim = 200, seed = 1)[
             , c("VarX", "VarY", "Claims", "Deviance", "DF", "Z", "P",
                 "MaxAE", "MaxAE_ExposureShare")],
           "detect-interactions")

# screen_features(): baseline without GEWICHT, plus a pure-noise column
dat$RUIS <- round(rnorm(nrow(dat)), 2)
m_base_scr <- glm(AantalClaims ~ ns(LEEFTIJD, 5) + REGIO +
                    offset(log(Exposure)), family = poisson(), data = dat)
scr <- suppressWarnings(suppressMessages(
  screen_features(m_base_scr, seed = 1, n_shap = 0,
                  features = c("LEEFTIJD", "REGIO", "GEWICHT",
                               "GEWICHT_PROXY", "BRANDSTOF", "RUIS"))))
snap(scr$plot, "screen-features", width = 760, height = 420)

# model_lift(): the Lorenz curve behind the Gini
snap(model_lift(m_freq, data = dat, actual_col = "AantalClaims")$plot_lorenz,
     "lorenz", width = 700, height = 460)

# premium_impact(): the spotlight overlay
imp_s <- premium_impact(dat, model_freq_new = m_freq, model_sev_new = m_sev,
                        old_premium_col = "HUIDIGE_PREMIE",
                        spotlight = REGIO == "Randstad" & LEEFTIJD < 25)
snap(imp_s$plot, "premium-impact-spotlight", width = 800)

# export_rating_table(): what the Overview sheet holds
ov <- data.frame(
  Item = c("Generated",
           "Intercept frequency (per exposure unit)",
           "Intercept severity", "Intercept premium",
           paste("Base value", names(attr(tbl, "base_values")))),
  Value = c(format(Sys.time(), "%Y-%m-%d %H:%M"),
            formatC(attr(tbl, "intercept_frequency"), format = "f", digits = 5),
            formatC(attr(tbl, "intercept_severity"), format = "f", digits = 1),
            formatC(attr(tbl, "intercept_premium"), format = "f", digits = 2),
            vapply(attr(tbl, "base_values"),
                   function(x) paste(format(x), collapse = ", "),
                   character(1))),
  stringsAsFactors = FALSE)
snap_table(ov, "export-overview", vwidth = 900)

# pricing_report(): the generated page itself
rep_file <- file.path(tempdir(), "figs", "report.html")
suppressMessages(pricing_report(
  m_freq, m_sev, dat, file = rep_file, title = "Motor portfolio – GLM pricing",
  variables = c("LEEFTIJD", "REGIO"),
  include = c("diagnostics", "oneway", "rating")))
snap_page(rep_file, "pricing-report")

cat("\nAll figures written to", out_dir, "\n")
