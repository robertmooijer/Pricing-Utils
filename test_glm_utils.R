# Smoke/validation tests for GLM UTILS.R
# Run with:  Rscript test_glm_utils.R
suppressWarnings(suppressMessages(source("GLM UTILS.R")))

set.seed(42)
n <- 20000
dat <- data.frame(
  LEEFTIJD  = round(runif(n, 18, 80)),
  REGIO     = factor(sample(c("Noord","Zuid","West"), n, TRUE, c(.5,.3,.2))),
  BOEKJAAR  = sample(2019:2023, n, TRUE),
  Exposure  = runif(n, 0.1, 1)
)
lin <- -2.3 + 0.015*(dat$LEEFTIJD-40) + ifelse(dat$REGIO=="Zuid", .25,
        ifelse(dat$REGIO=="West", .4, 0))
dat$AantalClaims <- rpois(n, dat$Exposure * exp(lin))
dat$SCHADELAST   <- ifelse(dat$AantalClaims > 0,
  rgamma(n, shape = 2, rate = 2/ (1500 * exp(0.01*(dat$LEEFTIJD-40)))) * dat$AantalClaims, 0)

m_freq <- glm(AantalClaims ~ ns(LEEFTIJD, 3) + REGIO + factor(BOEKJAAR) +
                offset(log(Exposure)),
              family = poisson(), data = dat)
d_sev <- dat[dat$AantalClaims > 0, ]
d_sev$AvgLoss <- d_sev$SCHADELAST / d_sev$AantalClaims
m_sev <- glm(AvgLoss ~ LEEFTIJD + REGIO, family = Gamma(link = "log"),
             data = d_sev, weights = AantalClaims)

ok <- function(name, cond) cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "OK " else "FAIL", name))

# 1. Rating table builds without crashing, incl. inline factor(BOEKJAAR)
tbl <- make_rating_table(m_freq, m_sev, data = dat)
ok("rating table built (incl. factor(BOEKJAAR))",
   all(c("LEEFTIJD","REGIO","BOEKJAAR") %in% tbl$Variable))
ok("BOEKJAAR recognised as categorical",
   all(tbl$Type[tbl$Variable == "BOEKJAAR"] == "categorical"))

# 2. IsBase rows have factor exactly 1
b <- tbl[tbl$IsBase & is.na(tbl$Group), ]
ok("every variable has exactly 1 base row",
   nrow(b) == length(unique(tbl$Variable[is.na(tbl$Group)])))
ok("factors on base row == 1",
   all(abs(c(b$Factor_Frequency, b$Factor_Severity) - 1) < 1e-10, na.rm = TRUE))

# 3. Reconstruction: intercept x factors == model prediction (exposure = 1)
# at an EXACT grid point of the age curve (no interpolation)
i_f <- attr(tbl, "intercept_frequency")
s_age <- tbl[tbl$Variable == "LEEFTIJD" & is.na(tbl$Group), ]
age   <- s_age$LevelNum[35]           # arbitrary grid point
test_row <- data.frame(LEEFTIJD = age, REGIO = factor("Zuid", levels(dat$REGIO)),
                       BOEKJAAR = 2022, Exposure = 1)
pred_direct <- predict(m_freq, newdata = test_row, type = "response")
fac <- function(v, lvl) {
  s <- tbl[tbl$Variable == v & is.na(tbl$Group), ]
  s$Factor_Frequency[match(as.character(lvl), s$Level)]
}
recon <- i_f * s_age$Factor_Frequency[35] * fac("REGIO","Zuid") * fac("BOEKJAAR","2022")
ok(sprintf("premium reconstruction (direct %.8f vs table %.8f)", pred_direct, recon),
   abs(pred_direct - recon) < 1e-10)

# 4. Intercept is per unit of exposure (exposure = 1)
base_pred <- predict(m_freq, newdata = data.frame(
  LEEFTIJD = median(dat$LEEFTIJD), REGIO = factor("Noord", levels(dat$REGIO)),
  BOEKJAAR = 2019, Exposure = 1), type = "response")
ok("intercept_frequency at exposure = 1", abs(i_f - base_pred) < 1e-10)

# 5. Interaction model: uplift columns and base cell
m_freq2 <- glm(AantalClaims ~ LEEFTIJD * REGIO + offset(log(Exposure)),
               family = poisson(), data = dat)
tbl2 <- make_rating_table(m_freq2, NULL, data = dat)
d_int <- tbl2[tbl2$Variable == "LEEFTIJD:REGIO", ]
ok("interaction rows present", nrow(d_int) > 0)
ok("interaction base cell: joint factor == 1",
   abs(d_int$Factor_Frequency[d_int$IsBase] - 1) < 1e-10)
# uplift on the reference REGIO must be 1 (no interaction effect at reference)
u_ref <- d_int$Uplift_Frequency[d_int$Group == "Noord"]
ok("uplift == 1 at reference group", all(abs(u_ref - 1) < 1e-8))

# 6. Non-log link produces a warning
w <- tryCatch({ make_rating_table(NULL,
       glm(AvgLoss ~ REGIO, family = Gamma(link = "inverse"),
           data = d_sev, weights = AantalClaims), data = dat); "none" },
     warning = function(w) conditionMessage(w))
ok("warning for non-log link", grepl("log link", w))

# 7. make_pdp: response scale + offset neutralisation
p_pdp <- make_pdp(m_freq, dat, "REGIO", metric = "Frequency")
ok("make_pdp Frequency returns plotly object", inherits(p_pdp, "plotly"))
# manual reference: exposure-weighted mean response prediction at Exposure = 1
nd <- dat; nd$Exposure <- 1; nd$REGIO <- factor("Zuid", levels(dat$REGIO))
ref_zuid <- weighted.mean(predict(m_freq, newdata = nd, type = "response"),
                          exp(m_freq$offset))
pd_dat <- plotly::plotly_build(p_pdp)$x$data
pdp_trace <- Filter(function(t) identical(t$name, "PDP (model)"), pd_dat)[[1]]
pdp_zuid <- unlist(pdp_trace$y)[match("Zuid", unlist(pdp_trace$x))]
ok(sprintf("PDP = exposure-weighted mean at exposure 1 (%.5f vs %.5f)",
           pdp_zuid, ref_zuid), abs(pdp_zuid - ref_zuid) < 1e-8)
# PDP level must be of the same order as the portfolio frequency
obs_tot <- sum(dat$AantalClaims)/sum(dat$Exposure)
ok(sprintf("PDP level plausible vs portfolio frequency %.4f", obs_tot),
   all(abs(unlist(pdp_trace$y)/obs_tot - 1) < 1))

p_pdp_sev <- make_pdp(m_sev, dat, "LEEFTIJD", metric = "Severity")
ok("make_pdp Severity returns plotly object", inherits(p_pdp_sev, "plotly"))
tw <- tryCatch({ make_pdp(m_freq, dat, "REGIO", transform = exp); "none" },
               warning = function(w) conditionMessage(w))
ok("'transform' gives deprecation warning", grepl("deprecated", tw))

# 8. agg_all with custom column names + validation
dat2 <- dat; names(dat2)[names(dat2) == "Exposure"] <- "EXPO"
a <- agg_all(dat2, "REGIO", by_year = TRUE, exposure_col = "EXPO")
ok("agg_all with custom exposure column",
   all(c("Exposure","ClaimCount","Loss","Frequency","Severity","Year") %in% names(a)))
e <- tryCatch({ agg_all(dat, "REGIO", FALSE, exposure_col = "DOES_NOT_EXIST"); "none" },
              error = function(e) conditionMessage(e))
ok("agg_all: clear error for missing column", grepl("DOES_NOT_EXIST", e))

# 9. plot_glm_predictor (quantile bins) and make_rating_plot
ok("plot_glm_predictor", inherits(plot_glm_predictor(m_freq, "LEEFTIJD", n_bins = 30), "plotly"))
ok("plot_glm_predictor severity", inherits(plot_glm_predictor(m_sev, "LEEFTIJD", n_bins = 20), "plotly"))
ok("make_rating_plot main effect", inherits(make_rating_plot(tbl, "REGIO"), "plotly"))
ok("make_rating_plot interaction", inherits(make_rating_plot(tbl2, "LEEFTIJD:REGIO"), "plotly"))

# 10. make_plot on raw data (aggregation happens inside make_plot)
p_raw <- make_plot(dat, "REGIO", "Frequency", ta_blue, "Frequency", "color", TRUE)
ok("make_plot color", inherits(p_raw, "plotly"))
ok("make_plot facet", inherits(make_plot(dat, "REGIO", "Frequency", ta_blue,
                                         "Frequency", "facet", TRUE), "plotly"))
ok("make_plot without year split",
   inherits(make_plot(dat, "REGIO", "Severity", ta_gold, "Severity", "color", FALSE), "plotly"))
# plotted values must equal the agg_all aggregation
trace_y <- function(p, nm) {
  tr <- Filter(function(t) identical(t$name, nm), plotly::plotly_build(p)$x$data)[[1]]
  as.numeric(unlist(tr$y))
}
ovz <- agg_all(dat, "REGIO", by_year = TRUE)
s <- ovz[ovz$Year == "2020", ]; s <- s[order(s$REGIO), ]
ok("make_plot values match agg_all (year 2020)",
   isTRUE(all.equal(trace_y(p_raw, "2020"), s$Frequency)))
# invalid metric gives a clear match.arg error
e2 <- tryCatch({ make_plot(dat, "REGIO", "Frequentie", ta_blue, "y", "color", FALSE); "none" },
               error = function(e) conditionMessage(e))
ok("make_plot: error for invalid metric", grepl("Frequency", e2))

# 11. base_level = "exposure": reference = level with the largest exposure
tbl3 <- make_rating_table(m_freq, NULL, data = dat, base_level = "exposure")
largest <- names(which.max(tapply(dat$Exposure, dat$REGIO, sum)))
b3 <- tbl3[tbl3$Variable == "REGIO" & tbl3$IsBase, "Level"]
ok(sprintf("base_level='exposure' picks '%s'", largest), identical(b3, largest))

# 12. glm_diagnostics
diag <- glm_diagnostics(m_freq, m_sev)
ok("glm_diagnostics returns 2 rows with key columns",
   nrow(diag) == 2 && all(c("Dispersion","AIC","DevianceExplained") %in% names(diag)))
ok("dispersion ~ 1 for well-specified Poisson",
   abs(diag$Dispersion[diag$Model == "frequency"] - 1) < 0.1)
# overdispersed data (negative binomial) fitted as Poisson -> warning
# (small size parameter: Pearson dispersion ~ 1 + mu/size >> 1.2)
dat$AC_od <- rnbinom(n, mu = dat$Exposure * exp(lin), size = 0.05)
m_od <- glm(AC_od ~ REGIO + offset(log(Exposure)), family = poisson(), data = dat)
w_od <- tryCatch({ glm_diagnostics(m_od); "none" },
                 warning = function(w) conditionMessage(w))
ok("overdispersion warning", grepl("overdispersion", w_od))

# 13. plot_glm_residuals
p_res <- plot_glm_residuals(m_freq)
ok("plot_glm_residuals vs fitted", inherits(p_res, "plotly"))
ok("plot_glm_residuals vs numeric predictor",
   inherits(plot_glm_residuals(m_freq, "LEEFTIJD"), "plotly"))
ok("plot_glm_residuals vs categorical predictor",
   inherits(plot_glm_residuals(m_freq, "REGIO"), "plotly"))
# for a correct model most binned means must lie inside the ±2·SE band
bt <- plotly::plotly_build(p_res)$x$data
mr <- Filter(function(t) identical(t$name, "Mean residual"), bt)[[1]]
bd <- Filter(function(t) identical(t$name, "±2·SE"), bt)[[1]]
ok("binned residuals mostly within the band",
   mean(abs(as.numeric(unlist(mr$y))) <= as.numeric(unlist(bd$y))) > 0.7)

# 14. credibility / thin cells in the rating table
cc <- tbl$ClaimCount[!is.na(tbl$ClaimCount)]
ok("Credibility = min(1, sqrt(claims/1082))",
   isTRUE(all.equal(tbl$Credibility[!is.na(tbl$ClaimCount)],
                    pmin(1, sqrt(cc / 1082)))))
ok("IsThin consistent with min_claims = 30 default",
   identical(tbl$IsThin[!is.na(tbl$ClaimCount)], cc < 30))
tbl_thin <- suppressWarnings(make_rating_table(m_freq, NULL, data = dat,
                                               min_claims = 1e6))
ok("IsThin TRUE everywhere with a huge threshold",
   all(tbl_thin$IsThin[!is.na(tbl_thin$IsThin)]))
w_thin <- tryCatch({ make_rating_table(m_freq, NULL, data = dat,
                                       min_claims = 1e6); "none" },
                   warning = function(w) conditionMessage(w))
ok("thin-cell warning mentions the threshold", grepl("little standalone", w_thin))
ok("make_rating_plot with thin flags still works",
   inherits(make_rating_plot(tbl_thin, "REGIO"), "plotly"))

# 15. premium_impact
imp0 <- premium_impact(dat, model_freq_new = m_freq, model_sev_new = m_sev,
                       model_freq_old = m_freq, model_sev_old = m_sev,
                       by = "REGIO")
ok("premium_impact: zero dislocation for identical models",
   max(abs(imp0$policy$ChangePct)) < 1e-8)
ok("premium_impact: by-level table has 3 REGIO rows",
   nrow(imp0$by_level) == 3)
ok("premium_impact: histogram is a plotly object", inherits(imp0$plot, "plotly"))
# old premium column = 2x the new rate -> rate-level change -50%, rebased dislocation 0
new_rate <- predict(m_freq, newdata = transform(dat, Exposure = 1),
                    type = "response") *
            predict(m_sev, newdata = dat, type = "response")
dat$OldPrem <- 2 * new_rate * dat$Exposure
imp2 <- premium_impact(dat, model_freq_new = m_freq, model_sev_new = m_sev,
                       old_premium_col = "OldPrem")
ok("premium_impact: overall rate-level change -50%",
   abs(imp2$stats$rate_change + 0.5) < 1e-10)
ok("premium_impact: rebased dislocation ~0",
   max(abs(imp2$policy$ChangePct)) < 1e-8)
ok("premium_impact: winners/losers respect n_show",
   nrow(imp2$largest_increases) == 10 && nrow(imp2$largest_decreases) == 10)

# 16. export_rating_table
f_x <- file.path(tempdir(), "rating.xlsx")
export_rating_table(tbl, f_x)
ok("export_rating_table writes the file", file.exists(f_x))
sn <- openxlsx::getSheetNames(f_x)
ok("workbook has Overview + variable sheets",
   all(c("Overview", "REGIO", "LEEFTIJD", "BOEKJAAR") %in% sn))
f_x2 <- file.path(tempdir(), "rating_int.xlsx")
export_rating_table(tbl2, f_x2)
ok("interaction sheet present (sanitised name)",
   any(grepl("LEEFTIJD.REGIO", openxlsx::getSheetNames(f_x2))))

# 17. pricing_report
f_h <- file.path(tempdir(), "report.html")
suppressMessages(pricing_report(m_freq, m_sev, dat, file = f_h, grid_res = 20))
ok("pricing_report writes the file",
   file.exists(f_h) && file.info(f_h)$size > 50000)
html <- readChar(f_h, file.info(f_h)$size, useBytes = TRUE)
ok("report contains diagnostics and variable sections",
   grepl("Model diagnostics", html) && grepl("var-REGIO", html) &&
   grepl("Rating factors", html))
ok("report has plotly widgets", grepl("htmlwidget", html))
