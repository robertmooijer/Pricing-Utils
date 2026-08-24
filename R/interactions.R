# Interaction detection -----------------------------------------------------
#
# One-way A/E checks cannot reveal a missing interaction.
#
# The score equations weight each observation by (dmu/deta)/V(mu), which is
# exactly 1 for a canonical link. What remains is sum(x*(y - mu)) = 0 for
# every column of the design matrix, so a categorical variable in the model
# has an A/E of exactly 1 at every level - whatever happens inside the
# cells. Poisson with a log link is canonical, so a frequency model is
# blind by identity.
#
# Gamma with a log link is NOT canonical (the canonical link for Gamma is
# the inverse), so a severity model is not blind by identity. It is blind
# in practice. The log link puts the score weight at mu/mu^2 = 1/mu, which
# pins the weighted mean of the RATIO y/mu to 1 rather than the ratio of
# the weighted totals: measured on 40,000 rows, sum(w*y/mu)/sum(w) sits at
# 3e-09 from 1 while the A/E itself sits at 1.2e-04. The residue that
# leaves in the A/E is far too small to work with - an omitted interaction
# of 35% moves the one-way A/E by 0.26%, invisible on an axis in the
# thousands.
#
# Either way, the two functions here look at the cells.

# Diverging colour scale for A/E, centred on 1.0: two hues (the house navy
# for overpriced, red for underpriced) with a neutral grey midpoint. Never
# a hue at the midpoint and never a rainbow, so the reader cannot mistake a
# colour change for a change in direction.
.ta_diverging <- list(
  c(0.00, "#00365E"), c(0.15, "#0073AB"), c(0.32, "#6FA3C7"),
  c(0.44, "#C9DCEA"), c(0.50, "#F2F2F1"), c(0.56, "#EFD3CE"),
  c(0.68, "#DC9E94"), c(0.85, "#C44536"), c(1.00, "#8E2A1F"))

#' Actual-vs-expected heatmap for a pair of variables
#'
#' Shows the exposure-weighted A/E ratio of a fitted GLM per cell of
#' `var_x` x `var_y`. This is the check that a one-way plot cannot do: a
#' missing interaction averages out in the margins while individual cells
#' can be badly mispriced. Exactly so for a frequency model - Poisson with
#' a log link is canonical, so the fitted total equals the observed total
#' at every level of a categorical term in the model - and near enough for
#' a Gamma severity, where an omitted interaction of 35% moves the one-way
#' A/E by a quarter of a percent.
#'
#' Both variables are grouped with the same rule used elsewhere in the
#' package: categorical as is, a numeric variable with at most `n_bins`
#' distinct values on its exact values, otherwise quantile bins.
#'
#' Cells backed by fewer than `min_claims` claims are left blank rather
#' than coloured, and marked with a grey cross: in a thin cell the A/E is
#' dominated by noise, and colouring it would be the loudest thing on the
#' plot. Their volume is still shown in the tooltip.
#'
#' @param model A fitted glm object (fitted with a `data =` argument).
#' @param var_x,var_y Names of the two variables. They must be columns of
#'   the data the model was fitted on; the values are taken from the
#'   model's own rows, so they always align with the fitted values.
#' @param n_bins Maximum number of groups per variable (default 20).
#' @param min_claims Cells with fewer claims are shown as blank (default
#'   30).
#' @param z_range Optional `c(lo, hi)` to fix the colour scale, so several
#'   heatmaps can be compared. `NULL` (default) scales to this plot, still
#'   centred on 1.
#' @param title Optional plot title.
#'
#' @return A plotly object. The underlying cell table (groups, actual,
#'   expected, A/E, exposure, claims, thin flag) is attached as the
#'   `"cells"` attribute.
#' @seealso [detect_interactions()] to rank all pairs before plotting one.
#' @export
plot_residual_heatmap <- function(model, var_x, var_y,
                                  n_bins = 20, min_claims = 30,
                                  z_range = NULL, title = NULL) {

  if (!inherits(model, "glm"))
    stop("plot_residual_heatmap: 'model' must be a glm object.", call. = FALSE)
  z_range <- .check_range(z_range, "plot_residual_heatmap", "z_range")

  parts <- .glm_ae_parts(model, "plot_residual_heatmap")
  gx <- .group_values(.glm_var_values(parts, var_x, "plot_residual_heatmap"),
                      n_bins)
  gy <- .group_values(.glm_var_values(parts, var_y, "plot_residual_heatmap"),
                      n_bins)

  keep <- !is.na(gx) & !is.na(gy)
  cells <- data.table::data.table(
    gx = gx[keep], gy = gy[keep],
    a = parts$actual[keep], e = parts$expected[keep],
    expo = parts$exposure[keep], clm = parts$claims[keep]
  )[, .(Actual = sum(a), Expected = sum(e),
        Exposure = sum(expo), Claims = sum(clm)), by = .(gx, gy)]
  cells[, AE := data.table::fifelse(Expected > 0, Actual / Expected, NA_real_)]
  cells[, IsThin := Claims < min_claims]
  cells <- as.data.frame(cells)

  lx <- levels(gx); ly <- levels(gy)
  ix <- match(as.character(cells$gx), lx)
  iy <- match(as.character(cells$gy), ly)

  z   <- matrix(NA_real_, length(ly), length(lx), dimnames = list(ly, lx))
  txt <- matrix(NA_character_, length(ly), length(lx))
  pos <- cbind(iy, ix)

  # Thin cells: no colour, only a marker and their volume in the tooltip
  z[pos] <- ifelse(cells$IsThin, NA_real_, cells$AE)
  txt[pos] <- paste0(
    "<b>", var_x, ":</b> ", cells$gx,
    "<br><b>", var_y, ":</b> ", cells$gy,
    "<br><b>A/E:</b> ", formatC(cells$AE, format = "f", digits = 3),
    "<br>Claims: ", format(round(cells$Claims), big.mark = ",", trim = TRUE),
    " \u00b7 ", parts$exposure_label, ": ",
    format(round(cells$Exposure), big.mark = ",", trim = TRUE),
    ifelse(cells$IsThin, "<br><i>too few claims - not coloured</i>", ""))

  p <- plot_ly() %>%
    add_trace(
      x = lx, y = ly, z = z, type = "heatmap",
      colorscale = .ta_diverging, zmid = 1,
      zmin = if (is.null(z_range)) NULL else z_range[1],
      zmax = if (is.null(z_range)) NULL else z_range[2],
      xgap = 2, ygap = 2,                       # surface gap between cells
      text = txt, hoverinfo = "text",
      colorbar = list(title = list(text = "A/E", side = "right"),
                      thickness = 12, outlinewidth = 0,
                      tickfont = list(size = 10, color = ta_muted)))

  if (any(cells$IsThin)) {
    th <- cells[cells$IsThin, , drop = FALSE]
    p <- p %>% add_trace(
      x = as.character(th$gx), y = as.character(th$gy),
      type = "scatter", mode = "markers", showlegend = FALSE,
      marker = list(symbol = "x", size = 7, color = ta_muted, opacity = 0.55),
      text = txt[cbind(match(as.character(th$gy), ly),
                       match(as.character(th$gx), lx))],
      hoverinfo = "text")
  }

  if (is.null(title))
    title <- paste0("A/E per cell \u2013 ", var_x, " \u00d7 ", var_y)

  p %>%
    layout(
      title = list(text = title, font = list(color = ta_navy, size = 14)),
      xaxis = list(title = var_x, tickangle = -45,
                   tickfont = list(size = 9), showgrid = FALSE,
                   type = "category", categoryorder = "array",
                   categoryarray = lx),
      yaxis = list(title = var_y, tickfont = list(size = 9), showgrid = FALSE,
                   type = "category", categoryorder = "array",
                   categoryarray = ly),
      hoverlabel = list(bgcolor = "white", font = list(size = 12)),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(b = 80, l = 110, r = 40)) %>%
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d"),
           toImageButtonOptions = list(format = "png", filename = "heatmap")) ->
    out

  attr(out, "cells") <- cells
  out
}

#' Rank variable pairs by unexplained interaction structure
#'
#' Scans every two-way combination of `vars` and ranks them by how much
#' cell-level lack of fit remains after the row and column margins have
#' been matched. The margins are matched by iterative proportional fitting,
#' so main-effect misfit (a spline that needs another knot, a level that is
#' simply mispriced) is scaled away first and what remains is genuine
#' interaction structure.
#'
#' The statistic is the likelihood-ratio deviance between the additive and
#' the saturated model on the two-way table of actual versus expected
#' claims. Because that deviance is unreliable in sparse tables, the
#' reference distribution is obtained by simulation rather than from the
#' chi-square asymptotics whenever the model is a Poisson count model:
#' claims are resampled from the raked expected values under the additive
#' null and the statistic is recomputed, `n_sim` times. For other models
#' the dispersion-scaled chi-square is used instead (reported in the
#' `Method` column).
#'
#' The ranking is by statistical signal (`Z`), which is not the same thing
#' as materiality: an effect can be highly significant and commercially
#' irrelevant, or the reverse. `MaxAE` and `MaxAE_ExposureShare` are
#' reported alongside so the two can be judged separately.
#'
#' @param model A fitted glm object (fitted with a `data =` argument).
#' @param vars Variables to scan. `NULL` (default) uses the model's own
#'   predictors (excluding the offset). Variables that are *not* in the
#'   model may be passed too, which is how an entirely omitted rating
#'   factor shows up.
#' @param n_bins Maximum number of groups per variable (default 10).
#' @param min_claims Cells with fewer claims are ignored when determining
#'   `MaxAE` (default 30).
#' @param n_sim Simulations for the reference distribution (default 200).
#' @param top_n Return only the strongest `top_n` pairs (default all).
#' @param seed Optional seed, for a reproducible reference distribution.
#'
#' @return A data.frame, strongest first, with one row per pair: `VarX`,
#'   `VarY`, `Cells`, `Claims`, `Deviance`, `DF`, `Z`, `P`, `Method`,
#'   `MaxAE` (A/E of the worst non-thin cell), `MaxAE_Claims` and
#'   `MaxAE_ExposureShare`.
#' @seealso [plot_residual_heatmap()] to inspect a pair.
#' @export
detect_interactions <- function(model, vars = NULL, n_bins = 10,
                                min_claims = 30, n_sim = 200,
                                top_n = NULL, seed = NULL) {

  if (!inherits(model, "glm"))
    stop("detect_interactions: 'model' must be a glm object.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)

  parts <- .glm_ae_parts(model, "detect_interactions")
  pool  <- if (!is.null(parts$data)) parts$data else parts$mf

  if (is.null(vars)) {
    vars <- setdiff(.model_base_vars(model, pool), .offset_vars(model))
    vars <- intersect(vars, names(pool))
  }
  vars <- unique(setdiff(vars, .offset_vars(model)))
  if (length(vars) < 2)
    stop("detect_interactions: need at least two variables to pair up.",
         call. = FALSE)

  grp <- lapply(vars, function(v)
    .group_values(.glm_var_values(parts, v, "detect_interactions"), n_bins))
  names(grp) <- vars

  # Match E to A's row and column margins (iterative proportional fitting).
  # What is left is the interaction, not the main effects.
  rake <- function(A, E, iter = 50, tol = 1e-9) {
    for (k in seq_len(iter)) {
      r <- rowSums(A) / rowSums(E); r[!is.finite(r)] <- 1
      E <- E * r
      cs <- colSums(A) / colSums(E); cs[!is.finite(cs)] <- 1
      E <- sweep(E, 2, cs, "*")
      if (max(abs(rowSums(E) - rowSums(A))) < tol) break
    }
    E
  }
  dev_of <- function(A, E) {
    ok <- E > 0
    2 * sum(ifelse(A[ok] > 0, A[ok] * log(A[ok] / E[ok]), 0) - (A[ok] - E[ok]))
  }

  # The resampling null needs the aggregated actuals to be Poisson counts.
  # That depends on the family and the response, not on whether the model
  # carries an exposure offset.
  fam  <- family(model)$family
  boot <- isTRUE(parts$poisson_counts)
  phi  <- sum(residuals(model, type = "pearson")^2) / model$df.residual
  if (!boot)
    message("detect_interactions: the response is not Poisson counts (",
            fam, "), so the reference distribution comes from the ",
            "dispersion-scaled chi-square rather than simulation; it is ",
            "less reliable in sparse tables.")

  pairs <- utils::combn(vars, 2, simplify = FALSE)
  rows  <- lapply(pairs, function(pr) tryCatch({
    gx <- grp[[pr[1]]]; gy <- grp[[pr[2]]]
    keep <- !is.na(gx) & !is.na(gy)
    A <- tapply(parts$actual[keep],   list(gx[keep], gy[keep]), sum)
    E <- tapply(parts$expected[keep], list(gx[keep], gy[keep]), sum)
    W <- tapply(parts$exposure[keep], list(gx[keep], gy[keep]), sum)
    C <- tapply(parts$claims[keep],   list(gx[keep], gy[keep]), sum)
    A[is.na(A)] <- 0; E[is.na(E)] <- 0; W[is.na(W)] <- 0; C[is.na(C)] <- 0
    if (nrow(A) < 2 || ncol(A) < 2) return(NULL)

    Er  <- rake(A, E)
    D   <- dev_of(A, Er)
    dfr <- (sum(rowSums(A) > 0) - 1) * (sum(colSums(A) > 0) - 1)
    if (dfr < 1) return(NULL)

    if (boot) {
      sim <- vapply(seq_len(n_sim), function(i) {
        As <- matrix(stats::rpois(length(Er), Er), nrow(Er), ncol(Er))
        dev_of(As, rake(As, Er))
      }, numeric(1))
      z <- (D - mean(sim)) / stats::sd(sim)
      p <- (1 + sum(sim >= D)) / (n_sim + 1)
      meth <- "simulation"
    } else {
      z <- (D / phi - dfr) / sqrt(2 * dfr)
      p <- stats::pchisq(D / phi, dfr, lower.tail = FALSE)
      meth <- "chisq"
    }

    ae   <- ifelse(E > 0, A / E, NA_real_)
    ae[C < min_claims] <- NA_real_
    worst <- if (all(is.na(ae))) NA_integer_ else which.max(abs(ae - 1))

    data.frame(
      VarX = pr[1], VarY = pr[2],
      Cells = sum(C > 0), Claims = sum(C),
      Deviance = D, DF = dfr, Z = z, P = p, Method = meth,
      MaxAE = if (is.na(worst)) NA_real_ else ae[worst],
      MaxAE_Claims = if (is.na(worst)) NA_real_ else C[worst],
      MaxAE_ExposureShare = if (is.na(worst)) NA_real_ else W[worst] / sum(W),
      stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("detect_interactions: pair ", paste(pr, collapse = " x "),
            " skipped: ", conditionMessage(e), call. = FALSE)
    NULL
  }))

  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out) || !nrow(out))
    stop("detect_interactions: no usable pairs found.", call. = FALSE)

  out <- out[order(-out$Z), ]
  rownames(out) <- NULL
  if (!is.null(top_n)) out <- utils::head(out, top_n)
  out
}
