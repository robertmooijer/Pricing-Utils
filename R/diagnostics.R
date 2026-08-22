#' Compact fit summary for a frequency and/or severity model
#'
#' Flags overdispersion for Poisson/binomial families (Pearson dispersion
#' above 1.2), in which case standard errors are understated and a quasi-
#' or negative binomial family should be considered.
#'
#' @param model_freq,model_sev Fitted glm objects (at least one).
#'
#' @return A data.frame with one row per model: `Model`, `Family`, `Link`,
#'   `N`, `Deviance`, `DFResidual`, `AIC`, `Dispersion` (Pearson chi-squared
#'   over df) and `DevianceExplained` (1 - deviance / null deviance).
#' @export
glm_diagnostics <- function(model_freq = NULL, model_sev = NULL) {

  if (is.null(model_freq) && is.null(model_sev)) {
    stop("Provide at least one model.")
  }

  one <- function(model, model_name) {
    if (is.null(model)) return(NULL)
    if (!inherits(model, "glm"))
      stop("glm_diagnostics: '", model_name, "' must be a glm object.",
           call. = FALSE)
    fam  <- family(model)
    disp <- sum(residuals(model, type = "pearson")^2) / model$df.residual

    if (fam$family %in% c("poisson", "binomial") && disp > 1.2)
      warning("glm_diagnostics: model '", model_name, "' shows ",
              "overdispersion (Pearson dispersion = ", round(disp, 2),
              " > 1); standard errors are understated. Consider a quasi-",
              fam$family, " or negative binomial family.", call. = FALSE)

    data.frame(
      Model             = model_name,
      Family            = fam$family,
      Link              = fam$link,
      N                 = stats::nobs(model),
      Deviance          = model$deviance,
      DFResidual        = model$df.residual,
      AIC               = suppressWarnings(
                            tryCatch(stats::AIC(model),
                                     error = function(e) NA_real_)),
      Dispersion        = disp,
      DevianceExplained = 1 - model$deviance / model$null.deviance,
      stringsAsFactors  = FALSE
    )
  }

  out <- rbind(one(model_freq, "frequency"), one(model_sev, "severity"))
  rownames(out) <- NULL
  out
}

#' Collinearity between model terms
#'
#' Reports the generalised variance inflation factor per **term**, not per
#' coefficient. A plain VIF is meaningless for a pricing model, where a
#' spline or a factor spans several columns: the generalised form of Fox
#' and Monette handles multi-column terms, and `GVIF^(1/(2*DF))` puts them
#' back on a scale comparable across terms.
#'
#' Read `GVIF_scaled` like a plain VIF on the square-root scale: the
#' default threshold of 3 corresponds to a VIF of about 9. A high value
#' means that term is largely explained by the others, so its coefficient
#' is unstable and its rating factors are not interpretable on their own,
#' even though the model's overall predictions may be perfectly fine.
#'
#' This is the model-level counterpart of the near-duplicate warning in
#' [screen_features()]: that one catches correlated *candidates* before
#' they go in, this one catches them once they are both in the model.
#'
#' @param model A fitted glm object with at least two terms.
#' @param threshold Flag terms whose `GVIF_scaled` reaches this value
#'   (default 3).
#'
#' @return A data.frame with `Term`, `DF`, `GVIF`, `GVIF_scaled` and
#'   `Flag`, ordered by `GVIF_scaled`. Returns zero rows with a message
#'   when the model has fewer than two terms.
#' @seealso [glm_diagnostics()]
#' @export
glm_collinearity <- function(model, threshold = 3) {

  if (!inherits(model, "glm"))
    stop("glm_collinearity: 'model' must be a glm object.", call. = FALSE)

  labs <- attr(terms(model), "term.labels")
  if (length(labs) < 2) {
    message("glm_collinearity: fewer than two terms, nothing to compare.")
    return(data.frame(Term = character(0), DF = integer(0),
                      GVIF = numeric(0), GVIF_scaled = numeric(0),
                      Flag = logical(0), stringsAsFactors = FALSE))
  }

  v  <- stats::vcov(model)
  as_ <- attr(stats::model.matrix(model), "assign")
  nm <- colnames(stats::model.matrix(model))
  # Aliased coefficients are dropped from vcov, so line the two up by name
  keep <- nm %in% rownames(v) & as_ != 0
  as_  <- as_[keep]
  v    <- v[nm[keep], nm[keep], drop = FALSE]
  if (!length(as_) || length(unique(as_)) < 2) {
    message("glm_collinearity: fewer than two estimable terms.")
    return(data.frame(Term = character(0), DF = integer(0),
                      GVIF = numeric(0), GVIF_scaled = numeric(0),
                      Flag = logical(0), stringsAsFactors = FALSE))
  }

  R    <- stats::cov2cor(v)
  detR <- det(R)
  if (!is.finite(detR) || detR <= 0) {
    warning("glm_collinearity: the coefficient correlation matrix is ",
            "singular, which is itself a sign of exact collinearity; ",
            "GVIF cannot be computed.", call. = FALSE)
    return(data.frame(Term = character(0), DF = integer(0),
                      GVIF = numeric(0), GVIF_scaled = numeric(0),
                      Flag = logical(0), stringsAsFactors = FALSE))
  }

  ids <- sort(unique(as_))
  out <- do.call(rbind, lapply(ids, function(i) {
    sel  <- as_ == i
    gvif <- det(R[sel, sel, drop = FALSE]) *
            det(R[!sel, !sel, drop = FALSE]) / detR
    dfi  <- sum(sel)
    data.frame(Term = labs[i], DF = dfi, GVIF = gvif,
               GVIF_scaled = gvif^(1 / (2 * dfi)),
               stringsAsFactors = FALSE)
  }))
  out$Flag <- out$GVIF_scaled >= threshold
  out <- out[order(-out$GVIF_scaled), ]
  rownames(out) <- NULL

  if (any(out$Flag))
    warning("glm_collinearity: ", sum(out$Flag), " term(s) at or above a ",
            "scaled GVIF of ", threshold, " (", paste(out$Term[out$Flag],
            collapse = ", "), "); their factors are unstable and should ",
            "not be read individually.", call. = FALSE)
  out
}

#' Binned residual plot
#'
#' For individual policy rows raw residual plots are unreadable (Poisson
#' residuals are dominated by the 0/1 claim pattern). This plots the mean
#' residual per (quantile) bin of the fitted value, or of a predictor,
#' together with a plus/minus 2 SE band: under a correctly specified model
#' roughly 95 percent of the bin means should fall inside the band.
#' Systematic patterns outside the band indicate missed structure
#' (candidate terms/splines).
#'
#' @param model A fitted glm object.
#' @param predictor Optional: bin by this predictor instead of the fitted
#'   values.
#' @param n_bins Number of quantile bins for numeric x (default 50).
#' @param residual_type `"pearson"` (default) or `"deviance"`.
#' @param y_range Optional `c(lo, hi)` to fix the residual (y) axis range,
#'   so residual plots for different predictors or models become directly
#'   comparable. `NULL` (default) auto-scales to this plot's own values.
#'
#' @return A plotly object.
#' @export
plot_glm_residuals <- function(model, predictor = NULL, n_bins = 50,
                               residual_type = c("pearson", "deviance"),
                               y_range = NULL) {

  residual_type <- match.arg(residual_type)
  y_range <- .check_range(y_range, "plot_glm_residuals")
  if (!inherits(model, "glm")) stop("'model' must be a glm object.")
  if (n_bins < 2) stop("plot_glm_residuals: 'n_bins' must be at least 2.")

  r    <- as.numeric(residuals(model, type = residual_type))
  disp <- sum(residuals(model, type = "pearson")^2) / model$df.residual

  if (is.null(predictor)) {
    x    <- as.numeric(fitted(model))
    xlab <- "Fitted value"
  } else {
    tr <- .glm_training_data(model, "plot_glm_residuals")
    if (predictor %in% names(tr$mf)) {
      x <- tr$mf[[predictor]]
    } else if (!is.null(tr$data) && predictor %in% names(tr$data)) {
      x <- tr$data[[predictor]]
    } else {
      stop(paste0("Predictor '", predictor, "' not found."))
    }
    xlab <- predictor
  }

  df <- data.frame(x = x, r = r)
  x_numeric <- is.numeric(df$x)
  if (x_numeric && length(unique(df$x)) > n_bins) {
    brks <- unique(stats::quantile(df$x,
                                   probs = seq(0, 1, length.out = n_bins + 1),
                                   na.rm = TRUE, names = FALSE))
    if (length(brks) < 2) brks <- range(df$x, na.rm = TRUE) + c(-0.5, 0.5)
    df$bin_group <- cut(df$x, breaks = brks, include.lowest = TRUE, dig.lab = 4)
  } else {
    df$bin_group <- factor(df$x)
  }

  agg <- df %>%
    group_by(bin_group) %>%
    summarise(
      x_plot = if (x_numeric) mean(x) else dplyr::first(as.character(x)),
      mean_r = mean(r),
      n      = n(),
      .groups = "drop"
    ) %>%
    mutate(band = 2 * sqrt(disp / n))
  agg <- if (x_numeric) agg %>% arrange(x_plot) else agg %>% arrange(bin_group)

  ht_mean <- paste0("<b>", xlab, ":</b> %{x}",
                    "<br><b>Mean residual:</b> %{y:.3f}",
                    "<br>%{text}<extra></extra>")

  xaxis_cfg <- list(title = xlab, tickangle = -45,
                    tickfont = list(size = 9), showgrid = FALSE)
  if (!x_numeric) {
    xaxis_cfg$categoryorder <- "array"
    xaxis_cfg$categoryarray <- agg$x_plot
  }

  p <- plot_ly()

  if (x_numeric) {
    # Plus/minus 2 SE band as a shaded ribbon
    p <- p %>%
      add_trace(data = agg, x = ~x_plot, y = ~band,
                type = "scatter", mode = "lines",
                line = list(color = "rgba(107,122,141,0.4)", width = 1,
                            dash = "dot"),
                name = "\u00b12\u00b7SE", legendgroup = "band",
                showlegend = TRUE, hoverinfo = "skip") %>%
      add_trace(data = agg, x = ~x_plot, y = ~-band,
                type = "scatter", mode = "lines",
                line = list(color = "rgba(107,122,141,0.4)", width = 1,
                            dash = "dot"),
                fill = "tonexty", fillcolor = "rgba(168,200,224,0.25)",
                name = "\u00b12\u00b7SE", legendgroup = "band",
                showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(data = agg, x = ~x_plot, y = ~mean_r,
                type = "scatter", mode = "lines+markers",
                name = "Mean residual",
                line   = list(color = ta_blue, width = 1.6),
                marker = list(color = ta_blue, size = 6),
                text = ~paste0("n = ", n), hovertemplate = ht_mean)
  } else {
    # Categorical x: error bars instead of a ribbon
    p <- p %>%
      add_trace(data = agg, x = ~x_plot, y = ~mean_r,
                type = "scatter", mode = "markers",
                name = "Mean residual",
                marker  = list(color = ta_blue, size = 8),
                error_y = list(type = "data", array = agg$band,
                               color = ta_muted, thickness = 1),
                text = ~paste0("n = ", n), hovertemplate = ht_mean)
  }

  p %>%
    layout(
      title  = list(text = paste0("Binned ", residual_type, " residuals \u2013 ",
                                  xlab),
                    font = list(color = ta_navy, size = 14)),
      xaxis  = xaxis_cfg,
      yaxis  = list(title = paste("Mean", residual_type, "residual"),
                    gridcolor = "#D0D8E0", zeroline = FALSE,
                    range = y_range, autorange = is.null(y_range)),
      shapes = list(list(type = "line", xref = "paper",
                         x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                         line = list(color = ta_muted, width = 1,
                                     dash = "dot"))),
      legend       = list(orientation = "h", y = -0.2),
      hoverlabel   = list(bgcolor = "white", font = list(size = 12)),
      plot_bgcolor = "white",
      paper_bgcolor= "white",
      margin       = list(b = 80, r = 40)
    ) %>%
    config(
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      toImageButtonOptions   = list(format = "png", filename = "residuals")
    )
}
