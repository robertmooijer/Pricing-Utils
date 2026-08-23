# Feature screening with a boosted challenger --------------------------------
#
# The booster never becomes a pricing model here. It is fitted with the GLM
# as an offset, so it can only model what the GLM leaves behind, and every
# number it returns is a diagnostic about the GLM.

# Deviance under the model's own family, so this works for Poisson counts
# and Gamma severity alike
.fam_deviance <- function(fam, y, mu, wt) sum(fam$dev.resids(y, mu, wt))

# Level map, taken once on the full data. Deriving levels per split would
# give a character column a different width in each split.
.screen_levels <- function(d, features) {
  stats::setNames(lapply(features, function(f) {
    x <- d[[f]]
    if (is.character(x) || is.factor(x)) levels(factor(x)) else NULL
  }), features)
}

# Design matrix for xgboost: numerics as is, factors one-hot. The
# indicators are built directly rather than through model.matrix(), which
# refuses a factor with a single level ("contrasts can be applied only to
# factors with 2 or more levels") even when no contrasts are needed.
.screen_matrix <- function(d, features, levs) {
  blocks <- lapply(features, function(f) {
    x  <- d[[f]]
    lv <- levs[[f]]
    if (!is.null(lv)) {
      idx <- as.integer(factor(as.character(x), levels = lv))
      m <- matrix(0, nrow(d), length(lv),
                  dimnames = list(NULL, paste0(f, lv)))
      ok <- !is.na(idx)
      if (any(ok)) m[cbind(which(ok), idx[ok])] <- 1
      m
    } else {
      if (is.logical(x)) x <- as.numeric(x)
      matrix(as.numeric(x), ncol = 1, dimnames = list(NULL, f))
    }
  })
  list(x = do.call(cbind, blocks),
       owner = rep(features, vapply(blocks, ncol, integer(1))))
}

#' Screen candidate features against a baseline GLM
#'
#' Fits a gradient boosting model with the GLM as an offset, so the booster
#' can only explain what the GLM leaves behind. Everything it returns is
#' therefore a statement about the GLM, not a pricing model: no scorable
#' object is returned.
#'
#' It answers two questions, depending on the baseline you pass:
#' with a minimal `model` it screens candidate features *before* you choose
#' what goes into the tariff ("what does this data add on top of what I
#' already price on"); with your full model it asks what the finished
#' tariff still misses.
#'
#' @details
#' # Reading the output
#'
#' `summary` compares out-of-sample deviance for the baseline, a depth-1
#' booster (additive corrections only) and a depth-`max_depth` booster
#' (which may use interactions). The verdict is deliberately staged: while
#' the depth-1 step still improves the fit, main effects are missing and
#' the interaction ranking cannot be trusted yet. Fix the main effects,
#' then run again.
#'
#' `features` is sorted by `PermDeviance`: the increase in out-of-sample
#' deviance when that feature is shuffled, with the baseline held fixed, so
#' it measures the feature's *incremental* contribution. A value at or
#' below zero means no usable signal. `Gain` is reported alongside but is
#' unreliable on its own: it is biased towards continuous and
#' high-cardinality features, which routinely gives pure noise a
#' respectable-looking share.
#'
#' Correlated candidates split their importance between them, so a pair of
#' near-duplicates can both look mediocre. Pairs above `cor_threshold` are
#' reported in `correlated`.
#'
#' # Sensitivity
#'
#' A booster is *less* sensitive to a two-way interaction between known
#' rating factors than [detect_interactions()] is: the cell-based test
#' concentrates the signal into one statistic with a known null, while the
#' booster has to discover the split structure and pays a variance cost for
#' its flexibility. Use this function to screen features and to get the
#' global verdict; use [detect_interactions()] to hunt for interactions.
#'
#' @param model Baseline glm, fitted with a `data =` argument. May be as
#'   small as `y ~ 1 + offset(log(Exposure))`. Poisson/quasipoisson count
#'   models and Gamma log-link models are supported.
#' @param features Candidate columns. `NULL` (default) uses every usable
#'   column of the model's data except the response and the offset.
#' @param split Row shares for train / validation (early stopping) /
#'   test (reporting). Must sum to 1.
#' @param max_depth Maximum tree depth for the second booster (default 2,
#'   i.e. two-way interactions).
#' @param eta,nrounds,early_stopping_rounds Boosting parameters.
#' @param n_shap Rows sampled for the SHAP interaction ranking (default
#'   4000); `0` skips it.
#' @param cor_threshold Absolute correlation above which numeric
#'   candidates are reported as near-duplicates (default 0.95).
#' @param max_levels Factors with more levels than this are skipped
#'   (default 50).
#' @param max_rows Work on a random sample of at most this many rows
#'   (default 1e6); `NULL` uses everything. Screening ranks candidates, and
#'   that ranking is stable long before the last million rows are added, so
#'   on a large portfolio this is much the cheapest lever: every stage
#'   (baseline refit, boosting, permutation) scales with the row count. The
#'   reported deviances then refer to the sample.
#' @param nthread Threads for boosting. `NULL` (default) uses one fewer
#'   than the available cores, capped by `OMP_THREAD_LIMIT` and reduced to
#'   2 under `R CMD check`, which limits what a package may claim. Boosting
#'   dominates the runtime, so this is the second lever: on an eight-core
#'   machine the measured gain over a single-threaded run is more than a
#'   factor of two.
#' @param seed Optional seed for the sample, the split and the boosting.
#'
#' @return A list with `summary` (the staged deviance comparison),
#'   `verdict` (one sentence), `features` (ranked candidates), `gain_note`,
#'   `interactions` (SHAP ranking, or `NULL`), `correlated` (near-duplicate
#'   pairs, or `NULL`), `stats` (the raw numbers) and `plot` (ranked
#'   incremental importance). Requires the 'xgboost' package.
#' @seealso [detect_interactions()], [plot_residual_heatmap()]
#' @export
screen_features <- function(model, features = NULL,
                            split = c(0.6, 0.2, 0.2),
                            max_depth = 2, eta = 0.05, nrounds = 2000,
                            early_stopping_rounds = 40,
                            n_shap = 4000, cor_threshold = 0.95,
                            max_levels = 50, max_rows = 1e6,
                            nthread = NULL, seed = NULL) {

  if (!requireNamespace("xgboost", quietly = TRUE))
    stop("screen_features: package 'xgboost' is required ",
         "(install.packages(\"xgboost\")).", call. = FALSE)
  if (!inherits(model, "glm"))
    stop("screen_features: 'model' must be a glm object.", call. = FALSE)
  if (length(split) != 3 || any(split <= 0) || abs(sum(split) - 1) > 1e-8)
    stop("screen_features: 'split' must be three positive shares summing to 1.",
         call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  if (is.null(nthread)) nthread <- .default_nthread()

  tr <- .glm_training_data(model, "screen_features")
  if (is.null(tr$data))
    stop("screen_features: training data cannot be recovered; fit the model ",
         "with a 'data =' argument.", call. = FALSE)
  d <- tr$data

  # Sub-sample before anything else: every later stage is linear in rows
  keep_rows <- seq_len(nrow(d))
  if (!is.null(max_rows) && nrow(d) > max_rows) {
    keep_rows <- sort(sample(nrow(d), max_rows))
    message("screen_features: sampling ", format(max_rows, big.mark = ","),
            " of ", format(nrow(d), big.mark = ","), " rows (max_rows); ",
            "pass max_rows = NULL to use all of them.")
    d          <- d[keep_rows, , drop = FALSE]
    tr$mf      <- tr$mf[keep_rows, , drop = FALSE]
    tr$weights <- tr$weights[keep_rows]
    if (!is.null(tr$offset)) tr$offset <- tr$offset[keep_rows]
  }

  fam <- family(model)
  counts <- !is.null(tr$offset) && any(tr$offset != 0) &&
            identical(fam$link, "log")
  objective <- if (fam$family %in% c("poisson", "quasipoisson") && counts) {
    "count:poisson"
  } else if (fam$family == "Gamma" && identical(fam$link, "log")) {
    "reg:gamma"
  } else {
    stop("screen_features: only Poisson count models with a log-link offset ",
         "and Gamma log-link models are supported; this model is ",
         fam$family, "/", fam$link, ".", call. = FALSE)
  }

  # Candidate features ------------------------------------------------------
  drop <- unique(c(all.vars(formula(model)[[2]]), .offset_vars(model)))
  if (is.null(features)) features <- setdiff(names(d), drop)
  features <- setdiff(unique(features), drop)
  missing_f <- setdiff(features, names(d))
  if (length(missing_f)) {
    warning("screen_features: column(s) not in the model data, skipped: ",
            paste(missing_f, collapse = ", "), call. = FALSE)
    features <- intersect(features, names(d))
  }
  # Classify each candidate, so a dropped column says why it was dropped
  status <- vapply(features, function(f) {
    x <- d[[f]]
    if (is.logical(x)) x <- as.numeric(x)
    if (is.character(x)) x <- factor(x)
    if (is.factor(x)) {
      nl <- nlevels(droplevels(x))
      if (nl < 2) return("constant")
      if (nl > max_levels) return("too many levels")
      return("ok")
    }
    if (!is.numeric(x)) return("unsupported type")
    if (length(unique(x[!is.na(x)])) < 2) return("constant")
    "ok"
  }, character(1))

  for (why in setdiff(unique(status), "ok"))
    warning("screen_features: skipped (", why, "): ",
            paste(features[status == why], collapse = ", "), call. = FALSE)
  features <- features[status == "ok"]
  if (!length(features))
    stop("screen_features: no usable candidate features left; every ",
         "candidate was constant, unsupported or too high-cardinality.",
         call. = FALSE)

  levs <- .screen_levels(d, features)

  in_model <- intersect(features, .rhs_vars(model))

  # Split, and refit the baseline on the training part so that the
  # comparison against the booster is out of sample on both sides
  n <- nrow(d)
  grp <- sample(rep(c("train", "valid", "test"),
                    times = c(round(split[1] * n), round(split[2] * n),
                              n - round(split[1] * n) - round(split[2] * n))))
  d_tr <- d[grp == "train", , drop = FALSE]
  d_va <- d[grp == "valid", , drop = FALSE]
  d_te <- d[grp == "test",  , drop = FALSE]

  base <- tryCatch(stats::update(model, data = d_tr), error = function(e) NULL)
  if (is.null(base)) {
    # Nearly always a factor level in the model formula that is absent from
    # the training split, so name it rather than leave the user guessing
    culprits <- Filter(Negate(is.null), lapply(.rhs_vars(model), function(v) {
      x <- d_tr[[v]]
      if (!is.null(x) && (is.factor(x) || is.character(x)) &&
          nlevels(droplevels(factor(x))) < 2) v else NULL
    }))
    warning("screen_features: the baseline could not be refitted on the ",
            "training split",
            if (length(culprits))
              paste0(" (no variation left in: ",
                     paste(unlist(culprits), collapse = ", "), ")") else "",
            "; using the supplied model, whose deviance is then in-sample ",
            "and flatters the baseline.", call. = FALSE)
    base <- model
  }

  mu_of <- function(nd) as.numeric(predict(base, newdata = nd,
                                           type = "response"))

  # Response and weights carried explicitly, so a subset of rows (the SHAP
  # sample) stays consistent with its labels
  y_all <- as.numeric(tr$mf[[1]])
  w_all <- if (counts) rep(1, n) else tr$weights
  y_tr <- y_all[grp == "train"]; w_tr <- w_all[grp == "train"]
  y_va <- y_all[grp == "valid"]; w_va <- w_all[grp == "valid"]
  y_te <- y_all[grp == "test"];  w_te <- w_all[grp == "test"]

  # The baseline margin is the expensive part of a predict.glm call, and it
  # does not change when a feature is shuffled, so compute it once per split
  # instead of once per permutation.
  margin_tr <- log(mu_of(d_tr))
  margin_va <- log(mu_of(d_va))
  mu_te     <- mu_of(d_te)
  margin_te <- log(mu_te)

  # Design matrices are built once as well; the permutation loop reshuffles
  # the rows of one feature's column block in place.
  layout_tr <- .screen_matrix(d_tr, features, levs)
  layout_te <- .screen_matrix(d_te, features, levs)
  cols <- layout_tr$owner
  nm   <- colnames(layout_tr$x)

  mk_from <- function(x, y, w, margin) {
    dm <- xgboost::xgb.DMatrix(x, label = y, weight = w)
    xgboost::setinfo(dm, "base_margin", margin)
    dm
  }
  mk <- function(nd, y, w, margin) {
    mk_from(.screen_matrix(nd, features, levs)$x, y, w, margin)
  }

  dm_tr <- mk_from(layout_tr$x, y_tr, w_tr, margin_tr)
  dm_va <- mk(d_va, y_va, w_va, margin_va)
  dm_te <- mk_from(layout_te$x, y_te, w_te, margin_te)

  # xgboost renamed `watchlist` to `evals` in its 2.x series, so pick the
  # name this installation actually has rather than pinning a version
  eval_arg <- if ("evals" %in% names(formals(xgboost::xgb.train)))
    "evals" else "watchlist"
  fit_depth <- function(depth) {
    args <- list(list(objective = objective, max_depth = depth, eta = eta,
                      subsample = 0.8, colsample_bytree = 0.8,
                      nthread = nthread),
                 dm_tr, nrounds = nrounds,
                 early_stopping_rounds = early_stopping_rounds, verbose = 0)
    args[[eval_arg]] <- list(valid = dm_va)
    do.call(xgboost::xgb.train, args)
  }
  b1 <- fit_depth(1)
  b2 <- if (max_depth > 1) fit_depth(max_depth) else b1

  dev0 <- .fam_deviance(fam, y_te, mu_te, w_te)
  dev1 <- .fam_deviance(fam, y_te, predict(b1, dm_te), w_te)
  dev2 <- .fam_deviance(fam, y_te, predict(b2, dm_te), w_te)
  p1 <- 100 * (dev1 - dev0) / dev0
  p2 <- 100 * (dev2 - dev1) / dev1

  summary_df <- data.frame(
    Stage = c("baseline GLM",
              "+ booster, depth 1 (additive only)",
              sprintf("+ booster, depth %d (may use interactions)", max_depth)),
    Deviance = round(c(dev0, dev1, dev2)),
    Change = c("", sprintf("%+.2f%%", p1), sprintf("%+.2f%%", p2)),
    stringsAsFactors = FALSE)

  verdict <- if (p1 < -0.1) {
    paste0("The baseline is missing main effects (depth 1 improves the fit ",
           "by ", sprintf("%.2f%%", -p1), "). Add them first: the ",
           "interaction ranking below is not trustworthy while additive ",
           "structure is still unexplained.")
  } else if (p2 < -0.1) {
    paste0("Main effects look adequate, but interaction structure remains ",
           "(depth ", max_depth, " improves the fit by ",
           sprintf("%.2f%%", -p2), "). Use detect_interactions() to pin ",
           "down which pair.")
  } else {
    "The booster finds nothing material beyond the baseline on the test split."
  }

  # Incremental importance: permute one feature, keep the baseline fixed --
  base_dev <- dev2
  # Shuffling the rows of a feature's column block is equivalent to
  # shuffling the underlying column, and avoids rebuilding the whole
  # design matrix and re-running predict.glm for every candidate.
  perm <- vapply(features, function(f) {
    x  <- layout_te$x
    jj <- which(cols == f)
    x[, jj] <- x[sample(nrow(x)), jj, drop = FALSE]
    .fam_deviance(fam, y_te,
                  predict(b2, mk_from(x, y_te, w_te, margin_te)),
                  w_te) - base_dev
  }, numeric(1))

  gain <- xgboost::xgb.importance(model = b2)
  gain_by <- vapply(features, function(f) {
    sum(gain$Gain[gain$Feature %in% nm[cols == f]])
  }, numeric(1))

  feat_df <- data.frame(
    Feature = features,
    InModel = features %in% in_model,
    PermDeviance = round(as.numeric(perm), 2),
    Gain = round(as.numeric(gain_by), 4),
    stringsAsFactors = FALSE)
  feat_df <- feat_df[order(-feat_df$PermDeviance), ]
  rownames(feat_df) <- NULL

  gain_note <- paste0(
    "Rank on PermDeviance, not Gain: Gain is biased towards continuous and ",
    "high-cardinality features and regularly gives pure noise a sizeable ",
    "share. PermDeviance at or below 0 means no usable signal.")

  # Near-duplicate candidates split their importance ----------------------
  num_f <- features[vapply(features, function(f) is.numeric(d[[f]]),
                           logical(1))]
  correlated <- NULL
  if (length(num_f) > 1) {
    cm <- suppressWarnings(stats::cor(d[, num_f, drop = FALSE],
                                      use = "pairwise.complete.obs"))
    # A pair with no overlapping complete observations correlates to NA.
    # which() ignores those, so pulling the values back out with the same
    # logical mask returns one NA per skipped pair and shifts every value
    # onto the wrong pair; index with the row/column pairs instead.
    n_napair <- sum(is.na(cm[upper.tri(cm)]))
    if (n_napair)
      warning("screen_features: ", n_napair, " numeric pair(s) could not be ",
              "correlated (no overlapping complete observations) and are ",
              "not screened for near-duplication.", call. = FALSE)
    up <- which(upper.tri(cm) & abs(cm) >= cor_threshold, arr.ind = TRUE)
    if (nrow(up)) {
      correlated <- data.frame(
        VarX = num_f[up[, 1]], VarY = num_f[up[, 2]],
        Correlation = round(cm[up], 4),
        stringsAsFactors = FALSE)
      correlated <- correlated[order(-abs(correlated$Correlation)), ]
      rownames(correlated) <- NULL
      warning("screen_features: ", nrow(correlated), " near-duplicate ",
              "candidate pair(s); they split their importance between them ",
              "(see $correlated).", call. = FALSE)
    }
  }

  # SHAP interaction ranking ----------------------------------------------
  interactions <- NULL
  if (max_depth > 1 && n_shap > 0) {
    # The SHAP interaction array is n_shap x (k+1) x (k+1) doubles, and k
    # grows with every factor level once one-hot encoded, so cap the sample
    # at roughly 1 GB rather than let it exhaust memory.
    k_cols  <- length(nm)
    max_rowsz <- max(500, floor(1e9 / (8 * (k_cols + 1)^2)))
    if (n_shap > max_rowsz) {
      message("screen_features: reducing n_shap from ", n_shap, " to ",
              max_rowsz, " to keep the SHAP interaction array near 1 GB (",
              k_cols, " matrix columns).")
      n_shap <- max_rowsz
    }
    s  <- sample(nrow(d_te), min(n_shap, nrow(d_te)))
    si <- tryCatch(
      predict(b2, mk_from(layout_te$x[s, , drop = FALSE],
                          y_te[s], w_te[s], margin_te[s]),
              predinteraction = TRUE),
      error = function(e) {
        warning("screen_features: SHAP interaction ranking failed: ",
                conditionMessage(e), call. = FALSE)
        NULL
      })
    if (!is.null(si)) {
      k <- seq_along(nm)
      M <- apply(abs(si[, k, k, drop = FALSE]), c(2, 3), mean)
      agg <- matrix(0, length(features), length(features),
                    dimnames = list(features, features))
      for (i in k) for (j in k)
        agg[cols[i], cols[j]] <- agg[cols[i], cols[j]] + M[i, j]
      up <- which(upper.tri(agg), arr.ind = TRUE)
      interactions <- data.frame(
        VarX = features[up[, 1]], VarY = features[up[, 2]],
        Strength = round(agg[upper.tri(agg)], 6),
        stringsAsFactors = FALSE)
      interactions <- interactions[order(-interactions$Strength), ]
      rownames(interactions) <- NULL
    }
  }

  # Plot: one bar per candidate, muted where there is no signal ----------
  fp <- feat_df[order(feat_df$PermDeviance), ]
  p <- plot_ly() %>%
    add_bars(
      x = fp$PermDeviance, y = factor(fp$Feature, levels = fp$Feature),
      orientation = "h",
      marker = list(color = ifelse(fp$PermDeviance > 0, ta_blue, ta_muted)),
      text = paste0("Gain: ", formatC(fp$Gain, format = "f", digits = 3),
                    ifelse(fp$InModel, " \u00b7 already in the model", "")),
      hovertemplate = paste0("<b>%{y}</b><br>PermDeviance: %{x:.2f}",
                             "<br>%{text}<extra></extra>")) %>%
    layout(
      title = list(text = "Incremental value over the baseline",
                   font = list(color = ta_navy, size = 14)),
      xaxis = list(title = "Deviance increase when shuffled",
                   gridcolor = "#D0D8E0", zeroline = FALSE),
      yaxis = list(title = "", tickfont = list(size = 10), showgrid = FALSE),
      shapes = list(list(type = "line", xref = "x", yref = "paper",
                         x0 = 0, x1 = 0, y0 = 0, y1 = 1,
                         line = list(color = ta_muted, width = 1,
                                     dash = "dot"))),
      hoverlabel = list(bgcolor = "white", font = list(size = 12)),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(l = 140, b = 60, r = 40)) %>%
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d"),
           toImageButtonOptions = list(format = "png", filename = "screen"))

  message("screen_features: ", verdict)

  list(summary = summary_df, verdict = verdict,
       features = feat_df, gain_note = gain_note,
       interactions = interactions, correlated = correlated,
       stats = list(deviance_baseline = dev0, deviance_depth1 = dev1,
                    deviance_depth2 = dev2, pct_depth1 = p1, pct_depth2 = p2,
                    objective = objective, n_train = nrow(d_tr),
                    n_valid = nrow(d_va), n_test = nrow(d_te),
                    n_rows_used = n, nthread = nthread,
                    best_iteration = c(depth1 = b1$best_iteration,
                                       depth2 = b2$best_iteration)),
       plot = p)
}
