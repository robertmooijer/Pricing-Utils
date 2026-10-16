#' Add the grouped columns a model needs to a dataset you want to score
#'
#' A grouping is made where it is needed: urbanisation 4 and 5 taken
#' together next to the average claim, brands grouped one way for the
#' frequency and another way for the severity, an age band for a severity
#' model next to a spline in the frequency model. Those columns then live
#' in the data each model was fitted on and nowhere else, while
#' [stats::predict()] needs every one of them on the data it scores. A
#' fitted glm carries its own data, so the relation is read off the model
#' that uses the column and the column is added here, from the source
#' columns that a new portfolio does have.
#'
#' It takes the same `merge_vars` declaration as [make_rating_table()], and
#' the table uses this same code, so one declaration serves both and the
#' two cannot disagree about which policy sits in which group.
#'
#' @section What is refused and what is not:
#' Anything that would be a guess is an error rather than a column with a
#' value in it:
#'
#' * a source value the model's data never saw - a new brand, or a level
#'   with no claims, which therefore has no group in the severity data;
#' * a grouping that is not a function of its source in the model's data,
#'   one value occurring with two groups;
#' * one column name that the two models group differently, which cannot
#'   be scored for both at once; give the two groupings separate names and
#'   declare both, `list(MERK = c("MERK_GR_F", "MERK_GR_S"))`;
#' * a grouped column that is already in `data` and disagrees with the
#'   model. It is checked, never overwritten.
#'
#' A continuous source outside the range the model was fitted on is not
#' refused. The outer bands of a banding are open in practice - nobody over
#' 80 in the claims data does not make 85 a new class - so it takes the
#' outer band, with a warning saying how many rows did. When the same
#' source is also a continuous term of a model, such as `ns(LEEFTIJD, 4)`,
#' the warning says so too: that term extrapolates beyond its range in
#' [stats::predict()], which no grouping can prevent, and capping the
#' values is the usual remedy.
#'
#' Whether a source is continuous or categorical is read off the models: a
#' factor term is categorical, a numeric term continuous. A source no model
#' uses directly is categorical unless it is numeric with more than 25
#' distinct values, so a numeric code such as urbanisation 1 to 5 is looked
#' up exactly and an unknown code is an error, not a neighbouring group.
#'
#' @param data The dataset to score, holding the source columns.
#' @param model_freq,model_sev The fitted glm objects that use the grouped
#'   columns. At least one is required, and a model has to have been fitted
#'   with a `data =` argument for its relation to be read.
#' @param merge_vars Named list or vector, keyed by the source column, of
#'   the grouped columns derived from it, e.g.
#'   `list(Stedelijkheid = "Stedelijkheid_groep",
#'   MERK = c("MERK_GR_F", "MERK_GR_S"), LEEFTIJD = "LFT_KL")`.
#'
#' @return `data` with the grouped columns added, each with the type and
#'   factor levels it has in the model's data. Columns that were already
#'   there are returned unchanged after the check.
#' @seealso [make_rating_table()], whose `merge_vars` takes the same
#'   declaration.
#' @examples
#' set.seed(1)
#' n <- 4000
#' d <- data.frame(Stedelijkheid = factor(sample(1:5, n, TRUE)),
#'                 Exposure = runif(n, .3, 1))
#' d$AantalClaims <- rpois(n, d$Exposure * .2)
#' d$SCHADELAST <- d$AantalClaims * rgamma(n, 3, scale = 600)
#' ds <- d[d$AantalClaims > 0, ]
#' ds$AvgLoss <- ds$SCHADELAST / ds$AantalClaims
#' ds$Stedelijkheid_groep <- factor(ifelse(ds$Stedelijkheid %in% c("4", "5"),
#'                                         "4-5",
#'                                         as.character(ds$Stedelijkheid)))
#' m_sev <- glm(AvgLoss ~ Stedelijkheid_groep, family = Gamma("log"),
#'              data = ds, weights = AantalClaims)
#'
#' new <- data.frame(Stedelijkheid = c(1, 4, 5))
#' new <- add_groupings(new, model_sev = m_sev,
#'                      merge_vars = list(Stedelijkheid = "Stedelijkheid_groep"))
#' new
#' predict(m_sev, new, type = "response")
#' @export
add_groupings <- function(data, model_freq = NULL, model_sev = NULL,
                          merge_vars) {
  fn <- "add_groupings"
  if (is.null(model_freq) && is.null(model_sev))
    stop(fn, ": provide at least one model.", call. = FALSE)
  for (m in list(model_freq, model_sev))
    if (!is.null(m) && !inherits(m, "glm"))
      stop(fn, ": the models must be glm objects.", call. = FALSE)
  if (missing(merge_vars) || is.null(merge_vars))
    stop(fn, ": 'merge_vars' is required, e.g. ",
         "list(Stedelijkheid = \"Stedelijkheid_groep\").", call. = FALSE)
  merge_vars <- as.list(merge_vars)
  if (is.null(names(merge_vars)) || any(!nzchar(names(merge_vars))))
    stop(fn, ": 'merge_vars' must be named by the source column, e.g. ",
         "list(Stedelijkheid = \"Stedelijkheid_groep\").", call. = FALSE)
  .check_cols(data, names(merge_vars), fn)

  used <- unique(c(if (!is.null(model_freq)) .rhs_vars(model_freq),
                   if (!is.null(model_sev))  .rhs_vars(model_sev)))
  is_dt <- data.table::is.data.table(data)
  if (is_dt) data <- data.table::copy(data)
  seen <- character(0)

  for (pv in names(merge_vars)) {
    for (a in as.character(unlist(merge_vars[[pv]]))) {
      if (identical(a, pv))
        stop(fn, ": 'merge_vars' maps '", pv, "' onto itself.", call. = FALSE)
      if (a %in% seen)
        stop(fn, ": '", a, "' is declared more than once.", call. = FALSE)
      seen <- c(seen, a)
      if (!a %in% used && a %in% names(data)) {
        warning(fn, ": no model uses '", a, "', so it is left as it is.",
                call. = FALSE)
        next
      }
      vals <- .grouping_column(data, pv, a, model_freq, model_sev, fn)
      if (is_dt) data.table::set(data, j = a, value = vals)
      else data[[a]] <- vals
    }
  }
  data
}
