#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom dplyr rename all_of arrange group_by summarise mutate ungroup
#'   filter n
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point
#'   scale_fill_manual scale_color_manual scale_y_continuous facet_wrap vars
#'   labs theme_minimal theme element_text element_rect
#' @importFrom plotly plot_ly add_bars add_trace add_markers layout config
#'   ggplotly
#' @importFrom grDevices colorRampPalette
#' @importFrom stats predict median quantile formula terms family residuals
#'   fitted model.frame weighted.mean approx setNames nobs AIC aggregate
#' @importFrom utils head tail
NULL

# data.table awareness for [.data.table syntax inside the package
.datatable.aware <- TRUE

# Column names used in data.table / dplyr / ggplot2 non-standard evaluation
# ("." is data.table's list() alias inside [.data.table)
utils::globalVariables(c(
  ".",
  ".x", ".w", ".obs", ".exposure", ".yhat",
  ".metric_max", ".exp_max", ".exp_scaled", ".exp_label", ".metric_label",
  ".thin_label",
  "Exposure", "ClaimCount", "Loss", "Frequency", "Severity", "Year",
  "Level", "Group", "IsBase", "IsThin",
  "Factor_Frequency", "Factor_Severity", "Factor_Premium",
  "Uplift_Frequency", "Uplift_Severity", "Uplift_Premium",
  "bin", "bin_group", "x", "r", "x_var", "x_plot",
  "gx", "gy", "a", "e", "expo", "clm", "AE", "Actual", "Expected", "Claims",
  "observed", "predicted", "weight", "weight_sum",
  "avg_observed", "avg_predicted", "mean_r", "band",
  "E", "C", "w", "ch"
))
