# Re-export the spline basis functions so that model formulas such as
# y ~ ns(AGE, 4) work after library(pricingtoolsRmO) without the user
# having to attach 'splines' separately.

#' @importFrom splines ns
#' @export
splines::ns

#' @importFrom splines bs
#' @export
splines::bs
