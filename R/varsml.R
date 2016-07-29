#' Sampling variance for a given set of plausible values and weights.
#'
#' @param data Name of a data frame containing columns given in other arguments.
#' @param pvname String of the form "MATH", "READ", "SCIE".
#' @param groups Name of one or more factors used for grouping.
#' @param brr_weights Names of columns that contain BRR weights.
#'
#' @return Vector that contains computed sampling variances.
#'

var_sml <- function(data, pvname, groups, final_weight = "W_FSTUWT", brr_weights = paste0("W_FSTR", 1:80)) {
    pvlabs <- paste0(paste0("PV", 1:5), pvname)
    varsmls <- lapply((1:5), {function(x) return(var_sml_opv(pvlabs[x], groups, brr_weights,final_weight, data))})
    return(0.2*rowSums(data.frame(varsmls), na.rm = T))
}
