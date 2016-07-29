#' Sampling variance for one plausible values and a set of replicate weights.
#'
#' @param data Name of a data frame containing columns given in other arguments.
#' @param pvname String of the form "MATH"/"READ"/"SCIE".
#' @param groups Name of one or more factors used for grouping.
#' @param final_weight Name of a column with student's final weight.
#' @param brr_weights Names of columns that contain BRR weights.
#'
#' @return Vector that contains computed sampling variances.
#'

var_sml_opv <- function(data, pvname, groups, final_weight = "W_FSTUWT", brr_weights = paste0("W_FSTR", 1:80)) {
    replicate_means <- lapply(brr_weights, {function(x) return(mean_pvse(pvname, groups, x, data)[, "mpv1"])})
    diffs <- lapply(data.frame(replicate_means), {function(x) return((x - mean_pvse(pvname, groups, final_weight, data)[, "mpv1"])^2)})
    return(0.05*rowSums(data.frame(diffs), na.rm = T))
}
