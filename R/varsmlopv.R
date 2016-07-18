#' Sampling variance for one plausible values and a set of replicate weights.
#'
#' @param pvname String of the form "MATH", "READ", "SCIE".
#' @param groups Name of one or more factors used for grouping.
#' @param brr_weights Names of columns that contain BRR weights.
#' @param means Vector of average performances computed by mean_o function.
#' @param data Name of a data frame containing columns given in previous arguments.
#'
#' @return Vector that contains computed sampling variances.
#'

var_sml_opv <- function(pvname, groups, brr_weights, final_weight, data) {
    replicate_means <- data.frame(lapply(brr_weights, {function(x) return(mean_pvse(pvname, groups, x, data)[, 2])}))
    diffs <- lapply(replicate_means, {function(x) return((x - mean_pvse(pvname, groups, final_weight, data)[, 2])^2)})
    return(data.frame(0.05*rowSums(data.frame(diffs))))
}
