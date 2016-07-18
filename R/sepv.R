#' Standard errors for average performances grouped by given factors.
#'
#' @param pvname String of the form "MATH", "READ", "SCIE".
#' @param groups Name of one or more factors used for grouping.
#' @param final_weight Name of a column that contains final student weights.
#' @param brr_weights Names of the columns that contain BRR weights.
#' @param means_ppv Data frame returned by mean_ppvs function that contains average performances
#'        computed for each plausible value.
#' @param data Name of a data frame containing columns given in previous arguments.
#'
#' @return Vector that contains computed standard errors.
#'

se_pv <- function(pvname, groups, final_weight, brr_weights, means_ppv, data) {
    return(sqrt(var_sml(pvname, groups, brr_weights, final_weight, data) + 1.2*var_imp(means_ppv)))
}
