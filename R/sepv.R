#' Standard errors for average performances grouped by given factors.
#'
#' @param data Name of a data frame containing columns given in other arguments.
#' @param pvname String of the form "MATH", "READ", "SCIE".
#' @param groups Name of one or more factors used for grouping.
#' @param means_ppv Data frame returned by mean_ppvs function that contains average performances
#'        computed for each plausible value.
#' @param final_weight Name of a column that contains final student weights.
#' @param brr_weights Names of the columns that contain BRR weights.
#'
#' @return Vector that contains computed standard errors.
#'

se_pv <- function(data, pvname, groups, means_ppv, final_weight = "W_FSTUWT", brr_weights = paste0("W_FSTR", 1:80)) {
    return(sqrt(var_sml(pvname, groups, brr_weights, final_weight, data) + 1.2*var_imp(means_ppv)))
}
