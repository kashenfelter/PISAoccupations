#' Average student's performances with standard errors by given factor variables.
#'
#' @param pvname String of the form "MATH", "READ", "SCIE".
#' @param groups Name of one or more factors used for grouping.
#' @param final_weight Name of a column that contains final student weights.
#' @param data Name of a data frame containing columns given in previous arguments.
#' @param se TRUE if standard error are to be returned, FALSE if only means are to be returned.
#' @param brr_weights Names of the columns that contain BRR weights. If se is set to FALSE,
#'                    this argument is redunant and thus the default value is "".
#'
#' @return Data frame with columns for each given factor and student's average performances with standard errors.
#'
#' @export

mean_pv <- function(pvname, groups, final_weight, brr_weights, data) {
    tmp <- mean_ppvs(pvname, groups, final_weight, data)
    tmp %>% select(-starts_with("mpv")) -> tmp2
    return(data.frame(tmp2, "mean" = mean_o(tmp), "se" = se_pv(pvname, groups, final_weight, brr_weights, tmp, data)))
}
