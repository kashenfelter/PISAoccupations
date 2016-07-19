#' Imputation variance for given set of plausible values and weights.
#'
#' @param means_ppv Data frame returned by mean_ppvs function that contains average performances
#'        computed for each plausible value.
#' @return Vector containing computed imputation variances.
#'

var_imp <- function(means_ppv) {
    return(0.25*rowSums(data.frame(lapply(means_ppv[, paste0("mpv", 1:5)],
                                          {function(x) return((x - mean_o(means_ppv))^2)})), na.rm = T))
}
