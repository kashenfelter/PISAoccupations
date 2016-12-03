#' Sampling variance for one plausible values and a set of replicate weights.
#'
#' @inheritParams var_sml
#' @param final_weight Name of a column with student's final weight.
#'
#' @return Vector that contains computed sampling variances.
#'

var_sml_opv <- function(data, pvname, groups, final_weight = "W_FSTUWT", brr_weights = paste0("W_FSTR", 1:80)) {
  replicate_means <- lapply(brr_weights, {function(x) return(mean_pvse(pvname, groups, x, data)[, "mpv1"])})
  diffs <- lapply(data.frame(replicate_means), {function(x) return((x - mean_pvse(pvname, groups, final_weight, data)[, "mpv1"])^2)})
  0.05*rowSums(data.frame(diffs), na.rm = T)
}


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
  0.2*rowSums(data.frame(varsmls), na.rm = T)
}


#' Imputation variance for given set of plausible values and weights.
#'
#' @param means_ppv Data frame returned by mean_ppvs function that contains average performances
#'        computed for each plausible value.
#' @return Vector containing computed imputation variances.
#'

var_imp <- function(means_ppv) {
  0.25*rowSums(data.frame(lapply(means_ppv[, paste0("mpv", 1:5)],
				 {function(x) return((x - mean_o(means_ppv))^2)})), na.rm = T)
}


#' Standard errors for average performances grouped by given factors.
#'
#' @inheritParams var_sml_opv
#' @param means_ppv Data frame returned by mean_ppvs function that contains average performances
#'        computed for each plausible value.
#'
#' @return Vector that contains computed standard errors.
#'

se_pv <- function(data, pvname, groups, means_ppv, final_weight = "W_FSTUWT", brr_weights = paste0("W_FSTR", 1:80)) {
  sqrt(var_sml(pvname, groups, brr_weights, final_weight, data) + 1.2*var_imp(means_ppv))
}
