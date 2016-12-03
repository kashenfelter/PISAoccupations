#' Average performance for one plausible value and a given replicate weight.
#'
#' @param data Name of a data frame containing columns given in other arguments.
#' @param pvname Name of the plausible value column.
#' @param groups Name of one or more factors used for grouping.
#' @param final_weights Name of a column that contains final student weights.
#'
#' @return Data frame containing columns for each of given factor variables, five columns with
#'         means calculated for each plausible values and a column with sums of weights.

mean_pvse <- function(data, pvname, groups, final_weights) {
    data %>%
        select_(.dots = c(pvname, groups,  final_weights)) %>%
        group_by_(.dots = groups) %>%
        rename_(.dots = setNames(c(pvname, final_weights), c("PV", "W_F"))) %>%
        summarise(mpv1 = sum(PV*W_F, na.rm = T)/sum(W_F, na.rm = T))
}


#' Mean calculated separately for each of the five plausible values and a given weight.
#'
#' @inheritParams mean_pvse
#' @param student_id Name of a column with student IDs.
#' @param school_id Name of a column with school IDs.
#'
#' @return Data frame containing columns for each of given factor variables, five columns with
#'         means calculated for each plausible values and a column with sums of weights.

mean_ppvs <- function(data, pvname, groups, final_weights, student_id, school_id) {
    pvlabs <- paste0(paste0("PV", 1:5), pvname)
    data %>%
        select_(.dots = c(pvlabs, groups, final_weights, school_id)) %>%
        group_by_(.dots = groups) %>%
        rename_(.dots = setNames(c(final_weights, pvlabs, student_id, school_id), 
				 c("W_F", paste0("PV", 1:5), "STU_ID", "SCH_ID"))) %>%
        summarise(mpv1 = sum(PV1*W_F, na.rm = TRUE)/sum(W_F, na.rm = TRUE),
                  mpv2 = sum(PV2*W_F, na.rm = TRUE)/sum(W_F, na.rm = TRUE),
                  mpv3 = sum(PV3*W_F, na.rm = TRUE)/sum(W_F, na.rm = TRUE),
                  mpv4 = sum(PV4*W_F, na.rm = TRUE)/sum(W_F, na.rm = TRUE),
                  mpv5 = sum(PV5*W_F, na.rm = TRUE)/sum(W_F, na.rm = TRUE),
                  population.share = 0.5*sum(W_F, na.rm = TRUE),
                  nstud = n_distinct(STUD_IDi, na.rm = TRUE),
                  nschool = n_distinct(SCH_ID, na.rm = TRUE))
}


#' Mean calculated from five plausible values.
#'
#' @param means_ppv Data frame returned by mean_ppvs function that contains average performances computed
#'                  for each plausible value.
#'
#' @return Vector of student's average performances.
#'

mean_o <- function(means_ppv) {
  0.2*rowSums(means_ppv[, paste0("mpv", 1:5)], na.rm  = T)
}


#' Average student's performances with standard errors by given factor variables.
#'
#' @inheritParams mean_ppvs
#' @param student_id Name of a column with student IDs.
#' @param brr_weights Names of the columns that contain BRR weights. If se is set to FALSE,
#'                    this argument is redunant and thus the default value is "".
#'
#' @return Data frame with columns for each given factor and student's average performances with standard errors.
#'
#' @export

mean_pv <- function(data, pvname, groups, final_weights, brr_weights, student_id, school_id) {
  tmp <- mean_ppvs(data, pvname, groups, final_weights, student_id, school_id)
  tmp %>% select(-starts_with("mpv")) -> tmp2
  data.frame(tmp2, "mean" = mean_o(tmp), "se" = se_pv(pvname, groups, final_weights, brr_weights, tmp, data))
}
