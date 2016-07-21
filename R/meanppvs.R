#' Mean calculated separately for each of the five plausible values and a given weight.
#'
#' @param pvname String of the form "MATH", "READ", "SCIE".
#' @param groups Name of one or more factors used for grouping.
#' @param weights Name of a column that contains final student weights.
#' @param data Name of a data frame containing columns given in previous arguments.
#'
#' @return Data frame containing columns for each of given factor variables, five columns with
#'         means calculated for each plausible values and a column with sums of weights.

mean_ppvs <- function(pvname, groups, weights, data) {
    pvlabs <- paste0(paste0("PV", 1:5), pvname)
    data %>%
        select_(.dots = c(pvlabs, groups, weights, school_id)) %>%
        group_by_(.dots = groups) %>%
        rename_(.dots = setNames(c(weights, pvlabs, school_id), c("W_F", paste0("PV", 1:5), "SCH_ID"))) %>%
        summarise(mpv1 = sum(PV1*W_F, na.rm = TRUE)/sum(W_F, na.rm = TRUE),
                  mpv2 = sum(PV2*W_F, na.rm = TRUE)/sum(W_F, na.rm = TRUE),
                  mpv3 = sum(PV3*W_F, na.rm = TRUE)/sum(W_F, na.rm = TRUE),
                  mpv4 = sum(PV4*W_F, na.rm = TRUE)/sum(W_F, na.rm = TRUE),
                  mpv5 = sum(PV5*W_F, na.rm = TRUE)/sum(W_F, na.rm = TRUE),
                  population.share = sum(W_F, na.rm = TRUE),
                  nstud = n(),
                  nschool = n_distinct(SCH_ID, na.rm = TRUE))
}
