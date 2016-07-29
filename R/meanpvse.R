#' Average performance for one plausible value and a given replicate weight.
#'
#' @param data Name of a data frame containing columns given in other arguments.
#' @param pvname Name of the plausible value column.
#' @param groups Name of one or more factors used for grouping.
#' @param weight Name of a column that contains final student weights.
#'
#' @return Data frame containing columns for each of given factor variables, five columns with
#'         means calculated for each plausible values and a column with sums of weights.

mean_pvse <- function(data, pvname, groups, weight) {
    data %>%
        select_(.dots = c(pvname, groups,  weight)) %>%
        group_by_(.dots = groups) %>%
        rename_(.dots = setNames(c(pvname, weight), c("PV", "W_F"))) %>%
        summarise(mpv1 = sum(PV*W_F, na.rm = T)/sum(W_F, na.rm = T))
}

