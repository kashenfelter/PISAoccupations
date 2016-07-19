#' Average performance for one plausible value and a given replicate weight.
#'
#' @param pvname Name of the plausible value column.
#' @param groups Name of one or more factors used for grouping.
#' @param weight Name of a column that contains final student weights.
#' @param data Name of a data frame containing columns given in previous arguments.
#'
#' @return Data frame containing columns for each of given factor variables, five columns with
#'         means calculated for each plausible values and a column with sums of weights.

mean_pvse <- function(pvname, groups, weight, data) {
    data %>%
        group_by_(.dots = groups) %>%
        select_(.dots = c(pvname, weight)) %>%
        mutate_(wpv1 = interp(~p*w, p = as.name(pvname), w = as.name(weight)),
                weights = interp(~w, w = as.name(weight))) %>%
        summarise(mpv1 = sum(wpv1, na.rm = T)/sum(weights, na.rm = T))
}

