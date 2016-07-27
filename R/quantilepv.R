#' Quantile of student's performances.
#'
#' @param pvname Character of the from "MATH"/"READ"/"SCIE".
#' @param prob Probability.
#' @param groups Characters with grouping variables names.
#' @param data Data frame that contains columns given in groups and pvname arguments.
#'
#' @return Data frame containing levels of given factors and a column with calculated values.
#'

quantile_pv <- function(pvname, prob, groups, data) {
    pisa2003 %>%
        select_(.dots = c(pvlabs, groups)) %>%
        group_by_(.dots = groups) %>%
        summarise_at(1:5, {function(x) return(quantile(x, prob, na.rm = T))}) %>%
        select(starts_with("PV")) %>% rowSums()/5
}
