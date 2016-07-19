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
    id_school <- "SCH_ID"
    data %>%
        group_by_(.dots = groups) %>%
        select_(.dots = c(pvlabs, weights)) %>%
        summarise_(mpv1 = interp("stats::weighted.mean(p,w, na.rm = T)",
                                 p = as.name(pvlabs[1]), w = as.name(weights)),
                   mpv2 = interp("stats::weighted.mean(p,w, na.rm = T)",
                                 p = as.name(pvlabs[2]), w = as.name(weights)),
                   mpv3 = interp("stats::weighted.mean(p,w, na.rm = T)",
                                 p = as.name(pvlabs[3]), w = as.name(weights)),
                   mpv4 = interp("stats::weighted.mean(p,w, na.rm = T)",
                                 p = as.name(pvlabs[4]), w = as.name(weights)),
                   mpv5 = interp("stats::weighted.mean(p,w, na.rm = T)",
                                 p = as.name(pvlabs[5]), w = as.name(weights)),
                   population.share = interp("sum(w, na.rm = T)", w = as.name(weights)),
                   nstud = interp("n()"),
                   nschool = interp("n_distinct(ss, na.rm = T)", ss = as.name(id_school)))
}
