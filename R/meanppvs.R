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
        group_by_(.dots = groups) %>%
        select_(.dots = c(pvlabs, weights)) %>%
        mutate_(wpv1 = interp(~p*w, p = as.name(pvlabs[1]), w = as.name(weights)),
                wpv2 = interp(~p*w, p = as.name(pvlabs[2]), w = as.name(weights)),
                wpv3 = interp(~p*w, p = as.name(pvlabs[3]), w = as.name(weights)),
                wpv4 = interp(~p*w, p = as.name(pvlabs[4]), w = as.name(weights)),
                wpv5 = interp(~p*w, p = as.name(pvlabs[5]), w = as.name(weights)),
                weights = interp(~w, w = as.name(weights))) %>%
        summarise(mpv1 = sum(wpv1)/sum(weights),
                  mpv2 = sum(wpv2)/sum(weights),
                  mpv3 = sum(wpv3)/sum(weights),
                  mpv4 = sum(wpv4)/sum(weights),
                  mpv5 = sum(wpv5)/sum(weights),
                  population.share = sum(weights))
}
