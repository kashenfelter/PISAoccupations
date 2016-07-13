#' Student's average performance by given factors in selected year.
#'
#' @param pvlabs Names of column that contain five plausible values.
#' @param groups Names of the grouping variables.
#' @param weights Name of the column that contains final student weight.
#' @param data Name of a data frame containing columns given as previous arguments.
#'
#' @return Data frame containing levels of given factor variables and average performances and sums of weights
#'         for each combination of these factors.
#'
#' @export

mean.pv <- function(pvlabs, groups, weights, data) {
    data  %>%
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
                  freq = sum(weights)) %>%
        mutate(mean = round((mpv1 + mpv2 + mpv3 + mpv4 + mpv5)/5, 2),
                   freq = freq) %>%
        select(-c(mpv1, mpv2, mpv3, mpv4, mpv5)) %>%
        ungroup() %>%
        arrange_(.dots = groups)
}
