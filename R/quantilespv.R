#' Quantiles of student's performances.
#'
#' @param pvname Character of the from "MATH"/"READ"/"SCIE".
#' @param probs Vector of probabilities.
#' @param groups Characters with grouping variables names.
#' @param data Data frame that contains columns given in groups and pvname arguments.
#'
#' @return Data frame containing levels of given factors and a column for each calculated quantile.
#'
#' @export

quantiles_pv <- function(pvname, probs, groups, data) {
    quantiles_tmp <- apply(data.frame(probs), 1, {function(x) return(return(quantile_pv(pvname, x, groups, data)))})
    data %>%
        group_by_(.dots = groups) %>%
        summarise(nn = n()) %>%
        select_(.dots = groups) -> labels
    n <- (dim(labels)[2] + dim(quantiles_tmp)[2])
    tmp <- data.frame(labels, quantiles_tmp)
    colnames(tmp)[(n - length(probs) + 1):n] <- paste0("q", probs)
    return(tmp)
}
