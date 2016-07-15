#' Average student performance by country and ISCO category - all categories.
#'
#' @param subject String of the form "MATH", "READ", "SCIE".
#' @param cnt_col Name of a column with country codes.
#' @param isco_cols Names of two columns with isco codes for mother's and father's occupation.
#' @param final_weight Name of a column that contains final student weights.
#'
#' @return Data frame with names of countries and means for isco categories.
#'

mean_by_all_cats <- function(subject, cnt_col, isco_cols, final_weight, data) {
    with_names <- mean_by_1_cat(pvname, cnt_col, isco_cols, "1", final_weight, data)
    no_names <- data.frame(apply(data.frame(as.character(2:9)), 1,
                 {function(x) return(mean_by_1_cat(pvname, cnt_col,
                                                   isco_cols, x, final_weight, data)[, "mean"])}))
    means <- cbind(with_names, no_names)
    colnames(means) <- c("CNT", paste0("isco", 1:9))
    return(means)
}
