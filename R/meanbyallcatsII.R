#' Average student performance by country and ISCO category - all categories - first two digits of the code.
#'
#' @param subject String of the form "MATH", "READ", "SCIE".
#' @param cnt_col Name of a column with country codes.
#' @param isco_cols Names of two columns with isco codes for mother's and father's occupation.
#' @param final_weight Name of a column that contains final student weights.
#' @param data Data frame containing columns given in previous arguments.
#'
#' @return Data frame with names of countries and means for isco categories.
#'
#' @export

mean_by_all_cats_II <- function(subject, cnt_col, isco_cols, final_weight, data) {
    cats <- as.character(paste0(rep(1:9, each = 9), rep(1:9, times = 9)))
    cnt_means <- mean_pv(subject, cnt_col, final_weight, data)[, c(cnt_col, "mean")]
    isco_means <- data.frame(apply(data.frame(cats), 1,
                                   {function(x) return(mean_by_1_cat(subject, cnt_col,
                                                                     isco_cols, x, final_weight, data)[, "mean"])}))
    means <- cbind(cnt_means, isco_means)
    colnames(means) <- c("CNT", "cnt_mean", paste0("isco", cats, "mean"))
    return(means)
}
