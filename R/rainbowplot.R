#' Rainbow plot of average performances for two countries grouped by isco categories.
#'
#' @param subject String of the form "MATH"/"READ"/"SCIE".
#' @param cnt_col Names of the columns that contain country codes.
#' @param cnts Country codes of countries to compare on a rainbow plot.
#' @param isco_cols Names of the columns with father's and mother's isco codes.
#' @param final_weight Name of a column with final student weights.
#' @param data Data frame returned by mean_by_all_cats function.
#'
#' @return GGplot2 object.
#'
#' @export

plot_rainbow <- function(subject, cnt_col, cnts, isco_cols, final_weight, data) {
    sdf <- to_rainbow(subject, cnt_col, cnts, isco_cols, final_weight, data)
    ggplot(sdf, aes(x = no, y = ave.perf, color = occu, group = occu, label = occu)) +
        theme_bw() +
        geom_line(size = 1.5) +
        geom_text(data = subset(sdf, no == 1 & !(occu %in% cnts)), hjust = "right", vjust = "bottom") +
        geom_text(data = subset(sdf, no == 5 & !(occu %in% cnts)), hjust = "left", vjust = "bottom") +
        theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), legend.position = "none") +
        xlab("") +
        ylab("") +
        geom_text(data = subset(sdf, occu %in% cnts), aes(x = no, y = ave.perf, label = occu), color = I("black")) +
        xlim(-3, 9) -> rainbow
    return(rainbow)
}
