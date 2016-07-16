#' Preparing data to plot.
#'
#' @param subject String of the form "MATH"/"READ"/"SCIE".
#' @param cnt_col Names of the columns that contain country codes.
#' @param cnts Country codes of countries to compare on a rainbow plot.
#' @param isco_cols Names of the columns with father's and mother's isco codes.
#' @param final_weight Name of a column with final student weights.
#' @param data Data frame returned by mean_by_all_cats function.
#'
#' @return Data frame with column for country code, number that is used by plot_rainbow function
#'         for x axis, isco categories along with country average perf. and means for each category.
#'

to_rainbow <- function(subject, cnt_col, cnts, isco_cols, final_weight, data) {
    load("data/isco_text_plt.rda")
    data %>%
        filter_(substitute(x == y | x == z, list(x = as.name(cnt_col), y = cnts[1], z = cnts[2]))) %>%
        select_(.dots = c(paste0("PV", 1:5, subject), cnt_col, isco_cols, final_weight)) %>%
        mean_by_all_cats(subject, cnt_col, isco_cols, final_weight, .) %>%
        melt() %>%
        arrange_(.dots = c(cnt_col, "variable")) %>%
        rename_(cnt = as.name(cnt_col)) %>%
        rename(occu = variable,
               ave.perf = value) %>%
        mutate(no = ifelse(cnt == sort(cnts)[1], 1, 5)) %>%
        mutate(occu = as.factor(c(cnts[1], isco_text_plt[2:10], cnts[2], isco_text_plt[2:10])))
}
