#' Dot plot of average performances of all countries in given year.
#'
#' @param csubject Character of the from "MATH"/"READ"/"SCIE".
#' @param cyear Chosen years as a character.
#' @param isco_cats ISCO categories to display.
#'
#' @return ggvis plot.
#'
#' @export

plot_dot <- function(sdf, csubject, cyear, isco_cats = as.character(1:9)) {
    ggvis(sdf, y = ~reorder(cnt_lab, -ave.perf), x = ~ave.perf, fill = ~isco, size = ~pop.share, key := ~id) %>%
        layer_points() %>%
        add_axis("x", title = "Year") %>%
        add_axis("y", title = "Average performance") %>%
        hide_legend("size") %>%
        hide_legend("fill") %>%
        add_tooltip(give_label, "hover")
}
