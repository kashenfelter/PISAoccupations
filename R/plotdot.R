#' Dot plot of average performances of all countries in given year.
#'
#' @param sdf Data frame returned by interactive function after filtering data.
#'
#' @return ggvis plot.
#'
#' @export

plot_dot <- function(sdf) {
    ggvis(sdf, y = ~reorder(cnt_lab, -ave.perf), x = ~ave.perf, fill = ~isco, size = ~pop.share, key := ~id) %>%
        layer_points() %>%
        add_axis("x", title = "Average performance") %>%
        add_axis("y", title = "") %>%
        hide_legend("size") %>%
        hide_legend("fill") %>%
        add_tooltip(give_label, "hover")
}
