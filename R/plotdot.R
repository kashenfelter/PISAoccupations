#' Dot plot of average performances of all countries in given year.
#'
#' @param sdf Data frame returned by interactive function after filtering data.
#'
#' @return ggvis plot.
#'
#' @export

plot_dot <- function(sdf) {
    ggvis(sdf, y = ~reorder(cnt_lab, -cnt_avg), x = ~ave.perf, fill := ~color, size = ~pop.share, key := ~id) %>%
        layer_points() %>%
        add_axis("x", title = "Mean performance",
                      properties = axis_props(labels = list(fontSize = 16),
                                              title = list(fontSize = 9))) %>%
        add_axis("y", title = "") %>%
        hide_legend("size") %>%
        hide_legend("fill") %>%
        add_tooltip(give_label, "hover")  %>%
        set_options(width = "auto",
                    height = "auto") %>%
        set_options(duration = 0)
}
