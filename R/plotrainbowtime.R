#' Rainbow plot of student's average performance over years.
#'
#' @param sdf Data frame returned from reactive function.
#'
#' @return ggvis plot.
#'
#' @export

plot_rainbow_time <- function(sdf) {
    ggvis(sdf, x = ~year, y = ~ave.perf, strokeWidth := 4) %>%
        group_by(isco) %>%
        arrange(year) %>%
        layer_paths(stroke := ~color) %>%
        layer_points(fill := ~color) %>%
        layer_points(fill := ~color, key := ~id) %>% # To przez jakiś bug w ggvis.
        hide_legend("fill") %>%
        hide_legend("stroke") %>%
        add_axis("x", title = "Year",
                 properties = axis_props(title = list(fontSize = 16),
                                         labels = list(fontSize = 16))) %>%
        add_axis("y", title = "Mean performance", title_offset = 50,
                 properties = axis_props(title = list(fontSize = 16),
                                         labels = list(fontSize = 16))) %>%
        add_tooltip(give_label, "hover") %>%
        set_options(width = "auto",
                    height = "auto",
                    duration = 0)
}
