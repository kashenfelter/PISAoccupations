#' Rainbow plot of student's average performance over years.
#'
#' @param sdf Data frame returned from reactive function.
#'
#' @return ggvis plot.
#'
#' @export

plot_rainbow_time <- function(sdf) {
    ggvis(sdf, x = ~year, y = ~ave.perf) %>%
        group_by(isco) %>%
        layer_paths(stroke = ~isco) %>%
        layer_points(fill = ~isco) %>%
        layer_points(fill = ~isco, key := ~id) %>% # To przez jakiś bug w ggvis.
        hide_legend("fill") %>%
        hide_legend("stroke") %>%
        add_axis("x", title = "Year") %>%
        add_axis("y", title = "Mean performance") %>%
        add_tooltip(give_label, "hover") %>%
        set_options(width = "auto",
                    height = "auto",
                    duration = 0)
}
