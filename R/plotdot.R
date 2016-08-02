#' Dot plot of average performances of all countries in given year.
#'
#' @param sdf Data frame returned by interactive function after filtering data.
#'
#' @return ggvis plot.
#'
#' @export

plot_dot <- function(sdf) {
    ggvis(sdf, y = ~cnt_lab, x = ~ave.perf, fill = ~isco, size = ~pop.share, key := ~id) %>%
        layer_points() %>%
        add_axis("x", title = "Mean performance") %>%
        add_axis("y", title = "") %>%
        # scale_ordinal("y", domain = cntsss) %>%
        hide_legend("size") %>%
        hide_legend("fill") %>%
        add_tooltip(give_label, "hover")  %>%
        set_options(width = "auto",
                    height = "auto") %>%
        set_options(duration = 0)
}
