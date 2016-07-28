#' Rainbow plot of student's average performance over years.
#'
#' @param csubject Character of the from "MATH"/"READ"/"SCIE".
#' @param ccnt Chosen country.
#'
#' @return ggvis plot.
#'
#' @export

plot_rainbow_time <- function(csubject, ccnt) {
    pisa %>%
        filter(subject == csubject,
               cnt %in% ccnt,
               isco != "cnt") %>%
    ggvis(x = ~year, y = ~ave.perf) %>%
        group_by(isco) %>%
        layer_paths(stroke = ~isco) %>%
        layer_points(fill = ~isco) %>%
        layer_points(fill = ~isco, key := ~id) %>% # To przez jakiś bug w ggvis.
        hide_legend("fill") %>%
        hide_legend("stroke") %>%
        add_axis("x", title = "Year") %>%
        add_axis("y", title = "Average performance") %>%
        add_tooltip(give_label, "hover")
}
