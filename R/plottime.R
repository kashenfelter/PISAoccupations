#' Rainbow plot of average performances for two countries grouped by isco categories designed for shiny app.
#'
#' @param csubject Character of the from "MATH"/"READ"/"SCIE".
#' @param cnts Country codes of countries to compare on a rainbow plot.
#' @param isco_cats ISCO categories to plot - as a character - numbers of categories.
#'
#' @return GGplot2 object.
#'
#' @export

plot_time_shiny <- function(csubject, cnts, isco_cats = as.character(1:9)) {
    pisa %>%
        filter(subject == csubject,
               cnt %in% cnts,
               isco == "cnt") -> sdf

    pisa %>%
        filter(subject == csubject,
               cnt %in% cnts,
               isco %in% c(isco_cats)) %>%
    ggplot(aes(x = year, y = ave.perf, shape = cnt, color = isco, group = as.factor(paste0(isco, cnt)))) +
        geom_point(size = 4) +
        theme_tufte(base_size = 18) +
        theme(legend.position = "right") +
        scale_shape_discrete(name = "Country",
                             breaks = cnts,
                             labels = country_names2[cnts]) +
        scale_color_discrete(guide = "none") +
        xlab("") +
        ylab("") +
        theme(axis.text.x = element_text(angle = 90)) +
        facet_grid(~isco, labeller = as_labeller(naming[naming != "Country"]))
}