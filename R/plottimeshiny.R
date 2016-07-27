#' Rainbow plot of average performances for two countries grouped by isco categories designed for shiny app.
#'
#' @param csubject Character of the from "MATH"/"READ"/"SCIE".
#' @param cnts Country codes of countries to compare on a rainbow plot.
#' @param isco_cats ISCO categories to plot - as a character - numbers of categories.
#'
#' @return GGplot2 object.
#'
#' @export

plot_time_shiny <- function(csubject, cnts, isco_cats) {
    pisa %>%
        filter(subject == csubject,
               cnt %in% cnts,
               isco == "cnt") -> sdf

    pisa %>%
        filter(subject == csubject,
               cnt %in% cnts,
               isco %in% c(isco_cats)) %>%
    ggplot(aes(x = year, y = ave.perf, color = cnt, group = as.factor(paste0(isco, cnt)))) +
        geom_point(size = 2) +
        geom_pointrange((aes(ymin = ave.perf - se, ymax = ave.perf + se))) +
        # geom_line(size = 1.5) +
        theme_bw() +
        theme(legend.position = "right") +
        scale_color_discrete(name = "Country",
                             breaks = cnts,
                             labels = country_names2[cnts]) +
        xlab("") +
        ylab("") +
        geom_smooth(method = "lm", se = F) +
        facet_grid(~isco, labeller = as_labeller(naming[naming != "Country"])) #+
#         geom_point(data = sdf, color = I("black")) +
#         geom_line(data = sdf, color = I("black"))
}
