#' Rainbow plot of average performances for two countries grouped by isco categories designed for shiny app.
#'
#' @param csubject Character of the from "MATH"/"READ"/"SCIE".
#' @param cnts Country codes of countries to compare on a rainbow plot.
#' @param isco_cats ISCO categories to plot - as a character - numbers of categories.
#' @param years Chosen years  - as a character - 2003/2006/2009/2012.
#'
#' @return GGplot2 object.
#'
#' @export

plot_time_shiny <- function(csubject, cnts, isco_cats) {
    pisa %>%
        filter(subject == csubject,
               cnt %in% cnts,
               isco %in% c(isco_cats, "cnt")) %>%
        mutate(isco = factor(isco, levels = c("cnt", as.character(1:9)))) %>%
    ggplot(aes(x = year, y = ave.perf, color = cnt, group = as.factor(paste0(isco, cnt)))) +
        geom_point() +
        geom_pointrange((aes(ymin = ave.perf - se, ymax = ave.perf + se))) +
        geom_line() +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90),
              legend.position = "right") +
        scale_color_discrete(name = "Country",
                             breaks = cnts,
                             labels = country_names2[cnts]) +
        xlab("") +
        ylab("") +
        facet_grid(~isco, labeller = as_labeller(naming))
}
