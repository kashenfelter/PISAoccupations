#' Plot of countries and all ISCO categories student's average performances by year designed for Shiny App.
#'
#' @param csubject Character of the from "MATH"/"READ"/"SCIE".
#' @param ccnts Country codes of two countries to compare.
#'
#' @return GGplot2 object.
#'
#' @export

plot_spread_all_shiny <- function(csubject, ccnts) {
    pisa %>%
        filter(subject == csubject,
               cnt %in% ccnts) %>%
        mutate(xl = paste0(year,cnt)) %>%
        arrange(cnt, year) -> sdf

    ggplot(sdf, aes(x = xl, y = ave.perf, group = xl, color = cnt, label = isco)) +
        geom_line() +
        geom_point(data = subset(sdf, isco != "10"), shape = 95, size = 6) +
        geom_point(data = subset(sdf, isco == "10"), size = 4) +
        geom_text_repel(data = subset(sdf, isco != "10")) +
        theme_bw() +
        theme(legend.position = "bottom",
              axis.ticks.x = element_blank()) +
        xlab("") +
        ylab("") +
        scale_color_brewer(name = "Country", palette = "Dark2") +
        scale_x_discrete(breaks = levels(sdf$xl),
                         labels = c("            2003", " ", "            2006", " ",
                                    "            2009", " ", "            2012", " "))
}
