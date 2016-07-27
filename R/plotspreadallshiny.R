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
    sdf$cnt <- apply(data.frame(sdf$cnt), 1,
                     {function(x) return(names(country_names)[grep(country_names, pattern = x)])})

    ggplot(sdf, aes(x = xl, y = ave.perf, group = xl, color = cnt, label = isco)) +
        geom_line() +
        geom_point(data = subset(sdf, isco != "cnt"), shape = 95, size = 6) +
        geom_point(data = subset(sdf, isco == "cnt"), size = 4) +
        geom_text(data = subset(sdf, isco != "cnt"), hjust = "right", vjust = "top") +
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
