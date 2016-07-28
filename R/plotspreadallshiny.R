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
        arrange(cnt, year) -> sdf
    sdf$cnt <- apply(data.frame(sdf$cnt), 1,
                     {function(x) return(names(country_names)[grep(country_names, pattern = x)])})

    ggplot(sdf, aes(x = year, y = ave.perf, group = isco, color = isco)) +
        geom_line(size = 2) +
        geom_line(aes(x = year, y = ave.perf, group = year),
                  inherit = FALSE, size = 0.8, alpha = 0.7, color = "grey") +
        geom_point(data = subset(sdf, isco != "cnt"), shape = 95, size = 8) +
        geom_point(data = subset(sdf, isco == "cnt"), size = 4) +
        xlab("") +
        ylab("") +
        theme_tufte(base_size = 18) +
        facet_grid(~cnt) +
        scale_color_discrete(name = "Category",
                             labels = c("cnt" = "Country", isco_text[2:10]))

}
