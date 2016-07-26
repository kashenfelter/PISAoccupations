#' Plot of country and extreme ISCO categories student's average performances by year designed for Shiny App.
#'
#' @param csubject Character of the from "MATH"/"READ"/"SCIE".
#' @param ccnts Country codes of two countries to compare.
#'
#' @return GGplot2 object.
#'
#' @export

plot_spread_shiny <- function(csubject, ccnts) {
    pisa %>%
        filter(subject == csubject,
               cnt %in% ccnts) %>%
        mutate(cat = ifelse(isco == "cnt", "cnt", "isco")) %>%
        group_by(cnt, year, cat) %>%
        summarise(max = max(ave.perf),
                  min = min(ave.perf)) %>%
        mutate(xl = paste0(year,cnt)) %>%
        melt(measure.vars = c("max", "min"),
             variable.name = "type", value.name = "ave.perf") %>%
        arrange(cnt, year) %>%
        filter(!(cat == "cnt" & type == "max")) %>%
        select(-c(type, year))-> sdf

    ggplot(sdf, aes(x = xl, y = ave.perf, group = xl, color = cnt)) +
        geom_line() +
        geom_point(data = subset(sdf, cat == "isco"), shape = 95, size = 6) +
        geom_point(data = subset(sdf, cat == "cnt"), size = 2) +
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
