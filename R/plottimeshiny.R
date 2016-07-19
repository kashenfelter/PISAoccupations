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

plot_time_shiny <- function(csubject, cnts, isco_cats, years) {
    pisa %>%
        filter(subject == csubject,
               cnt %in% cnts,
               isco %in% isco_cats,
               year %in% years) %>%
        arrange(as.character(cnt), as.character(year), as.character(isco)) %>%
        mutate(nno = paste0(year,isco)) -> sdf
    ggplot(sdf, aes(x = as.factor(nno), y = ave.perf, color = isco, shape = cnt)) +
        geom_point(size = 4) +
         geom_line(data = subset(sdf, cnt == sort(cnts)[1] & isco != "cnt"),
                   aes(group = isco)) +
        geom_line(data = subset(sdf, cnt == sort(cnts)[2] & isco != "cnt"),
                  aes(group = isco)) +
        # scale_x_continuous(breaks = seq(from = 1, to = length(years)*length(isco_cats), by = 3)) +
        # labels = c("", "2003", "", "", "2006", "", "", "2009", "", "", "2012", "")) +
        # geom_hline()
        theme_bw() +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_text(angle = 90),
              legend.position = "bottom") +
        scale_color_discrete(name = "Country") +
        xlab("") +
        ylab("")
}

