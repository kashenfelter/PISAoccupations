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
               isco %in% c(isco_cats, "10"),
               year %in% years) %>%
        arrange(as.character(cnt), as.character(year), as.character(isco)) %>%
        mutate(nno = paste0(year,isco),
               nno2 = paste0(year, cnt)) -> sdf
    sdf$mxl <- apply(data.frame(as.character(sdf$nno2)), 1, {function(x)
        return(sdf[(sdf$cnt == substr(x, 5, 7) & sdf$isco == "10" & sdf$year == substr(x, 1, 4)), "ave.perf"])})
    ggplot(subset(sdf, isco != "10"), aes(x = as.factor(nno), y = ave.perf, color = isco, shape = cnt)) +
        geom_point(size = 4) +
        geom_pointrange(aes(ymin=ave.perf - se, ymax=ave.perf + se)) +
        geom_line(data = subset(sdf, cnt == sort(cnts)[1] & isco != "10"),
                   aes(group = isco)) +
        geom_line(data = subset(sdf, cnt == sort(cnts)[2] & isco != "10"),
                  aes(group = isco)) +
        geom_line(data = subset(sdf, isco != "10"), aes(x = as.factor(nno), y = mxl, group = nno2, linetype = cnt, color = cnt)) +
        theme_bw() +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              legend.position = "bottom") +
        scale_shape_discrete(name = "Country") +
        scale_color_discrete(name = "Primary ISCO category",
                             breaks = as.character(0:9),
                             labels = as.character(0:9)) +
        xlab("") +
        ylab("")
}
