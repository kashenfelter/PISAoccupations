#' Rainbow plot of average performances for two countries grouped by isco categories designed for shiny app.
#'
#' @param subject Character of the from "MATH"/"READ"/"SCIE".
#' @param cnts Country codes of countries to compare on a rainbow plot.
#' @param cyear Chosen year as a character - 2003/2006/2009/2012.
#'
#' @return GGplot2 object.
#'
#' @export

plot_rainbow_shiny <- function(csubject, cnts, cyear) {
    pisa %>%
        filter(subject == csubject, year == cyear, cnt %in% cnts) %>%
        arrange(as.character(cnt), as.character(isco)) %>%
        mutate(no = if_else(cnt == sort(cnts)[1], 1, 3)) %>%
        mutate(isco2 = ifelse(isco == "cnt", as.character(cnt), "tba")) -> sdf
    sdf$isco2[sdf$isco != "cnt"] <- isco_text_plt[as.integer(as.character(sdf$isco[sdf$isco != "cnt"])) + 1]
    sdf$isco2[sdf$isco == "cnt"] <- apply(data.frame(sdf$isco2[sdf$isco == "cnt"]), 1,
                                          {function(x) return(names(country_names)[grep(country_names, pattern = x)])})

    ggplot(sdf, aes(x = no, y = ave.perf, color = isco, group = isco2, label = isco2)) +
        theme_bw() +
        geom_point(data = subset(sdf, isco != "cnt"), aes(size = pop.share)) +
        geom_line(size = 1.5) +
        geom_text(data = subset(sdf, no == 1 & isco != "cnt"), hjust = "right", size = 18) +
        geom_text(data = subset(sdf, no == 3 & isco != "cnt"), hjust = "left", size = 18) +
        geom_line(data = subset(sdf, isco == "cnt"), aes(group = year), size = 1.5, color = "black") +
        geom_point(data = subset(sdf, isco == "cnt"), color = "black") +
        scale_size(guide = "none") +
        scale_color_discrete(guide = "none") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              legend.position = "none") +
        xlab("") +
        ylab("") +
        geom_text(data = subset(sdf, no == 1 & isco == "cnt"),
                  aes(x = no, y = ave.perf, label = isco2), color = "black", hjust = "right", size = 18) +
        geom_text(data = subset(sdf, no == 3 & isco == "cnt"),
                  aes(x = no, y = ave.perf, label = isco2), color = "black", hjust = "left", 18) +
        xlim(-3, 9)
}
