#' Dot plot of average performances of all countries in given year.
#'
#' @param csubject Character of the from "MATH"/"READ"/"SCIE".
#' @param cyear Chosen years  - as a character - 2003/2006/2009/2012.
#'
#' @return GGplot2 object.
#'
#' @export

plot_dot_shiny <- function(csubject, cyear) {
    pisa %>%
        filter(subject == csubject,
               year == cyear) %>%
        select(cnt, isco, ave.perf, se, pop.share) -> sdf
    ggplot(subset(sdf, isco == "cnt"), aes(x = reorder(cnt, ave.perf), y = ave.perf)) +
        geom_point() +
        geom_point(data = subset(sdf, isco %in% as.character(c(1:4, 10))), aes(size = pop.share, color = isco)) +
        coord_flip() +
        theme_bw() +
        xlab("") +
        ylab("") +
        scale_size_continuous(guide = F)
}
