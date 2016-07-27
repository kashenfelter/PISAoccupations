#' Dot plot of average performances of all countries in given year.
#'
#' @param csubject Character of the from "MATH"/"READ"/"SCIE".
#' @param cyear Chosen years  - as a character - 2003/2006/2009/2012.
#' @param isco_cats ISCO categories to display.
#'
#' @return GGplot2 object.
#'
#' @export

plot_dot_shiny <- function(csubject, cyear, isco_cats = as.character(1:9)) {
    pisa %>%
        filter(subject == csubject,
               year == cyear) %>%
        select(cnt, isco, ave.perf, se, pop.share) -> sdf
    sdf$cnt_lab <- apply(data.frame(sdf$cnt), 1,
                         {function(x) return(names(country_names)[grep(country_names, pattern = x)])})

    ggplot(subset(sdf, isco == "cnt"), aes(x = reorder(cnt_lab, ave.perf), y = ave.perf)) +
        geom_point(data = subset(sdf, isco %in% c(isco_cats, "cnt")), aes(size = pop.share, color = isco)) +
        coord_flip() +
        theme_bw() +
        xlab("") +
        ylab("") +
        scale_size_continuous(guide = F) +
        scale_color_discrete(name = "Category",
                             labels = c("cnt" = "Country", isco_text[2:10]))
}
