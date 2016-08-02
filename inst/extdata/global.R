if (!require(PISAoccupations)) {
  devtools::install_github("mi2-warsaw/PISAoccupations")
}
library(shiny)
library(ggplot2)
library(dplyr)
library(ggvis)
library(markdown)
library(ggthemes)

pisa %>%
  group_by(cnt, year, subject) %>%
  mutate(cnt_avg = head(ave.perf[isco == 'cnt'],1)) %>%
  dplyr::ungroup() %>%
  arrange(-cnt_avg) %>%
  as.data.frame() -> pisa
