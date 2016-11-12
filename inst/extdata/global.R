if (!require(PISAoccupations)) {
  devtools::install_github("mi2-warsaw/PISAoccupations")
}
library(shiny)
library(ggplot2)
library(dplyr)
library(ggvis)
library(markdown)
library(ggthemes)
library(ggiraph)

pisa %>%
  group_by(cnt, year, subject) %>%
  mutate(cnt_avg = head(ave.perf[isco == 'cnt'],1)) %>%
  dplyr::ungroup() %>%
  arrange(-cnt_avg) %>%
  as.data.frame() -> pisa

countryNames <- pisa %>%
  select(cnt) %>%
  distinct() %>%
  unlist() %>%
  as.character()
names(countryNames) <- pisa %>%
  select(cnt_lab) %>%
  distinct() %>%
  unlist() %>%
  as.character() 

countryNames <- countryNames[order(names(countryNames))]

isco_cats
iscos
isco_cats2

iscoLabs <- pisa %>%
  select(isco_lab) %>%
  distinct() %>%
  unlist() %>%
  as.character() %>%
  grep(pattern = "[1-9]", value = TRUE) %>%
  sort()
names(iscoLabs) <- c(as.character(1:9))

subjectChoices <- as.character(sort(unique(pisa$subject)))
names(subjectsiChoices) <- c("Mathematics", "Reading", "Science") 

yearChoices <- as.character(unique(pisa$year))
names(yearChoices) <- as.numeric(yearChoices)
# Podmienić nazwy w pliku ui.R
