if (!require(PISAoccupations)) {
  devtools::install_github("mi2-warsaw/PISAoccupations")
}
library(shiny)
library(ggplot2)
library(dplyr)
library(ggvis)
library(markdown)
library(ggthemes)
