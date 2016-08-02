library(shiny)
library(ggplot2)
library(PISAoccupations)
library(dplyr)
library(ggvis)
library(markdown)

shinyUI(navbarPage(
    tags$head(tags$style(
        HTML("
                            #col1 {
                                      background-color: #f8f8f8;
                            }
                            #col2 {
                                      background-color: #f8f8f8;
                            }
                            #col3 {
                                      background-color: #f8f8f8;
                            }
                            #col4 {
                                      background-color: #f8f8f8;
                            }
             #time {
             height: 100vh !important;
             }
             #rainbow2 {
             height: 100vh !important;
             }
             #dots {
             height: 100vh !important;
             }
             #rnbwt1 {
             height: 100vh !important;
             }
             #rnbwt2 {
             height: 100vh !important;
             }
             "
        ))),
    "PISA occupations trends",
                   tabPanel("Home",
                            fluidRow(column(12,
                                            includeMarkdown("https://github.com/mi2-warsaw/PISAoccupations/raw/master/inst/extdata/home.Rmd")
                            ))),
                   tabPanel("Compare categories in time",
                            fluidRow(
                                column(2,
                                    id = "col1",
                                    selectInput("cnt1", label = "Select first",
                                        choices = country_names, selected = country_names[1]),
                                    selectInput("cnt2", label = "and second country for comparison",
                                        choices = c("-" = "-", country_names), selected = "-"),
                                    radioButtons("subject", label = "Subject",
                                        choices = subs_i,
                                        selected = "MATH"),
                                    actionButton("modify",
                                                 "Details"),
                                    conditionalPanel(condition = "(input.modify % 2) == 1",
                                        checkboxInput("se",
                                            label = "Show standard errors",
                                            value = TRUE),
                                        checkboxInput("trend",
                                            label = "Show trend",
                                            value = TRUE),
                                        checkboxGroupInput("isco_cats", label = "Primary ISCO categories",
                                            choices = iscos, selected = as.character(1:9)))),
                                column(10, plotOutput("time"))
                                )),
                   tabPanel("Compare two countries",
                            fluidRow(
                                column(2,
                                       id = "col2",
                                       selectInput("cnt12", label = "Select first",
                                           choices = country_names, selected = country_names[1]),
                                       selectInput("cnt22", label = "and second country for comparison",
                                           choices = c("-" = "-", country_names), selected = country_names[3]),
                                       radioButtons("subject1", label = "Subject",
                                           choices = subs_i,
                                           selected = "MATH"),
                                       radioButtons("cyear1", "Year",
                                           choices = years_i,
                                           selected = "2012")),
                                column(10, plotOutput("rainbow2"))
                            )),
                   tabPanel("Compare two countries in time",
                            fluidRow(
                                column(2,
                                       id = "col3",
                                       selectInput("cnt11", label = "Select first",
                                            choices = country_names,
                                            selected = country_names[1]),
                                       selectInput("cnt21", label = "and second country for comparison",
                                           choices = c("-" = "-", country_names),
                                           selected = "-"),
                                       radioButtons("subjectt1", label = "Subject",
                                           choices = subs_i,
                                           selected = "MATH")),
                                column(10,
                                       splitLayout(
                                           cellWidths = c("50%", "50%"),
                                           ggvisOutput("rnbwt1"),
                                           ggvisOutput("rnbwt2")))
                            )),
                   tabPanel("Compare all countries",
                            fluidRow(
                                column(2,
                                    id = "col4",
                                    radioButtons("subjectt", label = "Subject",
                                        choices = subs_i, selected = "MATH"),
                                    radioButtons("cyear", "Year",
                                        choices = years_i, selected = "2012"),
                                    actionButton("modify2",
                                                 "Details"),
                                    conditionalPanel(condition = "(input.modify2 % 2) == 1",
                                                 checkboxGroupInput("isco_cats2", label = "Primary ISCO categories",
                                                     choices = iscos, selected = as.character(1:9)))),
                                column(10, ggvisOutput("dots"))
                            ))
))
