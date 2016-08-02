library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggthemes)
library(ggvis)
library(PISAoccupations)

shinyServer(function(input, output) {
    sdf <- reactive({pisa %>%
                         filter(subject == input$subjectt,
                                year == input$cyear,
                                isco %in% c("cnt", input$isco_cats2))})
    sdf1 <- reactive({pisa %>%
                      filter(subject == input$subjectt1,
                             cnt == input$cnt11)
                    })
    sdf2 <- reactive({pisa %>%
                          filter(subject == input$subjectt1,
                                 cnt == input$cnt21)
                    })

    plot_dot(sdf) %>%
        bind_shiny("dots")
    plot_rainbow_time(sdf1) %>%
        bind_shiny("rnbwt1")
    plot_rainbow_time(sdf2) %>%
        bind_shiny("rnbwt2")

    output$rainbow2 <- renderPlot(
        plot_rainbow(input$subject1, c(input$cnt12, input$cnt22), input$cyear1),
        height = 900
    )
    output$time <- renderPlot(
            plot_time(input$subject, c(input$cnt1, input$cnt2), c(input$se, input$trend), input$isco_cats),
        height = 900
    )
})