library(dplyr)
library(ggplot2)
library(ggrepel)
library(PISAoccupations)

pisa_all %>%
    filter(nstud >= 30, nschool >= 5) -> pisa

shinyServer(function(input, output) {
     output$rainbow2 <- renderPlot(
          plot_rainbow_shiny(input$subject, c(input$cnt1, input$cnt2), input$cyear) #+
              # ylim(input$yaxis)
     )
     output$time <- renderPlot(
         plot_time_shiny(input$subject, c(input$cnt1, input$cnt2), input$isco_cats, input$years)
     )
     output$spread <- renderPlot(
         if(input$fspr == "sp")
             plot_spread_shiny(input$subject, c(input$cnt1, input$cnt2))
         else
             plot_spread_all_shiny(input$subject, c(input$cnt1, input$cnt2))
     )
})
