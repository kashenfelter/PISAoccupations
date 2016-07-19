# library(dplyr)
# library(ggplot2)
# library(lazyeval)
# library(ggrepel)
# library(devtools)
# library(PISAoccupations)
# load("data/pisa.rda")
# load("data/isco_text_plt.rda")

shinyServer(function(input, output) {
     output$rainbow2 <- renderPlot(
          plot_rainbow_shiny(input$subject, c(input$cnt1, input$cnt2), input$years[1]) #+
              # ylim(input$yaxis)
     )
     output$time <- renderPlot(
         plot_time_shiny(input$subject, c(input$cnt1, input$cnt2), input$isco_cats, input$years)
     )
})
