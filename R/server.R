# library(shiny)
# library(dplyr)
library(ggplot2)
# library(lazyeval)
library(ggrepel)
# library(devtools)
# library(PISAoccupations)
# load("data/math.rda")
# load("data/isco_text_plt.rda")
shinyServer(function(input, output) {
     output$rainbow2 <- renderPlot(
          plot_rainbow_shiny(c(input$cnt1, input$cnt2), "2012")
     )
})
