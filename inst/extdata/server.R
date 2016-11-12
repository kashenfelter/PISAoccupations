shinyServer(function(input, output) {
    output$rainbow <- renderPlot(
        plot_rainbow(input$subject1, c(input$cnt12, input$cnt22), input$cyear1),
        height = 800
    )
    output$time <- renderggiraph(
        ggiraph(code = print(plot_time(input$subject, c(input$cnt1, input$cnt2), c(input$se, input$trend), input$isco_cats))),
        height = 800
    )
    output$rainbowTime <- renderggiraph(
      ggiraph(code = print(plotRainbowTime(input$subjectt1, c(input$cnt11, input$cnt21))))
    )
    output$dots <- renderggiraph(
      ggiraph(code = print(plotDot(input$subjectt, input$cyear, c("cnt", input$isco_cats2))))
    )
})
