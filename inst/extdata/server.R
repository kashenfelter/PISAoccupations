shinyServer(function(input, output) {
    output$rainbow <- renderggiraph(
      ggiraph(code = print(plotRainbow(input$subject1, c(input$cnt12, input$cnt22), input$cyear1)),
	      width = 1)
    )
    output$time <- renderggiraph(
      ggiraph(code = print(plotTime(input$subject, c(input$cnt1, input$cnt2), c(input$se, input$trend), input$isco_cats)),
	      width = 1)
      )
    output$rainbowTime <- renderggiraph(
      ggiraph(code = print(plotRainbowTime(input$subjectt1, c(input$cnt11, input$cnt21))),
	      width = 1)
    )
    output$dots <- renderggiraph(
      ggiraph(code = print(plotDot(input$subjectt, input$cyear, c("cnt", input$isco_cats2))),
	      width = 1)
    )
})
