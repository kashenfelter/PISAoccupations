#' Function thats starts package's shiny app.
#'
#' @export

exploreOccupations <- function() {
    shinyApp(
        ui = navbarPage("PISA occupations",
                        tabPanel("Changes in time",
                                 fluidRow(
                                     column(2,
                                            selectInput("cnt1", label = "Select first",
                                                        choices = country_names, selected = country_names[1]),
                                            selectInput("cnt2", label = "and second country for comparison",
                                                        choices = c("-" = "-", country_names), selected = "-"),
                                            selectInput("subject", label = "Subject",
                                                        choices = subs_i,
                                                        selected = "MATH"),
                                            actionButton("modify",
                                                         "More details"),
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
                        tabPanel("Rainbow plot",
                                 fluidRow(
                                     column(2,
                                            selectInput("cnt12", label = "Select first",
                                                        choices = country_names, selected = country_names[1]),
                                            selectInput("cnt22", label = "and second country for comparison",
                                                        choices = c("-" = "-", country_names), selected = country_names[3]),
                                            selectInput("subject1", label = "Subject",
                                                        choices = subs_i,
                                                        selected = "MATH"),
                                            radioButtons("cyear1", "Year",
                                                         choices = years_i,
                                                         selected = "2012")),
                                     column(10, plotOutput("rainbow2"))
                                 )),
                        tabPanel("Rainbow plot in time",
                                 fluidRow(
                                     column(4,
                                            selectInput("cnt11", label = "Select first",
                                                        choices = country_names,
                                                        selected = country_names[1])),
                                     column(4,
                                            selectInput("cnt21", label = "and second country for comparison",
                                                        choices = c("-" = "-", country_names),
                                                        selected = "-")),
                                     column(4,
                                            selectInput("subjectt1", label = "Subject",
                                                        choices = subs_i,
                                                        selected = "MATH")),
                                     fluidRow(
                                         column(12,
                                                splitLayout(
                                                    cellWidths = c("50%", "50%"),
                                                    ggvisOutput("rnbwt1"),
                                                    ggvisOutput("rnbwt2")))

                                     ))),
                        tabPanel("All countries comparison",
                                 fluidRow(
                                     column(2,
                                            selectInput("subjectt", label = "Subject",
                                                        choices = subs_i, selected = "MATH"),
                                            radioButtons("cyear", "Year",
                                                         choices = years_i, selected = "2012"),
                                            actionButton("modify2",
                                                         "More details"),
                                            conditionalPanel(condition = "(input.modify2 % 2) == 1",
                                                             checkboxGroupInput("isco_cats2", label = "Primary ISCO categories",
                                                                                choices = iscos, selected = as.character(1:9)))),
                                     column(10, ggvisOutput("dots"))
                                 ))
        ),
    server = function(input, output) {
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
        plot_dot(sdf) %>% bind_shiny("dots")
        plot_rainbow_time(sdf1) %>% bind_shiny("rnbwt1")
        plot_rainbow_time(sdf2) %>% bind_shiny("rnbwt2")

        output$rainbow2 <- renderPlot(
            plot_rainbow(input$subject1, c(input$cnt12, input$cnt22), input$cyear1),
            height = 900
        )
        output$time <- renderPlot(
            if(input$se & input$trend)
                plot_time_shiny(input$subject, c(input$cnt1, input$cnt2), input$isco_cats) +
                geom_pointrange((aes(ymin = ave.perf - se, ymax = ave.perf + se))) +
                geom_smooth(method = "lm", se = F)
            else if(input$se & !input$trend)
                plot_time_shiny(input$subject, c(input$cnt1, input$cnt2), input$isco_cats) +
                geom_pointrange((aes(ymin = ave.perf - se, ymax = ave.perf + se)))
            else if(!input$se & input$trend)
                plot_time_shiny(input$subject, c(input$cnt1, input$cnt2), input$isco_cats) +
                geom_smooth(method = "lm", se = F)
            else
                plot_time_shiny(input$subject, c(input$cnt1, input$cnt2), input$isco_cats),
            height = 900
        )
    }
    )
}
