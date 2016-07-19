shinyUI(fluidPage(
    titlePanel("PISA occupations"),

    sidebarLayout(
        sidebarPanel(selectInput("cnt1", label = "Select first",
                                 choices = c("Germany" = "DEU",
                                             "Finland" = "FIN",
                                             "France" = "FRA",
                                             "Great Britain" = "GBR",
                                             "South Korea" = "KOR",
                                             "Poland" = "POL",
                                             "United States of America" = "USA"), selected = "DUE"),
        conditionalPanel(condition = "input.condPans == 'rnbw2' | input.condPans == 'tm1'",
                         selectInput("cnt2", label = "and second country for comparison",
                                 choices = c("Germany" = "DEU",
                                             "Finland" = "FIN",
                                             "France" = "FRA",
                                             "Great Britain" = "GBR",
                                             "South Korea" = "KOR",
                                             "Poland" = "POL",
                                             "United States of America" = "USA"), selected = "USA")),

                     checkboxGroupInput("years", label = "Years",
                                        choices = c("2012" = "2012",
                                                    "2009" = "2009",
                                                    "2006" = "2009",
                                                    "2003" = "2009"), selected = "2012"),

                     checkboxGroupInput("isco_cats", label = "Primary ISCO categories",
                                        choices = c("Armed forces occupations" = "0",
                                                     "Managers" = "1",
                                                     "Professionals" = "2",
                                                     "Technicians and associate professionals" = "3",
                                                     "Clerical support workers" = "4",
                                                     "Services and sales workers" = "5",
                                                     "Skilled agricultural, forestry and fishery workers" = "6",
                                                     "Craft and related trade workers" = "7",
                                                     "Plant and machine operators, and assemblers" = "8",
                                                     "Elementary occupations" = "9"), selected = "9")),

        mainPanel(
            tabsetPanel(
                tabPanel("Rainbow plot one country", plotOutput("rainbow1"), value = "rnbw1"),
                tabPanel("Rainbow plot two countries", plotOutput("rainbow2"), value = "rnbw2"),
                tabPanel("Two countries over time", plotOutput("time"), value = "tm1"),
                id = "condPans"
            )
        )
    )
))
