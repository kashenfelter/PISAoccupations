kraje <- sort(c("Germany" = "DEU",
           "Finland" = "FIN",
           "France" = "FRA",
           "Great Britain" = "GBR",
           "South Korea" = "KOR",
           "Poland" = "POL",
           "United States of America" = "USA",
           "Belgium" = "BEL",
           "Russian Federation" = "RUS",
           "Liechtenstein" = "LIE",
           "Turkey" = "TUR",
           "Czech Republic" = "CZE",
           "Spain" = "ESP",
           "Mexico" = "MEX",
           "Sweden" = "SWE"))

shinyUI(fluidPage(
    titlePanel("PISA occupations"),

    sidebarLayout(
        sidebarPanel(
            # actionButton("save", "Save current plot"),

            selectInput("subject", label = "Subject",
                        choices = c("Mathematics" = "MATH",
                                    "Reading" = "READ",
                                    "Science" = "SCIE"), selected = "MATH"),
            selectInput("cnt1", label = "Select first",
                                 choices = kraje, selected = kraje[1]),
            conditionalPanel(condition = "input.condPans == 'rnbw2' | input.condPans == 'tm1' | input.condPans == 'spr'",
                             selectInput("cnt2", label = "and second country for comparison",
                                 choices = kraje, selected = kraje[length(kraje)])),

            conditionalPanel(condition = "input.condPans == 'rnbw2'",
                             radioButtons("cyear", "Year",
                                          choices = c("2012" = "2012",
                                                      "2009" = "2009",
                                                      "2006" = "2006",
                                                      "2003" = "2003"), selected = "2012")),

            conditionalPanel(condition = "input.condPans == 'tm1'",
                 checkboxGroupInput("years", label = "Years",
                                        choices = c("2012" = "2012",
                                                    "2009" = "2009",
                                                    "2006" = "2006",
                                                    "2003" = "2003"), selected = NULL)),

            conditionalPanel(condition = "input.condPans == 'spr'",
                             selectInput("fspr", label = "Type",
                                        choices = c("Extreme values" = "sp",
                                                    "All categories" = "ac"), selected = "sp")),

            # sliderInput("yaxis", label = "Range for vertical axis",
                        # min = 300, max = 700, value = c(475, 580)), # Do zmiany później.

             conditionalPanel(condition = "input.condPans == 'tm1'",
                              checkboxGroupInput("isco_cats", label = "Primary ISCO categories",
                                        choices = c("0 Armed forces occupations" = "0",
                                                    "1 Managers" = "1",
                                                    "2 Professionals" = "2",
                                                    "3 Technicians and associate professionals" = "3",
                                                    "4 Clerical support workers" = "4",
                                                    "5 Services and sales workers" = "5",
                                                    "6 Skilled agricultural, forestry and fishery workers" = "6",
                                                    "7 Craft and related trade workers" = "7",
                                                    "8 Plant and machine operators, and assemblers" = "8",
                                                    "9 Elementary occupations" = "9"), selected = NULL))),

        mainPanel(
            tabsetPanel(
                # tabPanel("Rainbow plot one country", plotOutput("rainbow1"), value = "rnbw1"),
                tabPanel("Rainbow plot two countries", plotOutput("rainbow2"), value = "rnbw2"),
                tabPanel("Two countries over time", plotOutput("time"), value = "tm1"),
                tabPanel("Plot spread", plotOutput("spread"), value = "spr"),
                id = "condPans"
            )
        )
    )
))
