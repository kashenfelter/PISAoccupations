shinyUI(fluidPage(
    titlePanel("PISA occupations"),

    sidebarLayout(
        sidebarPanel("Choose perspective",
                     selectInput("cnt1", label = "Select first",
                                 choices = c("Germany" = 1,
                                             "Finland" = 2,
                                             "France" = 3,
                                             "Great Britain" = 4,
                                             "South Korea" = 5,
                                             "Poland" = 6,
                                             "United States of America" = 7), selected = 1),

                     selectInput("cnt2", label = "and second country for comparison",
                                 choices = c("Germany" = 1,
                                             "Finland" = 2,
                                             "France" = 3,
                                             "Great Britain" = 4,
                                             "South Korea" = 5,
                                             "Poland" = 6,
                                             "United States of America" = 7), selected = 7)),
                     #,),

        mainPanel(" ")
    )
))
