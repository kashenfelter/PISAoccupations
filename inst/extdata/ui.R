shinyUI(fluidPage(
  tags$head(tags$script("(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                        m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                        })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
                        ga('create', 'UA-5650686-6', 'auto');
                        ga('send', 'pageview');"),
  tags$style(
    HTML("#col1 #col2 #col3 #col4 { background-color: #f8f8f8; }
         #time #rainbow2{ height: 100vh !important; }
         #title1 #title2 { text-align: center;
                           font-weight: bold;
                           font-size: 16px; }"))), #  #rnbwt2  #dots #rnbwt1
  navbarPage("Trends in Occupations@PISA",
    tabPanel("Home",
      fluidRow(column(12, includeMarkdown("home.md") )),
      fluidRow(column(6, includeMarkdown("homeLeft.md") ),
	       column(6, includeMarkdown("homeRight.md") )),
      fluidRow(column(12, includeMarkdown("homeBottom.md") ))),
	tabPanel("Trends in Occupations",
      fluidRow(column(2,
        id = "col1",
	p("Students' mean performance is plotted on the vertical axis and
	  year of PISA study is displayed on the horizontal axis to help you discover
	  trends across all the ISCO categories."),br(),
	selectInput("cnt1", label = "Select your country of interest",
		    choices = countryNames, selected = "FIN"),
	selectInput("cnt2", label = "Add second country for comparison",
		    choices = c("-" = "-", countryNames), selected = "-"),
	radioButtons("subject", label = "Choose the subject of interest",
		     choices = subjectChoices,
		     selected = "MATH"),
	p("By default all ISCO categories are displayed. You can focus on some of them by clicking 'Plot settings' and unchecking categories."),
	actionButton("modify",
		     "Plot settings"),
	conditionalPanel(condition = "(input.modify % 2) == 1",
          checkboxInput("se",
			label = "Show standard errors",
			value = TRUE),
        checkboxInput("trend",
			 label = "Show trend lines",
			 value = TRUE),
	checkboxGroupInput("isco_cats", label = "Major ISCO group",
			   choices = iscoLabs, selected = as.character(1:9)))),
	         column(10,
			ggiraphOutput("time", width = "100%")))),
             tabPanel("Two countries",
		fluidRow(
	           column(2,
			  id = "col2",
			  p("Students' mean performance is displayed on vertical axis,
			    two countries are on left and right sides of the plot.
			    This plot helps you find out if the results are similar in all categories."),br(),
                     selectInput("cnt12", label = "Select your country of interest",
				 choices = countryNames, selected = "FIN"),
                     selectInput("cnt22", label = "Add second country for comparison",
				 choices = c("-" = "-", countryNames), selected = "BEL"),
		     radioButtons("subject1", label = "Choose the subject of interest",
				  choices = subjectChoices,
				  selected = "MATH"),
		     radioButtons("cyear1", "Choose the PISA study",
				  choices = yearChoices,
				  selected = "2015")),
		   column(10,
			  ggiraphOutput("rainbow", width = "100%")))),
             tabPanel("Trends in two countries",
                      fluidRow(
                        column(2,
                          id = "col3",
			  p("Mean performance is plotted on the vertical axis,
			    horizontal axis shows year of study.
			    You can see how results structure changed over years in one country
			    and compare it with another."),br(),
			  selectInput("cnt11", label = "Select your country of interest",
				      choices = countryNames,
				      selected = "FIN"),
			  selectInput("cnt21", label = "Add second country for comparison",
				      choices = c("-" = "-", countryNames),
				      selected = "BEL"),
                          radioButtons("subjectt1", label = "Choose the subject of interest",
				       choices = subjectChoices,
				       selected = "MATH"),
			  p("Hovering over lines or points on the plot displays a tooltip that gives a name of occupation, it's mean performance and more information.")),
                        column(10,
			       ggiraphOutput("rainbowTime", width = "100%"))
			)),
             tabPanel("All countries",
                      fluidRow(
                        column(2,
                               id = "col4",
                               p("Students' mean performance is plotted on the horizontal axis,
                                  country names on the vertical axis.
                                 Hover over the points or lines to see detailed informations."),br(),
                               radioButtons("subjectt", label = "Choose the subject of interest",
                                            choices = subjectChoices, selected = "MATH"),
                               radioButtons("cyear", "Choose the PISA study",
                                            choices = yearChoices, selected = "2015"),
                               p("By default all ISCO categories are displayed. You can focus on some of them by clicking 'Plot settings' and unchecking categories."),
                               actionButton("modify2",
                                            "Plot settings"),
                               conditionalPanel(condition = "(input.modify2 % 2) == 1",
                                                checkboxGroupInput("isco_cats2", label = "Primary ISCO categories",
                                                                   choices = iscoLabs, selected = as.character(1:9)))),
                        column(10,
                               ggiraphOutput("dots", width = "100%"))
                      ))
  )
))
