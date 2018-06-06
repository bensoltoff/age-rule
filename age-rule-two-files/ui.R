# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Can You Date This Person? The Half Plus Seven Rule"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      withMathJax(includeMarkdown("describe.md")),
      numericInput("age_you",
                   "Your age:",
                   min = 14,
                   max = 123,
                   value = 25),
      numericInput("age_partner",
                   "Partner's age:",
                   min = 14,
                   max = 123,
                   value = 25),
      textOutput("age_check", strong)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("age_plot", height = "800px")
    )
  )
)
