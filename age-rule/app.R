library(shiny)
library(dplyr)
library(ggplot2)
library(markdown)

# function to determine minimum date-able age
age_min <- function(you){
  age <- (you / 2) + 7
  age[age > you] <- you[age > you]
  return(age)
}

# function to determine maximum date-able age
age_max <- function(you){
  age <- 2 * (you - 7)
  age[age < you] <- you[age < you]
  return(age)
}

# function to check if this is the roy moore scandal
roy_moore <- function(you, partner){
  if(you == 32 && partner == 14) return(TRUE)
  return(FALSE)
}

# function to check if you-partner ages are permissible
age_check <- function(you, partner){
  # roy moore check
  if(roy_moore(you, partner)) {
    "This is not appropriate, unless you are Roy Moore"
  } else if(age_min(you) <= partner && age_max(you) >= partner){
    "Yes, this is permissible"
  } else if(age_min(you) > partner){
    "No, you are too old to date this person"
  } else if(age_max(you) < partner){
    "No, you are too young to date this person"
  }
}

# generate example points
age_min_data <- 14
age_max_data <- 123

ages <- data_frame(you = age_min_data:age_max_data,
                   age_min = age_min(you),
                   age_max = age_max(you))

# age plot
cushion <- 15     # amount of extra axis space to draw when rendering plot

age_plot <- ggplot(ages) +
  geom_line(aes(x = you, y = age_min), linetype = 2) +
  geom_line(aes(x = you, y = age_max), linetype = 2) +
  geom_ribbon(aes(x = you, ymin = age_min, ymax = age_max, fill = "range"), alpha = .25) +
  scale_fill_manual(name = NULL, values = c("pink"), labels = c("Acceptable")) +
  labs(title = "Zone of Permissibility",
       x = "Your age",
       y = "Partner's age") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none")

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

# Define server logic
server <- function(input, output) {
  
  output$age_plot <- renderPlot({
    age_plot +
      geom_point(data = data_frame(you = input$age_you,
                                   partner = input$age_partner),
                 aes(you, partner)) +
      coord_cartesian(xlim = c(max(age_min_data, input$age_you - cushion),
                               min(age_max_data, input$age_you + cushion)),
                      ylim = c(max(age_min_data, input$age_partner - cushion),
                               min(age_max_data, input$age_partner + cushion)))
  })
  
  output$age_check <- renderText(age_check(input$age_you,
                                           input$age_partner))
}

# Run the application 
shinyApp(ui = ui, server = server)
