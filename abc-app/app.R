library(shiny)
library(tidyverse)

abc <- read_csv("abc.csv")

ui <- fluidPage(titlePanel("Virginia ABC Store prices"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput(
                      "priceInput",
                      "Price",
                      min = 0,
                      max = 100,
                      value = c(25, 40),
                      pre = "$"
                    ),
                    radioButtons(
                      "typeInput",
                      "Product type",
                      choices = c("Mixers", "Rimmers", "Spirits", "Wine"),
                      selected = "Spirits"
                    ),
                    selectInput(
                      "proofInput",
                      "Proof",
                      choices = c("0-40", "40-80", "80-120", "120-160", "160+")
                    ),
                    uiOutput("subtypeInput")
                  ),
                  mainPanel(plotOutput("coolplot"),
                            tableOutput("results"))
                ))

server <- function(input, output) {
  filtered <- reactive({
    if(is.null(input$subtypeInput)) {
      return(NULL)
    }
    
    abc %>%
      filter(
        CurrentPrice >= input$priceInput[1],
        CurrentPrice <= input$priceInput[2],
        Type == input$typeInput,
        ProofBin == input$proofInput,
        Subtype == input$subtypeInput
      )
  })
  
  output$subtypeInput <- renderUI({
    selectInput("subtypeInput", "Subtype",
                sort(unique(abc$Subtype)),
                selected = "Whiskey")
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes(Size)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)
