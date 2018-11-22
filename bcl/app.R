library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(stringr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      img(src = "BC_Liquor_Store_Port_Coquitlam.jpg", height = "95%", width = "95%"),
      helpText("Image used under CC BY-SA 4.0 license, from Premeditated Chaos - https://commons.wikimedia.org/w/index.php?curid=63286919"),
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      uiOutput("countryOutput"),
      radioButtons("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "WINE"),
      
      radioButtons("sortInput", "Sort by", choices = c("Name", "Alcohol content", "Price",  "Sweetness"), selected = NULL)
      
      
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(),
      textOutput("num_results"), 
      br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }  
    bcl_modified <- bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      ) %>% 
      select(c(-Country, -Type))
    if(nrow(bcl_modified) == 0){
      return(NULL)
    }
    if(input$sortInput == "Price"){
        return(arrange(bcl_modified, Price))
    }
    else if(input$sortInput == "Alcohol content"){
      return(arrange(bcl_modified, Alcohol_Content))
    }
    else if(input$sortInput == "Name"){
      return(arrange(bcl_modified, Name))
    }
    else if(input$sortInput == "Sweetness"){
      return(arrange(bcl_modified, Sweetness))
    }
    else{
      return(bcl_modified)
    }
    
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })

  output$results <- renderTable({
    if (is.null(filtered())) {
      return()
    }
    filtered()
  })
  output$num_results <- renderText({
    if(is.null(filtered())){
      paste("We found 0 options for you :(")
    }
    else {
      paste("We found", nrow(filtered()), "options for you :)")
    }
  }
  )
}

shinyApp(ui = ui, server = server)
