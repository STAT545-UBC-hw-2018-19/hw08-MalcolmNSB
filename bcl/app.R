library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      img(src = "BC_Liquor_Store_Port_Coquitlam.jpg", height = "95%", width = "95%"),
      helpText("Image with permission from Premeditated Chaos - Own work, CC BY-SA 4.0, https://commons.wikimedia.org/w/index.php?curid=63286919"),
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "WINE"),
      #selectInput("sortInput", "Sort by", choices = c("None", "Price"), selected = NULL),
      radioButtons("sortInput", "Sort by", choices = c("Product Name", "Alcohol content", "Price",  "Sweetness"), selected = NULL),
      #checkboxGroupInput("sortInput", "Sort by price", value = FALSE),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
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
    
    if(input$sortInput == "Price"){
        arrange(bcl_modified, Price)
    }
    else if(input$sortInput == "Alcohol content"){
      arrange(bcl_modified, Alcohol_Content)
    }
    else if(input$sortInput == "Product Name"){
      arrange(bcl_modified, Name)
    }
    else if(input$sortInput == "Sweetness"){
      arrange(bcl_modified, Sweetness)
    }
    else{
      bcl_modified
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
    filtered()
  })
}

shinyApp(ui = ui, server = server)
