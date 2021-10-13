

library(shiny)
library(tidyverse)

crypto_data <- read.csv("data/All_combined.csv")

dated_crypto_data <- crypto_data %>%
    mutate(Date = parse_date(`Date`, "%Y-%m-%d"))

crypto_options <- crypto_data %>%
    distinct(Currency_Name) %>%
    arrange(Currency_Name)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Crypto Currency Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Price, Popularity and Prediction of top 50 cryptocurrencies"),
            p("Click on tabs to choose difference aspects."),
            HTML("Data derived from <a href=https://www.kaggle.com/odins0n/top-50-cryptocurrency-historical-prices>Kaggle</a>")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(type = "tabs",
                       tabPanel("Price",
                                selectInput("Cryptocurrency",
                                            "Select Crypto to highlight:",
                                            choices = crypto_options),
                                plotOutput("price_plot")),
                       tabPanel("Popularity",
                                selectInput("Cryptocurrency",
                                            "Select Crypto to highlight:",
                                            choices = crypto_options),
                                plotOutput("popularity_plot")),
                       tabPanel("Predictions",
                                selectInput("Cryptocurrency",
                                            "Select Crypto to highlight:",
                                            choices = crypto_options),
                                plotOutput("prediction_plot")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$price_plot <- renderPlot({
        
    })
    
    output$popularity_plot <- renderPlot({
        
    })
    
    output$prediction_plot <- renderPlot({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
