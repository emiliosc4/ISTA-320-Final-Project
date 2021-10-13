

library(shiny)
library(tidyverse)

crypto_data <- read.csv("data/All_combined.csv")

dated_crypto_data <- crypto_data %>%
    mutate(Date = parse_date(`Date`, "%Y-%m-%d"))

price_crypto_options <- crypto_data %>%
    group_by(Currency_Name) %>%
    summarize(max_price = max(Price)) %>%
    arrange(desc(max_price)) %>%
    distinct(Currency_Name)

popularity_crypto_options <- crypto_data %>%
    group_by(Currency_Name) %>%
    summarize(max_vol = max(Vol.)) %>%
    arrange(desc(max_vol)) %>%
    distinct(Currency_Name)





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Crypto Currency Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Price, Popularity and Prediction of top 50 cryptocurrencies"),
            p("Click on tabs to choose difference aspects."),
            p("Selections for crypto ordered by highest current aspect."),
            HTML("Data derived from <a href=https://www.kaggle.com/odins0n/top-50-cryptocurrency-historical-prices>Kaggle</a>")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(type = "tabs",
                       tabPanel("Price",
                                selectInput("price",
                                            "Select Crypto to highlight:",
                                            choices = price_crypto_options),
                                plotOutput("price_plot")),
                       tabPanel("Popularity",
                                selectInput("popularity",
                                            "Select Crypto to highlight:",
                                            choices = popularity_crypto_options),
                                plotOutput("popularity_plot")),
                       tabPanel("Predictions",
                                selectInput("prediction",
                                            "Select Crypto to highlight:",
                                            choices = price_crypto_options),
                                plotOutput("prediction_plot")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$price_plot <- renderPlot({
        
        highlighted_region <- dated_crypto_data %>%
            filter(Currency_Name == input$price)
        
        dated_crypto_data %>%
            ggplot(aes(x = Date,
                       y = Price,
                       group = Currency_Name)) + 
            geom_line(color = "black") + 
            geom_point(data = highlighted_region,
                       color = "red") + 
            geom_line(data = highlighted_region,
                      color = "red")+
            theme_linedraw()
            
    })
    
    output$popularity_plot <- renderPlot({
        highlighted_region <- dated_crypto_data %>%
            filter(Currency_Name == input$popularity)
        
        dated_crypto_data %>%
            ggplot(aes(x = Date,
                       y = Vol.,
                       group = Currency_Name)) + 
            geom_line(color = "black") + 
            geom_point(data = highlighted_region,
                       color = "red") + 
            geom_line(data = highlighted_region,
                      color = "red")+
            theme_linedraw()
    })
    
    output$prediction_plot <- renderPlot({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
