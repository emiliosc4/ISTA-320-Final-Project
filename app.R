
#Author:    Emilio Santa Cruz
#Class:     ISTA 320


library(shiny)
library(tidyverse)

crypto_data <- read.csv("data/All_combined.csv")

dated_crypto_data <- crypto_data %>%
    mutate(Date = parse_date(`Date`, "%Y-%m-%d"))

#options sorted by highest recorded price
price_crypto_options <- crypto_data %>%
    group_by(Currency_Name) %>%
    summarize(max_price = max(Price)) %>%
    arrange(desc(max_price)) %>%
    distinct(Currency_Name)

#options sorted by highest recorded volume
popularity_crypto_options <- crypto_data %>%
    group_by(Currency_Name) %>%
    summarize(max_vol = max(Vol.)) %>%
    arrange(desc(max_vol)) %>%
    distinct(Currency_Name)


ui <- fluidPage(

    # Application title
    titlePanel("Crypto Currency Dashboard"),

    # Sidebar with description 
    sidebarLayout(
        sidebarPanel(
            h3("Price, Popularity and Prediction of top 50 cryptocurrencies"),
            p("Click on tabs to choose difference aspects."),
            p("Price and Predictions sorted by highest recorded price. Popularity sorted by highest recorded volume."),
            p("2010-2016 removed for clarity of graph due to Bitcoin being created in 2010 in price and prediction plots."),
            p("2010-2019 removed for clarity of graph due to minimal activity relatively in popularity plot."),
            HTML("Data derived from <a href=https://www.kaggle.com/odins0n/top-50-cryptocurrency-historical-prices>Kaggle</a>")
        ),

        # Tabs to choose price, popularity and predictions of cryptos
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

server <- function(input, output) {

    output$price_plot <- renderPlot({ #price plot
        
        #data to be highlighted
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
            theme_linedraw() + 
            xlim(as.Date(c('1/1/2017', '8/24/2021'), format = "%d/%m/%Y"))
            
    })
    
    output$popularity_plot <- renderPlot({ #popularity plot
        
        #data to be highlighted
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
            theme_linedraw() + 
            xlim(as.Date(c('1/1/2020', '8/24/2021'), format = "%d/%m/%Y"))
    })
    
    output$prediction_plot <- renderPlot({ #prediction plot
        
        #data to be highlighted
        highlighted_region <- dated_crypto_data %>%
            filter(Currency_Name == input$prediction)
        
        dated_crypto_data %>%
            ggplot(aes(x = Date,
                       y = Price,
                       group = Currency_Name)) + 
            geom_point(color = "black") + 
            geom_smooth(data = highlighted_region, method = "lm", color = "red") +
            theme_linedraw() + 
            xlim(as.Date(c('1/1/2017', '8/24/2021'), format = "%d/%m/%Y"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
