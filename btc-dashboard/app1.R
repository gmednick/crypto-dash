library(tidyverse)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(lubridate)
library(tidymodels)
library(httr)
library(jsonlite)
library(viridis)
library(scales)
library(rvest)
library(plotly)
library(tidytext)
library(rtweet)
library(wordcloud2)
theme_set(theme_light())

coins_df <- GET("https://api.coingecko.com/api/v3/coins/list?include_platform=false") %>%
    content(., as = "text") %>%
    fromJSON(., flatten = TRUE) # convert from JSON to a data frame

names_list <- coins_df %>%
    select(id) %>%
    filter(id == str_remove(id, pattern = "[0-9]+"))

#Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Crypto Dash"),

    # Sidebar with a slider input for number of bins
    dashboardSidebar(
        sidebarMenu(
            menuItem(selectInput("coin",
                        label = "Coin:",
                        choices = names_list$id,
                        selected = "bitcoin"))

        )),

        # Show a plot of the generated distribution
    dashboardBody(
        shinyDashboardThemes(
            theme = "blue_gradient"
        ),
        valueBoxOutput("market_cap", width = 12),
        valueBoxOutput("volume", width = 12),
        plotOutput("price_chart")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    url_input <- reactive({
        paste0("https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&ids=", input$coin,
               "&order=market_cap_desc&per_page=100&page=1&sparkline=false&price_change_percentage=1h%2C24h%2C7d%2C14d%2C30d%2C200d%2C1y%20") %>%
            GET() %>%
            content(., as = "text") %>%
            fromJSON(., flatten = TRUE)
    })

    output$market_cap <- renderValueBox({
        valueBox(
            value = paste0("Market cap for ", url_input()$name, ": $",
                           prettyNum(url_input()$market_cap, big.mark = ",")),
            subtitle = "",
            color = 'light-blue',
            icon =icon("")
        )
    })

    output$volume <- renderValueBox({
        valueBox(
            value = paste0("Total Volume for ", url_input()$name, ": $",
                           prettyNum(url_input()$total_volume, big.mark = ",")),
            subtitle = "",
            color = 'light-blue',
            icon =icon("")
        )
    })

    url_historic <- reactive({
        paste0("https://api.coingecko.com/api/v3/coins/", input$coin, "/market_chart?vs_currency=usd&days=max") %>%
            GET() %>%
            content(., as = "text") %>%
            fromJSON(., flatten = TRUE) %>%
            do.call(data.frame, .) %>%
            as_tibble %>%
            select('time' = prices.1, 'price' = prices.2,
                   'market_cap' = market_caps.2, 'volume' = total_volumes.2) %>%
            mutate(time = time/1000,
                   time = as.Date(as.POSIXct(time, origin="1970-01-01")))
    })

    output$price_chart <- renderPlot({

        url_historic() %>%
            ggplot(aes(time, price)) +
            geom_line() +
            #geom_vline(xintercept = vline$time, lty = 2, color = "midnightblue") +
            geom_point(size = 0.8, aes(color = volume)) +
            labs(title = paste0("Price chart for ", input$name)) +
            scale_y_continuous(labels = dollar) +
            scale_color_viridis_c(option = "D")

    })
}

# Run the application
shinyApp(ui = ui, server = server)
