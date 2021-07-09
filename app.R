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

# coins_df <- GET("https://api.coingecko.com/api/v3/coins/list?include_platform=false") %>%
#   content(., as = "text") %>%
#   fromJSON(., flatten = TRUE) # convert from JSON to a data frame
#
# names_list <- coins_df %>%
#   select(id) %>%
#   filter(id == str_remove(id, pattern = "[0-9]+"))

names_select <- read_rds('crypto_names2000') %>%
  mutate(value = str_to_lower(value),
  value = str_replace_all(value, " ", "-"))

# define UI for application
ui <- dashboardPage(

  # title
  dashboardHeader(title = "Crypto Dash"),

  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Price chart", tabName = "price_chart", icon = icon("dashboard")),
      menuItem("Twitter words", tabName = "twitter_search", icon = icon("fab fa-twitter"))

    )),

  # dashboard organization
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabItems(
      tabItem(tabName = "price_chart",
              valueBoxOutput("market_cap", width = 12),
              valueBoxOutput("volume", width = 12),
              fluidRow(box(uiOutput("coin_select"), width = 6),
              box(uiOutput("img"), width = 6)),
              plotlyOutput("price_chart")),
      tabItem(tabName = "twitter_search",
              uiOutput("twitter_search"),
              plotOutput("twitter_words")
      )
    )
  )
)
#------------------------------------------------------------------------------

server <- function(input, output) {


  output$coin_select <- renderUI({selectizeInput("coin",
                                              label = "Coin:",
                                              choices = names_select$value,
                                              selected = "trustswap")})

  output$twitter_search <- renderUI({textInput("tweet",
                                               label = NULL,
                                               value = "@trustswap",
                                               placeholder = "@trustswap")})



  url_input <- reactive({
    paste0("https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd&ids=", input$coin,
           "&order=market_cap_desc&per_page=100&page=1&sparkline=false&price_change_percentage=1h%2C24h%2C7d%2C14d%2C30d%2C200d%2C1y%20") %>%
      GET() %>%
      content(., as = "text") %>%
      fromJSON(., flatten = TRUE)
  })

  output$img <- renderUI({
    tags$img(src = url_input()$image)
  })

  output$market_cap <- renderValueBox({
    valueBox(
      value = paste0(url_input()$name, " market cap", ": $",
                     prettyNum(url_input()$market_cap, big.mark = ",")),
      subtitle = "",
      color = 'olive',
      icon =icon("")
    )
  })

  output$volume <- renderValueBox({
    valueBox(
      value = paste0("Total volume",": $",
                     prettyNum(url_input()$total_volume, big.mark = ",")),
      subtitle = "",
      color = 'olive',
      icon =icon("")
    )
  })

  url_historic <- reactive({
    paste0("https://api.coingecko.com/api/v3/coins/", input$coin, "/market_chart?vs_currency=usd&days=max") %>%
      GET() %>%
      content(., as = "text") %>%
      fromJSON(., flatten = TRUE) %>%
      do.call(data.frame, .) %>%
      as_tibble() %>%
      select(time = prices.1, price = prices.2,
             'market_cap' = market_caps.2, 'volume' = total_volumes.2) %>%
      mutate(time = time/1000,
             time = as.Date(as.POSIXct(time, origin="1970-01-01")))
  })

  output$price_chart <- renderPlotly({

    p <- url_historic() %>%
      ggplot(aes(time, price)) +
      geom_line() +
      geom_point(size = 0.8, aes(color = volume)) +
      labs(title = paste0("Price chart for ", str_to_title(input$coin))) +
      scale_y_continuous(labels = dollar) +
      scale_color_viridis_c(option = "D")

    ggplotly(p)

  })


  tweet_search <- reactive({
    search_tweets(input$tweet, n = 1000, include_rts = TRUE, lang = 'en')})

  tweets_df <- reactive({
    tweet_search() %>%
      select(screen_name, text, retweet_count, favorite_count, created_at) %>%
      unnest_tokens(word, text, token = "tweets") %>%
      anti_join(stop_words, by = 'word') %>%
      mutate(word = str_replace_all(word, "[[:punct:]]", ""),
             word = str_replace_all(word, "[^\x01-\x7F]", ""),
             word = str_replace_all(word,"#[a-z,A-Z]*", ""),
             word = str_remove_all(word, "[[:space:]]")) %>%
      count(word, sort = TRUE) %>%
      slice(2:21) %>%
      mutate(word = fct_reorder(word, n))
  })

  output$twitter_words <- renderPlot({

    tweets_df() %>%
      ggplot(aes(n, word, fill = word)) +
      geom_col() +
      theme(legend.position = 'none') +
      labs(title = paste0('Most common words in tweets that reference ', input$tweet),
           x = 'frequency of words') +
      scale_fill_viridis_d()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
