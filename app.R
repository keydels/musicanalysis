library(shiny)
library(tidyverse)

my_files <- list.files(path=here::here("data/"), pattern="*.csv", full.names = TRUE)
big_spotify <- read_csv(my_files)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "artist",
        label = "Search for an artist"
      )
      # radioButtons(
      #   inputId = "stat",
      #   label = "Choose a statistic",
      #   choices = c("Disc Number" = "disc_number",
      #               "Popularity" = "popularity",
      #               "Track Number" = "track_number",
      #               "Duration" = "duration",
      #               "Danceability" = "danceability",
      #               "Loudness" = "loudness",
      #               "Tempo" = "tempo",
      #               "Time Signature" = "time_signature")
      # )
    ),
    mainPanel(
      plotOutput(outputId = "energy")
  )
  
  )
)

server <- function(input, output, session) {
  output$energy <- renderPlot({
    req(input$artist)
    big_spotify %>% 
      filter(artist_name == input$artist) %>% 
      ggplot(aes(x = energy)) +
      geom_histogram()
  })
}

shinyApp(ui, server)