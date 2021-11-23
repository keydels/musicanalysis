library(shiny)
library(tidyverse)

my_files <- list.files(path=here::here("data/"), pattern="*.csv", full.names = TRUE)
big_spotify <- read_csv(my_files)
big_spotify <- big_spotify %>% 
  mutate(year = substr(release_date, 1, 4),
         duration_s = as.duration(duration) / 1000)
spotify_nodup <- big_spotify %>% 
  distinct(track_uri, .keep_all = TRUE)
spotify_nodup <- spotify_nodup[-20]

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "artist",
        label = "Search for an artist"
      ),
      actionButton(
        inputId = "generate",
        label = "Submit!"
      )
    ),
    mainPanel(
      plotOutput(outputId = "energy")
  )
  
  )
)

server <- function(input, output, session) {
  observeEvent(input$generate, {output$energy <- renderPlot({
    req(input$artist)
    spotify_nodup %>% 
      filter(artist_name == input$artist) %>% 
      ggplot(aes(x = energy)) +
      geom_histogram(binwidth = 0.05, color = "white", fill = "black")
  })
  })
}

shinyApp(ui, server)