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