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
  output$energy <- renderPlot({
    req(input$artist)
    big_spotify %>% 
      filter(artist_name == input$artist) %>% 
      ggplot(aes(x = energy)) +
      geom_histogram(binwidth = 0.05, color = "white", fill = "black")
  })
}

shinyApp(ui, server)