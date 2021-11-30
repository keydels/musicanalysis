# Load packages
library(shiny)
library(tidyverse)
library(lubridate)

# Load testing data (one file the folder)
data <- read_csv(here::here("data/chunks1-50.csv"))
# Add column for just the release year and track duration in seconds
# Also removes duplicates
data <- data %>%
  mutate(year = substr(release_date, 1, 4),
         duration_s = as.duration(duration) / 1000) %>%
  distinct(track_uri, .keep_all = TRUE) %>%
  arrange(artist_name)

# Create list of possible stats to choose from for the graphs
stats <- sort(c("Disc Number" = "disc_number",
           "Popularity" = 'popularity',
           'Track Number' = 'track_number',
           'Has Explicit Content' = 'explicit',
           'Danceability' = 'danceability',
           'Energy' = 'energy',
           'Key' = 'key',
           'Loudness (dB)' = 'loudness',
           'Major/Minor' = 'mode',
           'Speechiness' = 'speechiness',
           'Acousticness' = 'acousticness',
           'Liveness' = 'liveness',
           'Valence (Happiness)' = 'valence',
           'Tempo' = 'tempo',
           'Time Signature' = 'time_signature',
           'Instrumentalness' = 'instrumentalness',
           'Release Year' = 'year',
           'Track Duration (sec)' = 'duration_s'))

# # Load all data (takes a long time)
# my_files <- list.files(path=here::here("data/"), pattern="*.csv", full.names = TRUE)
# data <- read_csv(my_files)
# # Add column for just the release year and track duration in seconds
# # Also removes duplicates
# data <- data %>%
#   mutate(year = substr(release_date, 1, 4),
#          duration_s = as.duration(duration) / 1000) %>%
#   distinct(track_uri, .keep_all = TRUE) %>%
#   arrange(artist_name)

ui <- fluidPage(
  title = "Visualiztions of Spotify Artist Data",
  br(),
  
  fluidRow(
    column(3, selectizeInput(
      inputId = "artist",
      label = "Search for an artist",
      choices = NULL
    )),
    column(3, selectizeInput(
      inputId = "stat_left",
      label = "Left graph",
      choices = stats,
      selected = "popularity"
    )),
    column(3, selectizeInput(
      inputId = "stat_right",
      label = "Right graph",
      choices = stats,
      selected = "energy"
    )),
    column(3, actionButton(
      inputId = "generate",
      label = "Submit!"
    ))
  ),
  
    fluidRow(
      column(12,
      tabsetPanel(
        tabPanel("Basic Stats", br(), textOutput("welcome")),
        
        tabPanel("Visualizations", br(), textOutput(outputId = "total_tracks"),
                 br(), textOutput("disclaimer"),
                 fluidRow(
                   column(6, plotOutput(outputId = "energy")),
                   column(6, plotOutput(outputId = "tempo"))
                 )
        )
      
      )
  
    )
))

# Server
server <- function(input, output, session) {
  updateSelectizeInput(session, 'artist', choices = data$artist_name, 
                       server = TRUE, selected = "Taylor Swift")
  
  # Server-side rendering for the welcome page
  output$welcome <- renderText({
    paste("Welcome to the analysis page for", input$artist, "!")
  })
  
  
  # Server-side rendering for the visualizations page
  lengthArtist <- function(artist) {
    artist <- data %>% 
      filter(artist_name == artist) %>% 
      summarise(n = n())
    length <- artist[1]
  }
  
  # Left Graph (on visualizations page)
  observeEvent(input$generate, {output$energy <- renderPlot({
    req(input$artist)
    req(input$stat_left)
    req(input$generate)
    
    if (input$stat_left == 'explicit'){ 
           data %>%
             filter(artist_name == input$artist) %>%
             ggplot(aes_string(x = input$stat_left)) +
             geom_bar(fill = "red4") +
             xlab(paste(str_to_sentence(input$stat_left), "for songs by", input$artist))
    }
    else {
      data %>%
        filter(artist_name == input$artist) %>%
        ggplot(aes_string(x = input$stat_left)) +
        geom_histogram(color = "black", fill = "red4") +
        xlab(paste(str_to_sentence(input$stat_left), "for songs by", input$artist))  
      }
  })
  })
  
  # Right Graph (on visualizations page)
  observeEvent(input$generate, {output$tempo <- renderPlot({
    req(input$artist)
    req(input$stat_right)
    req(input$generate)
    
    if (input$stat_right == 'explicit'){ 
      data %>%
        filter(artist_name == input$artist) %>%
        ggplot(aes_string(x = input$stat_right)) +
        geom_bar(fill = "deepskyblue4")} +
        xlab(paste(str_to_sentence(input$stat_right), "for songs by", input$artist))
    else {
      data %>%
        filter(artist_name == input$artist) %>%
        ggplot(aes_string(x = input$stat_right)) +
        geom_histogram(color = "black", fill = "deepskyblue4", bins = 20) +
        xlab(paste(str_to_sentence(input$stat_right), "for songs by", input$artist))
    }
  })
  })
  
  
  
  observeEvent(input$generate, output$total_tracks <- renderText({
    paste("There are", lengthArtist(input$artist), "total tracks for", input$artist) 
  })
  )
  
  observeEvent(input$generate, output$disclaimer <- renderText({
    "Please be aware that some tracks may have been automatically removed due to 
          incorrect data. Furthermore, note that the total number of track for some 
          artists may be overestimated. Some artists have duplicate tracks as 
          some tracks are released on multiple different albums (e.g., a regular and a 
          deluxe album)."
  })
  )
  
  
}

shinyApp(ui, server)