# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(shinythemes)
library(magick)
#library(shinybusy)

# Load testing data (one file the folder)
data <- read_csv(here::here("data/chunks756-772.csv"))
# Add column for just the release year and track duration in seconds
# Also removes duplicates
data <- data %>%
  mutate(year = substr(release_date, 1, 4),
         duration_s = as.duration(duration) / 1000) %>%
  distinct(track_uri, .keep_all = TRUE) %>%
  arrange(artist_name)

# Create ordered list of possible stats to choose from for the graphs
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

# Load headphones image
headphones <- image_read(here::here('headphones.jpg'))

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

ui <- fluidPage(theme = shinytheme('cyborg'),
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
        tabPanel("Welcome", br(), 
                 fluidRow(
                   column(6, offset = 2, imageOutput('headphones'))), 
                 fluidRow(
                   column(12, textOutput("welcome"))
                 )),
        
        tabPanel("Visualizations", br(), textOutput(outputId = "total_tracks"),
                 br(), textOutput("disclaimer"), br(),
                 fluidRow(
                   column(6, plotOutput(outputId = "left_graph")),
                   column(6, plotOutput(outputId = "right_graph"))
                 )
        ),
        
        tabPanel("Variables"),
        tabPanel("About")
      
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
  
  output$headphones <- renderImage({
    tmpfile <- headphones %>%
      image_resize('500') %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    list(src = tmpfile, contentType = "image/jpeg")
  }, deleteFile = FALSE)
  
  
  
  
  # Server-side rendering for the visualizations page
  
  # Create empty holder for reactive values
  reactives <- reactiveValues()
  
  # Waits for the action button, then subsets data, prepares messages, and stores 
  # reactive values for use later
  observeEvent(input$generate, {
    
    
    subset <- data %>% 
      select(artist_name, input$stat_left, input$stat_right) %>% 
      filter(artist_name == input$artist)
    
    n_tracks <- paste("There are", nrow(subset), "total tracks for", input$artist)
    disclaimer <- "Please be aware that some tracks may have been automatically removed due to 
          incorrect data. Furthermore, note that the total number of tracks for some 
          artists may be overestimated. Some artists have duplicate tracks as 
          some tracks are released on multiple different albums (e.g., a regular and a 
          deluxe album)."
    
    reactives$total_tracks = n_tracks
    reactives$sub_data = subset
    reactives$disclaimer = disclaimer
    reactives$lstat = input$stat_left
    reactives$rstat = input$stat_right
    reactives$artist = input$artist
    
  
  })
  
  # Create a plot based on inputted statistic and artist
  makeGraph <- function(stat, artist, fill_color) {
    if (stat %in% c('explicit', 'disc_number', 'track_number', 'key', 'mode', 
                         'year', 'time_signature')) {
      reactives$sub_data %>% 
      ggplot(aes_string(x = stat)) +
        geom_bar(color = "black", fill = fill_color) +
        xlab(paste(str_to_sentence(stat), "for songs by", artist))
    }
    else {
      if (stat %in% c('popularity', 'loudness')) {binwidth <- 5}
      else if (stat == 'tempo') {binwidth <- 10}
      else if (stat == 'duration_s') {binwidth <- 30}
      else {binwidth <- 0.1}
      
      reactives$sub_data %>% 
      ggplot(aes_string(x = stat)) +
        geom_histogram(color = "black", fill = fill_color, binwidth = binwidth) +
        xlab(paste(str_to_sentence(stat), "for songs by", artist))
    }
  }
  
  # Output for the left graph on Visualizations page
  output$left_graph <- renderPlot({
    req(reactives$lstat)
    makeGraph(reactives$lstat, reactives$artist, 'salmon2')
    
  })
  
  # Output for the right graph on Visualizations page
  output$right_graph <- renderPlot({
    req(reactives$rstat)
    makeGraph(reactives$rstat, reactives$artist, 'azure4')
  })


  # States total number of tracks for an artist
  output$total_tracks <- renderText({
     reactives$total_tracks
  })

  # States that there are likely to be some errors
  output$disclaimer <- renderText({
    reactives$disclaimer
  })
  
  
  
}

shinyApp(ui, server)