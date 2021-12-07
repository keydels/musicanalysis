# Load packages
library(shiny)
library(tidyverse)
library(feather)
library(waiter)
library(magick)
library(lubridate)
library(DT)

# Load testing data (one file the folder)
data <- read_csv(here::here("project/chunks1-50_testing.csv"))
# Add column for just the release year and track duration in seconds
# Also removes duplicates
data <- data %>%
  mutate(year = substr(release_date, 1, 4),
         duration_s = as.duration(duration) / 1000) %>%
  distinct(track_uri, .keep_all = TRUE) %>%
  arrange(artist_name)

# # Load full data (faster than tidyverse)
# data <- read_feather(here::here('project/spotify.feather'))

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

waiting_screen <- tagList(
  spin_loaders(5),
  h4("Loading app!")
) 

# UI -------------------------------------------------------------------

ui <- fluidPage(theme = shinythemes::shinytheme('cyborg'),
  titlePanel(title = "Visualizations of Spotify Artist Data",
             windowTitle = "Music Analysis"),
  br(),
  
  useWaiter(),
  waiter_show_on_load(html = waiting_screen),
  
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
                   column(12, textOutput("welcome"))
                 ), br(),
                 fluidRow(
                   column(6, offset = 2, imageOutput('headphones')))
                 ),
        
        tabPanel("About", 
                 h3("The Authors"), p({
                   'The creators/authors for this app are Ian Curtis (Majors: Statistics, 
                   French), Stacy Keydel (Biostatistics M.S.), 
                   and Sarba Uprety (Data Science, M.S.). This project was the 
                   culminating exeperience of 
                   STA 518: Statistical Computing in R at Grand Valley State University.'
                 }),
                 h3('The Method'), p({
                   'Originally, we started with a smaller dataset that didn\'t have an equal 
                   representation of artists (e.g., more rock artists). As such, we 
                   realized we need to expand our scope and use the Spotify API to get 
                   artist data.'
                 }), p({
                   'We found a dataset call the Spotify Million Playlist Dataset containing 
                   the tracklisting for one million playlists on Spotify. We then used Python 
                   to get all of the tracks on the album for each song and then get track 
                   features for each of the songs on the album. That data can be found in the 
                   original_data/ folder. We created a feather file to help with speed but 
                   this still takes a while to load.'
                 }), p({
                   'For an exploratory data analysis, we extracted the year out of the 
                   Spotify-provided release date because not all tracks had a MDY format. 
                   We also changed the millisecond duration to seconds and removed all 
                   duplicate tracks. Through EDA, summary statistics, and plotting variables, 
                   we discovered that some of the Spotify data was incorrect. This will need 
                   to be corrected in the future.'
                 }), p({
                   'We then worked on constructing an app. We started with a basic shell 
                   that simply displayed plots based on a user\'s input of variables. We 
                   then built in the submit button to prevent the plots from automatically 
                   generating every time a new variable chosen. Finally, we worked on 
                   some customization such as the layout (i.e., variables at the top with 
                   tabs underneath), colors, and (eventually) font.'
                 }),
                 h3('Limitations'), p({
                   'Time was our major limitation. We have grand plans and were 
                        not able to realize all of them. We also are not advanced programmers 
                   in R and were limited in what we understood.'
                 }), p({
                   tags$ul(
                     tags$li('Multiple artists on a track may not be accounted for correctly'),
                     tags$li('On some occaisions, Spotify data is recorded incorrectly (such 
                            as years equalling \'0000\' or tempos of 0'),
                     tags$li('Related to above, filtering graphs so they only display the 
                            correct data'),
                     tags$li('The data is not finished compiling. We used Python to get 
                            the data and it is taken a long time. Moreover, the datasets 
                            are very large, making the app take a long time to load.')
                   )
                 }),
                 h3('Future Work'), p({
                   'Due to our time restraints, we were unable to complete some of the 
                   features we had originally planned. Following are a few items of 
                   potential future work.'
                 }), tags$ul(
                   tags$li('Searching by song and providing a 30-second sample of the 
                           song'),
                   tags$li('Correcting x-axis variable names on the visualizations to 
                           display human-readable names'),
                   tags$li('Updating the welcome page with more content (summary stats?)
                           or gifs.'),
                   tags$li('Load data into a database and connect the app for faster 
                           loading'),
                   tags$li('Customizing app and html of layout')
                 )),
        
        tabPanel("Visualizations",
                 br(), p({
                   "Please be aware that some tracks may have been automatically 
                   removed due to incorrect data. Furthermore, note that the total 
                   number of tracks for some artists may be severly overestimated. Some 
                   artists have duplicate tracks as some tracks are released on 
                   multiple different albums (e.g., a regular and a deluxe album)."
                 }), br(), 
                 textOutput(outputId = "total_tracks"), br(),
                 fluidRow(
                   column(6, plotOutput(outputId = "left_graph")),
                   column(6, plotOutput(outputId = "right_graph"))
                 )
        ),
        
        tabPanel("Variable Info", DTOutput('variables')),
      
      )
  
    )
))

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  waiter_hide()
  w <- Waiter$new(id = c('left_graph', 'right_graph'))
  
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
    
    reactives$total_tracks = n_tracks
    reactives$sub_data = subset
    reactives$lstat = input$stat_left
    reactives$rstat = input$stat_right
    reactives$artist = input$artist
    

  })
  
  # Create a plot based on inputted statistic and artist
  makeGraph <- function(stat, artist, fill_color) {
   w$show()
    if (stat %in% c('explicit', 'disc_number', 'track_number', 'key', 'mode', 
                         'year', 'time_signature')) {
      reactives$sub_data %>% 
      ggplot(aes_string(x = stat)) +
        geom_bar(color = "black", fill = fill_color) +
        labs(title = paste(str_to_sentence(stat), "for songs by", artist)) +
        theme(
          plot.background = element_rect(fill = 'black', color = 'black'),
          axis.text = element_text(color = "#DEDEDE"),
          axis.title = element_text(color = "#CCCCCC"),
          plot.title = element_text(color = "#CCCCCC")
        )
    }
    else {
      if (stat %in% c('popularity', 'loudness')) {binwidth <- 5}
      else if (stat == 'tempo') {binwidth <- 10}
      else if (stat == 'duration_s') {binwidth <- 30}
      else {binwidth <- 0.1}
      
      reactives$sub_data %>% 
      ggplot(aes_string(x = stat)) +
        geom_histogram(color = "black", fill = fill_color, binwidth = binwidth) +
        labs(title = paste(str_to_sentence(stat), "for songs by", artist)) +
        theme(
          plot.background = element_rect(fill = 'black', color = 'black'),
          axis.text = element_text(color = "#DEDEDE"),
          axis.title = element_text(color = "#CCCCCC"),
          plot.title = element_text(color = "#CCCCCC")
        )
      
    }
  }

  # Output for the left graph on Visualizations page
  output$left_graph <- renderPlot({
  
    req(reactives$lstat)
    makeGraph(reactives$lstat, reactives$artist, 'red4')
  
  })
  
  # Output for the right graph on Visualizations page
  output$right_graph <- renderPlot({
    
    req(reactives$rstat)
    makeGraph(reactives$rstat, reactives$artist, 'cyan3')
  })

  
  # States total number of tracks for an artist
  output$total_tracks <- renderText({
    reactives$total_tracks
  })


  
  
  # Server-side rendering for Variables page
  output$variables <- renderDT({
    vars <- read_tsv(here::here('variables.csv'))
    vars %>% datatable(options = list(
      paging = FALSE,
      searching = FALSE,
      order = list(1, 'asc'),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'color': '#000'});",
        "$(this.api().table().header()).css({'background-color': '#AFAFAF'});",
        "}")
      ), rownames = FALSE
    ) %>% formatStyle(columns = names(vars), 
                      target = 'row',
                      color = 'black',
                      backgroundColor = '#CCCCCC')
  })
  
}

shinyApp(ui, server)