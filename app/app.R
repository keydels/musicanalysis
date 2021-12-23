# Load packages
library(shiny)
library(tidyverse)
library(feather)
library(waiter)
library(magick)
library(lubridate)
library(DT)

# Load testing data (one file the folder)
data <- read_feather('data/spotify_testing.feather')

# # Load full data
# data <- read_feather('data/spotify.feather')

# Load functions
source('functions.R', local = TRUE)

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
headphones <- image_read('headphones.jpg')

waiting_screen <- tagList(
  spin_loaders(5),
  h4("Loading app!")
) 

# UI -------------------------------------------------------------------

ui <- fluidPage(theme = shinythemes::shinytheme('cyborg'),
  titlePanel(title = "Visualizations of Music Artist Data",
             windowTitle = "Music Analysis"),
  br(),
  
  useWaiter(),
  waiter_show_on_load(html = waiting_screen),
  
  # Welcome Tab ----------------------------------------
  tabsetPanel(
    tabPanel("Welcome", br(), h4('Welcome!'),
             p(
               'I hear you\'re interested in looking at the artist data. Well 
               you\'ve come to the right place. To read about the creators of this app, 
               its limitations, and planned future work, navigate to the About tab. To 
               see the plots themselves, navigate to the Visualizations tab and follow 
               the intructions there. To get information on the variables in the app, 
               naviagte to the Variables tab. Enjoy!'
             ), br(),
             fluidRow(
               column(1, offset = 3, imageOutput('headphones'))),
             p(
               'The image above was taken from ',
               a(href='https://unsplash.com/photos/7LNatQYMzm4', '@icons8 on 
                     unsplash.com'),
               'and was free to use under the Unsplash license.'
             )
    ),
    
    # About Tab ----------------------------------------
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
             }), p(
               'We found a dataset called the ',
               a(href = 'https://www.aicrowd.com/challenges/spotify-million-playlist-dataset-challenge', 
                 'Spotify Million Playlist Dataset'), 
               'containing 
                   the tracklisting for one million playlists on Spotify. We then used Python 
                   to get all of the tracks on the album for each song and then get track 
                   features for each of the songs on the album. That data can be found in the 
                   original_data/ folder. We created a feather file to help with speed but 
                   this still takes a while to load.'
             ), p({
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
               tags$li('Adjusting x-axis scales so that variables with the same scale 
                           have the same max and min (ex. danceability and energy should 
                           both go from 0 to 1 regardless of whether there are values at 
                           0 or 1).'),
               tags$li('Updating the welcome page with more content (summary stats?)
                           or gifs.'),
               tags$li('Load data into a database and connect the app for faster 
                           loading'),
               tags$li('Customizing app and html of layout including a custom theme for 
                           fonts and colors')
             ), br()
    ),
    
    # Visualizations Tab -------------------------------------
    navbarMenu("Visualizations",
               tabPanel(
                 'One Artist, Two Variables',
                 fluidRow(
                   column(3, selectizeInput(
                     inputId = "artistOT",
                     label = "Search for an artist",
                     choices = NULL
                   )),
                   column(3, selectizeInput(
                     inputId = "statOTL",
                     label = "Left graph",
                     choices = stats,
                     selected = "popularity"
                   )),
                   column(3, selectizeInput(
                     inputId = "statOTR",
                     label = "Right graph",
                     choices = stats,
                     selected = "energy"
                   )),
                   column(3, actionButton(
                     inputId = "generateOT",
                     label = "Submit!"
                   ))
                 ), br(),
                 h5('Choose an artist and variables above. Then click Submit 
                             and watch your plots appear!'),
                 p(
                   "Please be aware that some tracks may have been automatically 
                   removed due to incorrect data. Furthermore, note that the total 
                   number of tracks for some artists may be severly overestimated. Some 
                   artists have duplicate tracks as some tracks are released on 
                   multiple different albums (e.g., a regular and a deluxe album)."
                 ), br(),
                 textOutput(outputId = "ntracksOT"), br(),
                 fluidRow(
                   column(6, plotOutput(outputId = "graphOTL")),
                   column(6, plotOutput(outputId = "graphOTR"))
                 )
               ),
               tabPanel(
                 'Two Artists, One Variable',
                 fluidRow(
                   column(3, selectizeInput(
                     inputId = "artistTOL",
                     label = "Search for artist #1",
                     choices = NULL
                   )),
                   column(3, selectizeInput(
                     inputId = "artistTOR",
                     label = "Search for artist #2",
                     choices = NULL
                   )),
                   column(3, selectizeInput(
                     inputId = "statTO",
                     label = "Statistic",
                     choices = stats,
                     selected = "energy"
                   )),
                   column(3, actionButton(
                     inputId = "generateTO",
                     label = "Submit!"
                   ))
                 ), br(), 
                 h5('Choose your artists and variable above. Then hit Submit and watch 
                    your plots appear!'), br(),
                 textOutput('ntracksTO'),
                 fluidRow(
                   column(6, plotOutput(outputId = "graphTOL")),
                   column(6, plotOutput(outputId = "graphTOR"))
                 )
                 
               )
                 ),
    
    # Variables Tab ----------------------------
    
    tabPanel("Variable Info", br(), p(
      'Description of the variables is as follows. Most are taken verbatim from 
          the Spotify API Documentation on ', 
      a(href = 'https://developer.spotify.com/documentation/web-api/reference/#/operations/get-track',
        'Getting Tracks'),
      'and ',
      a(href = 'https://developer.spotify.com/documentation/web-api/reference/#/operations/get-several-audio-features',
        'Getting Track Features', .noWS = 'outside'), '.', noWS = c('after-begin', 'before-end')
    ), DTOutput('variables'))
    
  )
  
   )

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  waiter_hide()
  wOT <- Waiter$new(id = c('graphOTL', 'graphOTR'))
  wTO <- Waiter$new(id = c('graphTOL', 'graphTOR'))
  
  updateSelectizeInput(session, 'artistOT', choices = data$artist_name, 
                       server = TRUE, selected = "Taylor Swift")
  updateSelectizeInput(session, 'artistTOL', choices = data$artist_name, 
                       server = TRUE, selected = "Taylor Swift")
  updateSelectizeInput(session, 'artistTOR', choices = data$artist_name, 
                       server = TRUE, selected = "Imagine Dragons")
  
  # Server-side rendering for the welcome page
  
  output$headphones <- renderImage({
    tmpfile <- headphones %>%
      image_resize('500') %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    list(src = tmpfile, contentType = "image/jpeg")
  }, deleteFile = FALSE)
  
  
  
  
  # Visualizations page ----------------------------
  
  # One Artist, Two Variables --------------------------------------------------
  
  # Create empty holder for reactive values
  reactivesOT <- reactiveValues()
  
  # Waits for the action button, then subsets data, prepares messages, and stores 
  # reactive values for use later
  observeEvent(input$generateOT, {
   
    subset <- data %>% 
      select(artist_name, input$statOTL, input$statOTR) %>% 
      filter(artist_name == input$artistOT)
    
    n_tracks <- paste("There are", nrow(subset), "total tracks for", input$artistOT)
    
    reactivesOT$ntracksOT = n_tracks
    reactivesOT$subsetOT = subset
    reactivesOT$statOTL = input$statOTL
    reactivesOT$statOTR = input$statOTR
    reactivesOT$artistOT = input$artistOT
  })

  # Output for the left graph on O-T page
  output$graphOTL <- renderPlot({
    wOT$show()
    req(reactivesOT$statOTL)
    plots_ovar(reactivesOT$subsetOT, reactivesOT$statOTL, reactivesOT$artistOT, 'red4')
  
  })
  
  # Output for the right graph on O-T page
  output$graphOTR <- renderPlot({
    wOT$show()
    req(reactivesOT$statOTR)
    plots_ovar(reactivesOT$subsetOT, reactivesOT$statOTR, reactivesOT$artistOT, 'forestgreen')
  })

  
  # States total number of tracks for an artist
  output$ntracksOT <- renderText({
    reactivesOT$ntracksOT
  })


  
  # Two Artists, One Variable --------------------------------------------------
  
  # Create empty holder for reactive values
  reactivesTO <- reactiveValues()
  
  # Waits for the action button, then subsets data, prepares messages, and stores 
  # reactive values for use later
  observeEvent(input$generateTO, {
    
    subset <- data %>% 
      select(artist_name, input$statTO) %>% 
      filter(artist_name == input$artistTOL | artist_name == input$artistTOR)
    
    n_tracks <- paste("There are ", nrow(subset %>% filter(artist_name == input$artistTOL)), 
                      " total tracks for ", input$artistTOL, ' and ', 
                      nrow(subset %>% filter(artist_name == input$artistTOR)), ' total 
                      tracks for ', input$artistTOR, '.', sep = "")
    
    reactivesTO$ntracksTO = n_tracks
    reactivesTO$subsetTO = subset
    reactivesTO$statTO = input$statTO
    reactivesTO$artistTOL = input$artistTOL
    reactivesTO$artistTOR = input$artistTOR
  })
  
  # Output for the left graph on T-O page
  output$graphTOL <- renderPlot({
    wTO$show()
    req(reactivesTO$artistTOL)
    plots_ovar(reactivesTO$subsetTO, reactivesTO$statTO, reactivesTO$artistTOL, 'darkorchid4')
    
  })
  
  # Output for the right graph on T-O page
  output$graphTOR <- renderPlot({
    wTO$show()
    req(reactivesTO$artistTOR)
    plots_ovar(reactivesTO$subsetTO, reactivesTO$statTO, reactivesTO$artistTOR, 'darkorange3')
  })
  
  
  # States total number of tracks for an artist
  output$ntracksTO <- renderText({
    reactivesTO$ntracksTO
  })
  
  
  
  
  
  # Server-side rendering for Variables page
  output$variables <- renderDT({
    vars <- read_tsv('variables.csv')
    vars %>% datatable(options = list(
      paging = FALSE,
      searching = FALSE,
      order = list(0, 'asc'),
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