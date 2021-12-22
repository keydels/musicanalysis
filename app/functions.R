# Create a plot based on inputted statistic and artist
plots_ovar <- function(sub_data, stat, artist, fill_color) {
  
  if (stat %in% c('explicit', 'disc_number', 'track_number', 'key', 'mode', 
                  'year', 'time_signature')) {
    sub_data %>% 
      filter(artist_name == artist) %>% 
      ggplot(aes_string(x = stat)) +
      geom_bar(color = "black", fill = fill_color) +
      labs(title = paste(str_to_sentence(stat), "for songs by", artist)) +
      theme(
        plot.background = element_rect(fill = 'black', color = 'black'),
        axis.text = element_text(color = "#DEDEDE"),
        axis.title = element_text(color = "#CCCCCC"),
        plot.title = element_text(color = "#CCCCCC"),
        panel.background = element_rect(fill = "grey70")
      )
  }
  else {
    if (stat %in% c('popularity', 'loudness')) {binwidth <- 5}
    else if (stat == 'tempo') {binwidth <- 10}
    else if (stat == 'duration_s') {binwidth <- 30}
    else {binwidth <- 0.1}
    
    sub_data %>% 
      filter(artist_name == artist) %>% 
      ggplot(aes_string(x = stat)) +
      geom_histogram(color = "black", fill = fill_color, binwidth = binwidth) +
      labs(title = paste(str_to_sentence(stat), "for songs by", artist)) +
      theme(
        plot.background = element_rect(fill = 'black', color = 'black'),
        axis.text = element_text(color = "#DEDEDE"),
        axis.title = element_text(color = "#CCCCCC"),
        plot.title = element_text(color = "#CCCCCC"),
        panel.background = element_rect(fill = "grey70")
      )
    
  }
}