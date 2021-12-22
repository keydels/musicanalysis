library(feather)
library(tidyverse)

my_files <- list.files(path=here::here('original_data/'),
                       pattern="*.csv", full.names = TRUE)
spotify <- read_csv(my_files)

# Adds a column for just the year, the track duration in seconds, and removes duplicates
spotify <- spotify %>% 
  mutate(year = as.factor(substr(release_date, 1, 4)),
         duration_s = as.duration(duration) / 1000) %>% 
  distinct(track_uri, .keep_all = TRUE)

# Remove empty column
spotify <- spotify[-20]

# Write feather file
write_feather(spotify, 'app/data/spotify.feather')
