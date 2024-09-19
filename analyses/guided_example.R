# Template

# Load in packages and functions
source('util/setup.R')
source('util/setlist_functions.R')

# Choose a band! Here are a few options if you're having trouble
band <- 'Old 97s'
# band <- 'Taylor Swift'

# Return MBID (Musicbrainz identifier)
mbid <- getArtistInfo(band)

# Pulls the band's entire live history availble on the site
tour.history <- getSongInfo(band, mbid)

# Often you want to find information about a specific, current tour
tour_start_date <- '2024-04-03'
tour_end_date <- '2024-12-31'

current.tour <- 
  tour.history |> 
  filter(
    event_date >= as.Date(tour_start_date),
    event_date <= as.Date(tour_end_date))

# What songs did they open the show with?
current.tour |> 
  filter(song_position == 1) |> 
  group_by(song_name) |> 
  summarize(opener_count = n(), .groups = 'drop') |> 
  arrange(desc(opener_count)) 

# Use the getSongAgg function to aggregate the song data for the tour
current.tour.agg <- getSongAgg(current.tour) 

# Visualize song frequency
current.tour.agg |> 
  filter(times_played >= 1) |> 
  ggplot(aes(x = reorder(song_name, times_played), y = p)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(
    legend.position = c(.7, .2),
    legend.direction = 'vertical',
    legend.title = element_blank()) +
  labs(
    x = NULL,
    y = NULL,
    title = paste0('Will you hear your favorite ', band, 'song?'),
    subtitle = 'Predictions for the current tour^',
    caption = '^Source: All current tour performances from setlist.fm'
  ) +
  scale_y_continuous(labels = scales::percent)
