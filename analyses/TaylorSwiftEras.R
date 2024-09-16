# This code is used to analyze Taylor Swift's Eras Tour

# Setup ----

source('util/setup.R')
source('util/setlist_functions.R')

# Load Data ----

# Pull data from setlist.fm API
# Band Name (required)
# Musicbrainz Identifier (if known -- else will prompt)
tswift_setlists <- getSongInfo(
  'Taylor Swift', 
  '20244d07-534f-4eff-b4d4-930878889970') |>
  # Taylor Swift sets are not correctly loading in due to how the recorded intro is coded in the API
  # Remving those '' songs and re-numbering the song position
  filter(song_name != '') |> 
  group_by(event_id) |> 
  mutate(song_position = row_number()) |> 
  ungroup() |> 
  select(
    'event_id', 'event_date',
    'venue_name', 'city', 'state', 'country', 'latitude', 'longitude',
    'song_position', 'song_name', 'cover', 'original_artist') 

# Eras Tour ----
# The Eras Tour kicked off March 17, 2023
# and will end December 8, 2024

# filter to eras tour
eras_tour <- tswift_setlists |> 
  filter(
    event_date >= as.Date('2023-03-17'),
    event_date <= as.Date('2024-12-08'))

# what songs opened eras shows
eras_tour |> 
  filter(song_position == 1) |> 
  group_by(song_name) |> 
  summarize(count = n(), .groups = 'drop') |> 
  arrange(desc(count)) |> 
  View()

# aggregate by song
eras_tour_agg <- getSongAgg(eras_tour) 

# song frequency on Eras Tour
eras_tour_agg |> 
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
    title = 'Will you hear your favorite Taylor Swift song?',
    subtitle = 'Predictions for the Eras Tour^',
    caption = '^Source: All Eras performances from setlist.fm'
  ) +
  scale_y_continuous(labels = scales::percent)
# 1100 x 3800 <- good export dimensions
