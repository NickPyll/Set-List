# This code analyzed the band's set list leading up to the Raleigh show

# Setup ----

source('util/setup.R')
source('util/setlist_functions.R')

# Load Data ----

# Pull data from setlist.fm API
# Band Name (required)
# Musicbrainz Identifier (if known -- else will prompt)
old97s.setlists <- getSongInfo(
  'Old 97s', '8dd216da-6db2-4130-9a7c-ece756d1394d')

old97s.setlists |> distinct(event_date) |> View()

old97s.tour <-
  old97s.setlists |>
  filter(
    event_date >= as.Date('2024-04-03'))

# what songs opened shows on this tour
old97s.tour.openers <-
  old97s.tour |> 
  filter(song_position == 1) |> 
  group_by(song_name) |> 
  summarize(opener_count = n(), .groups = 'drop') |> 
  arrange(desc(opener_count)) 

old97s.tour.agg <- getSongAgg(old97s.tour)

# song frequency on this tour
old97s.tour.agg |> 
  filter(
    times_played >= 1) |> 
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
    title = 'Will you hear your favorite Old 97s song?',
    subtitle = 'Predictions for the Raleigh show^',
    caption = '^Source: All 2024 performances from setlist.fm'
  ) +
  scale_y_continuous(labels = scales::percent)
