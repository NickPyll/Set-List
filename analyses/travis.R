# This code analyzed the band's set list leading up to the Raleigh
# show, as well as throughout the entire Raze the Bar tour

# Setup ----

source("util/setup.R")
source("util/setlist_functions.R")

# Load Data ----

travis.setlists <-
  getSongInfo(
    "Travis",
    "22a40b75-affc-4e69-8884-266d087e4751"
  ) |>
  mutate(song_name = if_else(song_name == "Do-Re-Mi" & cover == FALSE, "DELETEME", song_name)) |>
  mutate(song_name = str_remove_all(song_name, '"')) |>
  mutate(
    song_name =
      if_else(grepl("Let It Snow", song_name), "Christmas Medley",
        if_else(grepl("J. Smith", song_name), "The Ballad of J. Smith",
          song_name
        )
      )
  ) |>
  filter(
    song_name != "DELETEME",
    song_name != "",
    !grepl("Rocky", song_name),
    !grepl("Cheers", song_name),
    song_name != "Gonna Fly Now"
  )

# filter to Raze the Bar tour
raze.tour <- travis.setlists |>
  filter(
    event_date >= as.Date("2024-08-26")
  )

# aggregate by song
raze.tour.agg <- getSongAgg(raze.tour) |>
  arrange(times_played)

# song frequency on Raze Tour
raze.tour.agg |>
  ggplot(aes(x = reorder(song_name, times_played), y = p)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(
    legend.position = c(.7, .2),
    legend.direction = "vertical",
    legend.title = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Will you hear your favorite Travis song?",
    subtitle = "Predictions for the 2024 Raze the Bar tour^",
    caption = "^Source: All Raze the Bar performances from setlist.fm"
  ) +
  scale_y_continuous(labels = scales::percent)
