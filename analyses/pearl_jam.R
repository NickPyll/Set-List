# This code analyzed the band's set list leading up to the Baltimore
# show, as well as throughout the entire Dark Matter Tour

# set up environment ----

source("util/setup.R")
source("util/setlist_functions.R")
library(tmap)
library(sf)
library(RColorBrewer)

# load data ----

# bring album tracklists in
source("data/pj_albums.R")

# my shows
np.shows <-
  as.Date(
    c(
      "1996-10-04", "1998-08-31",
      "2000-08-03", "2003-04-15",
      "2016-04-21",
      "2022-09-22", "2024-09-12",
      "2025-05-11", "2025-05-13"
    )
  )

# Pull data from setlist.fm API
# Band Name (required)
# Musicbrainz Identifier (if known -- else will prompt)
pj.setlists <- getSongInfo(
  "Pearl Jam",
  "83b9cbe7-9857-49e2-ab8e-b57b01038103"
) |>
  full_join(
    bind_rows(mget(ls(pattern = "^album."))),
    by = "song_name"
  ) |>
  mutate(
    album = if_else(is.na(album) & cover == TRUE, "Other", album),
    album = factor(album, levels = levels.album),
    np_show = ifelse(as.Date(event_date) %in% np.shows, 1, 0)
  ) |>
  select(
    "tour",
    "event_id", "event_date", "event_info",
    "np_show",
    "venue_name", "city", "state", "country", "latitude", "longitude",
    "tape",
    "song_position", "song_name", "song_tag",
    "album", "release_date", "album_detail", "cover", "original_artist"
  ) 

pj.setlists <-
  pj.setlists |>
  filter(song_name != "Dark Entree") |>
  select(-song_position) |>
  group_by(event_id) |>
  mutate(song_position = row_number()) |>
  ungroup() |> 
  arrange(event_date) |>
  mutate(event_id = dense_rank(event_date))

# songs that need to be cleaned up
pj.setlists |>
  filter(
    is.na(album),
    cover == "FALSE"
  ) |>
  distinct(song_name) |>
  View()

pj.agg <- getSongAgg(pj.setlists) |>
  full_join(
    bind_rows(mget(ls(pattern = "^album."))),
    by = "song_name"
  ) |>
  mutate(
    album = if_else(!is.na(album), album, "Cover Song")
  )

pj.full <-
  pj.setlists |>
  left_join(
    pj.agg |>
      select(
        song_name,
        first_played,
        avg_song_position,
        times_played, num_events,
        p, p_bin
      ),
    by = "song_name"
  ) |>
  select(
    event_id, event_date,
    venue_name, city, state, country, latitude, longitude,
    song_position, song_name,
    cover, original_artist,
    album, album_detail, release_date,
    first_played, times_played, num_events,
    avg_song_position,
    p, p_bin
  )

pj.full |>
   |> 
  View()

# write.csv(pj.full, "pearljam_2025-01-27.csv")

pj.full |>
  mutate(x = times_played - num_events) |>
  filter(x > 0) |>
  View()

# dark matter tour ----
# The Dark Matter Tour kicked off May 4, 2024
# and will end November 21, 2024


## prepare data ----
# filter to dark matter tour
dm.tour <- pj.setlists |>
  filter(
    event_date >= as.Date("2024-05-04"),
    event_date <= as.Date("2025-06-01")
  )

# what songs opened dark matter shows
dm.tour.openers <-
  dm.tour |>
  filter(song_name != "Dark Entree") |>
  group_by(event_id) |>
  filter(row_number() == 1) |>
  group_by(song_name) |>
  summarize(opener_count = n(), .groups = "drop") |>
  arrange(desc(opener_count))

# aggregate by song
dm.tour.agg <- getSongAgg(dm.tour) |>
  full_join(
    bind_rows(mget(ls(pattern = "^album."))),
    by = "song_name"
  ) |>
  mutate(
    song_type =
      as.factor(if_else(cover == "TRUE", "cov",
        if_else(album == "Dark Matter", "dm", "prev")
      )),
    album = if_else(!is.na(album), album, "Cover Song")
  )

## exploration ----

# whoa some interesting openers...what opened each show?
dm.tour |>
  filter(song_position == 1) |>
  select(song_name, event_date, venue_name, city, state, country) |>
  View()

# which albums are most played on this tour
dm.tour |>
  filter(album != "Cover Song") |>
  group_by(album) |>
  summarize(count = n()) |>
  ggplot(aes(x = reorder(album, count), y = count, fill = album)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(
    legend.position = "none"
  ) +
  scale_fill_manual(values = colors.album)

# song frequency on Dark Matter Tour
dm.tour.agg |>
  # filter(Cover == 'FALSE') |>
  filter(
    times_played >= 1,
    !is.na(song_type)
  ) |>
  ggplot(aes(x = reorder(song_name, times_played), y = p, fill = song_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    labels = c("Cover Song", "Dark Matter", "Previous Album"),
    values = c("#25591f", "#A42820", "grey70")
  ) +
  coord_flip() +
  theme(
    legend.position = c(.7, .2),
    legend.direction = "vertical",
    legend.title = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Will you hear your favorite Pearl Jam song?",
    subtitle = "Predictions for the 2025 Dark Matter tour^",
    caption = "^Source: All Dark Matter performances from setlist.fm"
  ) +
  scale_y_continuous(labels = scales::percent)

# songs that have not been played on this tour
dm.tour.agg |>
  filter(is.na(times_played)) |>
  select(album, song_name) |>
  View()

# songs rarely played on this tour
dm.tour.agg |>
  filter(times_played < 3) |>
  select(album, song_name) |>
  View()

## baltimore analysis ----

# songs played in Baltimore
dm.tour.agg |>
  # filter(Cover == 'FALSE') |>
  filter(
    times_played >= 1,
    !is.na(song_type)
  ) |>
  left_join(
    dm.tour |>
      filter(event_date == "2024-09-12") |>
      select(song_name) |>
      mutate(baltimore = 1),
    by = "song_name"
  ) |>
  mutate(baltimore = factor(if_else(!is.na(baltimore), "yes", "no"), levels = c("yes", "no"))) |>
  ggplot(aes(x = reorder(song_name, times_played), y = p, fill = baltimore)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    labels = c("Played in Baltimore", "Played elsewhere on DM Tour"),
    values = c("#241773", "grey70")
  ) +
  coord_flip() +
  theme(
    legend.position = c(.7, .2),
    legend.direction = "vertical",
    legend.title = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Baltimore September 12, 2024",
    caption = "^Source: All Dark Matter performances from setlist.fm"
  ) +
  scale_y_continuous(labels = scales::percent)
# 1100 x 1900 <- good export dimensions


# setlist prediction ----
# Creating a process for predicting a setlist

## model setup and build ----
num.songs.played <-
  dm.tour |>
  group_by(event_id) |>
  summarize(num.songs.played = n()) |>
  summarize(num.songs.played = round(mean(num.songs.played), 0)) |>
  pull()

song.names <-
  dm.tour.agg |>
  filter(!is.na(times_played)) |>
  select(song_name) |>
  filter(song_name != "Dark Entree") |>
  pull()

weights <-
  dm.tour.agg |>
  filter(
    !is.na(times_played),
    song_name != "Dark Entree"
  ) |>
  select(p) |>
  pull()

predicted.setlist <-
  sample(song.names, num.songs.played, FALSE, weights) |>
  as.data.frame() |>
  setNames("song_name") |>
  inner_join(
    dm.tour.agg |>
      select(song_name, avg_song_position, p),
    by = "song_name"
  ) |>
  arrange(avg_song_position)

# predicted.setlist |> View()

## baltimore prediction ----

# from 9-11-24
predicted.setlist <-
  c(
    "Long Road", "Present Tense", "Go", "Elderly Woman Behind the Counter in a Small Town",
    "Given to Fly", "Lukin", "Wreckage", "Daughter", "Dark Matter", "MFC", "Even Flow",
    "Out of My Mind", "Won't Tell", "Waiting for Stevie", "Once", "Rearviewmirror",
    "Gimme Some Truth", "Do the Evolution", "Song of Good Hope", "Crazy Mary", "State of Love and Trust",
    "Alive", "Baba O'Riley", "Setting Sun", "Yellow Ledbetter"
  )

actual.setlist <-
  dm.tour |>
  filter(event_date == "2024-09-12") |>
  mutate(correct = if_else(song_name %in% predicted.setlist, "yes", "no")) |>
  inner_join(dm.tour.agg, by = "song_name") |>
  rename(actual_played = song_name) |>
  bind_cols(predicted_played = predicted.setlist) |>
  select(actual_played, predicted_played, correct)

# show openers ----

# what songs opened all time
pj.setlists |>
  filter(song_position == 1) |>
  group_by(song_name) |>
  summarize(count = n(), .groups = "drop") |>
  arrange(desc(count)) |>
  View()

pj.setlists |>
  filter(song_position == 1) |>
  group_by(song_name) |>
  summarize(count = n(), .groups = "drop") |>
  ggplot(aes(x = reorder(song_name, count), y = count)) +
  geom_bar(stat = "identity", fill = "#217428") +
  coord_flip() +
  labs(
    x = NULL,
    y = NULL,
    title = "Show Openers Frequency (All-Time)",
    caption = "^Source: All Pearl Jam performances from setlist.fm"
  )

# whoa some interesting openers...what opened each show?
pj.setlists |>
  filter(song_position == 1) |>
  select(song_name, event_date, venue_name, city, state, country) |>
  View()

# Album Presence on Tour -----

ggplotly(
  pj.setlists |>
    filter(
      !is.na(album)
      # ,album != 'Other'
    ) |>
    mutate(month = floor_date(as.Date(event_date), "month")) |>
    group_by(
      month,
      album
    ) |>
    summarize(count = n(), .groups = "drop_last") |>
    mutate(freq = count / sum(count)) |>
    ungroup() |>
    ggplot(aes(x = month, y = freq, group = album, color = album, linetype = album)) +
    geom_line(linewidth = 1) +
    theme(
      legend.title = element_blank(),
      legend.direction = "vertical",
      legend.position = "right"
    ) +
    scale_color_manual(values = colors.album) +
    scale_linetype_manual(values = linetypes.album) +
    scale_y_continuous(labels = scales::percent)
)

pj.setlists |>
  filter(
    !is.na(album)
    # ,album != 'Other'
  ) |>
  mutate(month = floor_date(as.Date(event_date), "month")) |>
  group_by(
    month,
    album
  ) |>
  summarize(count = n(), .groups = "drop_last") |>
  mutate(freq = count / sum(count)) |>
  ungroup() |>
  ggplot(aes(x = month, y = freq, group = album, color = album, linetype = album)) +
  geom_line(linewidth = 1) +
  theme(
    legend.title = element_blank(),
    legend.direction = "vertical",
    legend.position = "right"
  ) +
  scale_color_manual(values = colors.album) +
  scale_linetype_manual(values = linetypes.album) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~album)

pj.setlists |>
  filter(
    !is.na(album)
    # ,album != 'Other'
  ) |>
  mutate(month = floor_date(as.Date(event_date), "month")) |>
  group_by(
    month,
    album
  ) |>
  summarize(count = n(), .groups = "drop_last") |>
  mutate(freq = count / sum(count)) |>
  ungroup() |>
  select(-count) |>
  pivot_wider(names_from = "album", values_from = "freq") |>
  clean_names() |>
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) |>
  mutate(total = ten + vs + vitalogy + no_code + yield + binaural + riot_act + pearl_jam + backspacer + lightning_bolt + gigaton + dark_matter + other) |>
  View()

# trying to make area plot of the above
# ggplot(aes(x = month, y = freq, fill = album)) +
# geom_area() +
# theme(
#   legend.title = element_blank(),
#   legend.direction = 'vertical',
#   legend.position = 'right'
# ) +
# scale_fill_manual(values = colors_album) +
# scale_y_continuous(labels = scales::percent)

# random set history questions ----

# when have these songs appeared on same set
# song.list <- c("Breath", "State of Love and Trust", "Chloe Dancer/Crown of Thorns")
# song.list <- c("Once", "Alive", "Footsteps")
# song.list <- c("Footsteps", "Crazy Mary")
# song.list <- c("Harvest Moon")
song.list <- c("Chloe Dancer/Crown of Thorns")


pj.setlists |>
  filter(song_name %in% song.list) |>
  group_by(event_id) |>
  summarize(count = n(), .groups = "drop") |>
  filter(count == length(song.list)) |>
  select(event_id) |>
  inner_join(pj.setlists, by = "event_id") |>
  distinct(event_date, venue_name, city) |>
  View()

# event uniqueness ----

dm.song.p <- dm.tour.agg |>
  select(p) |>
  mutate(p = if_else(is.na(p), .0001, p)) |>
  pull()

dm.events <- dm.tour |>
  distinct(event_id) |>
  pull()

# create empty df to store loop output
dm.event.uniqueness <-
  data.frame(
    event_id = character(),
    p_rare = double(),
    p_unlikely = double(),
    p_likely = double(),
    p_standards = double(),
    opener_count = double(),
    variance_p = double(),
    mean_p = double(),
    iteration = double()
  )

for (event in dm.events) {
  dm.event <- dm.tour |>
    filter(event_id == event) |>
    inner_join(
      dm.tour.agg |>
        select(song_name, p),
      by = "song_name"
    ) |>
    select(p) |>
    pull()

  event.df <-
    dm.tour |>
    filter(event_id == event) |>
    inner_join(
      dm.tour.agg |>
        select(song_name, p_bin),
      by = "song_name"
    ) |>
    group_by(event_id, p_bin) |>
    summarize(count = n(), .groups = "drop_last") |>
    mutate(freq = count / sum(count)) |>
    ungroup() |>
    select(-count) |>
    mutate(p_bin = paste("p", p_bin)) |>
    pivot_wider(names_from = p_bin, values_from = freq) |>
    clean_names() |>
    bind_cols(
      dm.tour |>
        filter(event_id == event) |>
        filter(song_position == 1) |>
        select(song_name) |>
        inner_join(dm.tour.openers, by = "song_name") |>
        select(opener_count)
    ) |>
    select(
      event_id,
      p_rare, p_unlikely, p_likely, p_standards,
      opener_count
    )

  for (iteration in 1:100) {
    dm.sample.p <-
      sample(song.names, num.songs.played, FALSE, weights) |>
      as.data.frame() |>
      setNames("song_name") |>
      inner_join(
        dm.tour.agg,
        by = "song_name"
      ) |>
      select(p) |>
      pull()

    var_p <- var.test(dm.sample.p, dm.event, alternative = "two.sided")$p.value
    t_p <- t.test(dm.sample.p, dm.event, var.equal = TRUE)$p.value

    iteration.df <-
      event.df |>
      bind_cols(
        "variance_p" = var_p,
        "mean_p" = t_p
      ) |>
      mutate(iteration = iteration)

    dm.event.uniqueness <-
      dm.event.uniqueness |>
      bind_rows(iteration.df)

    rm(var_p, t_p)
  }
}

dm.event.uniqueness <-
  dm.event.uniqueness |>
  select(-iteration) |>
  group_by(event_id) |>
  summarise_all(mean, .groups = "drop") |>
  inner_join(
    dm.tour |>
      distinct(event_id, event_date, city, state, venue_name),
    by = "event_id"
  ) |>
  mutate(event_id = as.numeric(event_id)) |>
  arrange(event_id) |>
  select(
    event_date,
    city, state, venue_name,
    variance_p, mean_p,
    p_rare, p_unlikely, p_likely, p_standards,
    opener_count
  ) |>
  mutate(
    dim_rare = 1 - p_rare,
    dim_opener = 1 / opener_count,
    dim_var = 1 - variance_p,
    dim_mean = 1 - mean_p,
    uniqueness_factor = dim_rare * dim_opener * dim_var * dim_mean
  )

dm.event.uniqueness |>
  select(event_date, venue_name, city, state, uniqueness_factor) |>
  arrange(desc(uniqueness_factor)) |>
  View()

dm.tour |>
  filter(city == "Las Vegas", event_date == "2024-05-16") |>
  View()

dm.tour |>
  inner_join(
    dm.tour.agg |>
      select(song_name, p),
    by = "song_name"
  ) |>
  mutate(event_id = paste0(event_id, ": ", venue_name)) |>
  select(event_id, p) |>
  ggplot(aes(x = p)) +
  geom_density() +
  facet_wrap(. ~ event_id)

# np history ----

np.setlists <-
  pj.setlists |>
  filter(np_show == 1)

np.agg <- getSongAgg(np.setlists)

pj.agg |>
  left_join(
    np.agg |>
      select(song_name) |>
      mutate(np_show = 1),
    by = "song_name"
  ) |>
  filter(
    times_played >= 1,
    cover == FALSE
  ) |>
  mutate(np_show = factor(if_else(!is.na(np_show), "yes", "no"), levels = c("yes", "no"))) |>
  ggplot(aes(x = reorder(song_name, times_played), y = p, fill = np_show)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    labels = c("NP has seen", "NP has not seen"),
    values = c("#241773", "grey70")
  ) +
  coord_flip() +
  theme(
    legend.position = c(.7, .2),
    legend.direction = "vertical",
    legend.title = element_blank()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Has NP seen song live?",
    caption = "^Source: All Pearl Jam performances from setlist.fm"
  ) +
  scale_y_continuous(labels = scales::percent)


# map
pj.locations <-
  pj.setlists |>
  filter(!is.na(latitude)) |>
  mutate(
    event_year = year(event_date),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) |>
  distinct(
    event_year, event_date,
    np_show,
    venue_name,
    city, state, country,
    latitude, longitude
  ) |>
  mutate(is_pj = 1)

pj.locations |>
  filter(state == "Texas") |>
  View()

pj.locations |>
  filter(state == "North Carolina") |>
  View()

pj.locations_L10Y <-
  pj.locations |>
  filter(event_year >= 2014)

# |>
#   st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

# tmap_mode("view")
# tmap_options(basemaps = c(leaflet::providers$Stamen.TonerLite,
#                           leaflet::providers$OpenStreetMap,
#                           leaflet::providers$Esri.WorldGrayCanvas))

# pj.map <-
#   tm_shape(pj.locations, name = 'Pearl Jam Show Locations') +
#   tm_dots('is_pj', title = 'PJ', palette = c('black'), size = 0.1, legend.show = FALSE,
#           id = 'event_date')

# pj.map

base_map <- map_data("world")

ggplot() +
  stat_density2d(data = pj.locations, aes(x = longitude, y = latitude, fill = ..level..), alpha = 0.5, geom = "polygon") +
  geom_path(data = base_map, aes(x = long, y = lat, group = group), color = "grey50") +
  geom_point(data = pj.locations, aes(x = longitude, y = latitude), color = "black", size = 2) +
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
  coord_cartesian(xlim = c(-130, -60), ylim = c(25, 55)) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 24)
  ) +
  labs(title = "Locations of North American Pearl Jam Shows")

ggplot() +
  stat_density2d(data = pj.locations_L10Y, aes(x = longitude, y = latitude, fill = ..level..), alpha = 0.5, geom = "polygon") +
  geom_path(data = base_map, aes(x = long, y = lat, group = group), color = "grey50") +
  geom_point(data = pj.locations_L10Y, aes(x = longitude, y = latitude), color = "black", size = 2) +
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
  coord_cartesian(xlim = c(-130, -60), ylim = c(25, 55)) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 24)
  ) +
  labs(
    title = "Locations of North American Pearl Jam Shows",
    subtitle = "2014 - 2024"
  )

ggplot() +
  stat_density2d(data = pj.locations, aes(x = longitude, y = latitude, fill = ..level..), alpha = 0.5, geom = "polygon") +
  geom_path(data = base_map, aes(x = long, y = lat, group = group), color = "grey50") +
  geom_point(data = pj.locations, aes(x = longitude, y = latitude), color = "black", size = 1) +
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
  coord_cartesian(xlim = c(-159, 175), ylim = c(-45, 65)) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 24)
  ) +
  labs(title = "Locations of Pearl Jam Shows")
