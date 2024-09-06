# This code analyzed the band's set list leading up to the Baltimore
# show, as well as throughout the entire Dark Matter Tour

# Setup ----

source('util/setup.R')
source('util/setlist_functions.R')

# Load Data ----

# Pull data from setlist.fm API
# Band Name (required)
# Musicbrainz Identifier (if known -- else will prompt)
pj <- getSongInfo(
  'Pearl Jam',
  '83b9cbe7-9857-49e2-ab8e-b57b01038103')

source("data/pj_albums.R")

pj |> 
  mutate(EventID = as.numeric(EventID)) |> 
  View()
# Dark Matter Tour ----
# The Dark Matter Tour kicked off May 4, 2024
# and will end November 21, 2024

dm <- pj |> 
  filter(
    EventDate >= as.Date('2024-05-04'),
    EventDate <= as.Date('2024-09-21'))

dm <- getSongAgg(dm)

dm <-
  dm |> 
  left_join(
    album.dark_matter, by = 'SongName') |> 
  # mutate(across(everything(), ~replace_na(.x, 0))) 
  mutate(
    SongType = 
      as.factor(if_else(Cover == 'TRUE', 'cov',
                if_else(album == 'Dark Matter', 'dm', 'prev'))),
    OrigArtist = if_else(Cover == FALSE, 'Pearl Jam', OrigArtist))

dm |> 
  # filter(Cover == 'FALSE') |> 
  # filter(TimesPlayed > 1) |> 
  ggplot(aes(x = reorder(SongName, TimesPlayed), y = p, fill = SongType)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    labels = c('Cover Song', 'Dark Matter', 'Previous Album'),
    values = c('#25591f', '#A42820', 'grey70')) +
  coord_flip() +
  theme(
    legend.position = c(.7, .2),
    legend.direction = 'vertical',
    legend.title = element_blank()) +
  labs(
    x = NULL,
    y = NULL,
    title = 'Will you hear your favorite Pearl Jam song?',
    subtitle = 'Predictions for the 2024 Dark Matter tour^',
    caption = '^Source: All 2024 performances from setlist.fm'
  ) +
  scale_y_continuous(labels = scales::percent)
# 1100 x 1700 <- good export dimensions

# Album Presence on Tour -----

pj_album <-
  pj |> 
  left_join(
    bind_rows(mget(ls(pattern = "^album."))),
    by = 'SongName'
  ) |> 
  mutate(album = if_else(is.na(album) & Cover == TRUE, 'Other', album)) |> 
  mutate(album = factor(album, levels = levels_album))

# pj_album |> 
#   filter(
#     is.na(album),
#     Cover == 'FALSE') |> 
#   distinct(SongName) |> 
#   View()

pj_album |> 
  filter(!is.na(album)) |> 
  # filter(album %in% c('Ten', 'Vs', 'Yield')) |> 
  mutate(month = floor_date(as.Date(EventDate), 'month')) |> 
  group_by(
    month, 
    album) |> 
  summarize(count = n(), .groups = 'drop_last') |> 
  mutate(freq = count / sum(count)) |> 
  ungroup() |> 
  ggplot(aes(x = month, y = freq, group = album, color = album, linetype = album)) +
  geom_line(linewidth = 1) +
  theme(
    legend.title = element_blank(),
    legend.direction = 'vertical',
    legend.position = 'right'
  ) +
  scale_color_manual(values = colors_album) +
  scale_linetype_manual(values = linetypes_album) +
  scale_y_continuous(labels = scales::percent) 








# population <- dm |> 
#   select(p) |> 
#   pull()

# event_id_list <- pj_raw[[1]] |> distinct(EventID) |> pull()

# event_uniqueness <- 
#   data.frame(
#     EventID = character(),
#     variance_p = double(),
#     mean_p = double()
#   )

# for (i in event_id_list){

#   sample <- pj_raw[[1]] |> 
#     filter(EventID == i) |> 
#     inner_join(pre_bmore_set |> 
#       select(SongName, p),
#     by = 'SongName') |> 
#     select(p) |> 
#     pull()
  
#   var_p <- var.test(population, sample, alternative = "two.sided")$p.value
#   t_p <- t.test(population, sample, var.equal = TRUE)$p.value

#   df <- data.frame(
#     'EventID' = i, 
#     'variance_p' = var_p,
#     'mean_p' = t_p)
  
#   event_uniqueness <- 
#     event_uniqueness |> 
#     bind_rows(df)

#   rm(i, var_p, t_p, df)

# }

# event_uniqueness <-
#   event_uniqueness |> 
#   inner_join(
#     pj_raw[[1]] |>
#       inner_join(
#         song_ct |>
#           select(SongName, FreqBin),
#         by = 'SongName') |>
#       group_by(EventID, FreqBin) |>
#       summarize(count = n()) |> 
#       mutate(freq = count / sum(count)) |> 
#       ungroup() |> 
#       select(-count) |> 
#       pivot_wider(names_from = FreqBin, values_from = freq),
#     by = 'EventID') |> 
#   inner_join(
#     pj_raw[[1]] |> 
#       distinct(EventID, EventDate, City, State, VenueName),
#     by = 'EventID'
#   ) |> 
#   select(
#     EventID, EventDate, 
#     City, State, VenueName,
#     variance_p, mean_p,
#     Rare, Unlikely, Likely, Standards
#   )


# pj_raw[[1]] |> 
#   filter(EventID == 13) |> 
#   inner_join(
#     pre_bmore_set |>
#       select(SongName, p),
#     by = 'SongName') |> 
#   select(SongName, p) |> 
#   View()



# pj_raw[[1]] |> 
#   inner_join(
#     pre_bmore_set |>
#       select(SongName, p),
#     by = 'SongName') |> 
#   mutate(EventID = paste0(EventID, ': ', VenueName)) |> 
#   select(EventID, p) |> 
#   ggplot(aes(x = p)) +
#   geom_density() +
#   facet_wrap(.~EventID) 
# +
#   geom_density(data = pre_bmore_set, aes(x = p))

# sample <- pj_raw[[1]] |> 
#   filter(EventID == 7) |> 
#   inner_join(pre_bmore_set |> 
#     select(SongName, p),
#   by = 'SongName') |> 
#   select(p) |> 
#   pull()

# var_p <- var.test(population, sample, alternative = "two.sided")$p.value
# t_p <- t.test(population, sample, var.equal = TRUE)$p.value


# pj_raw[[1]] |> 
#   inner_join(
#     pre_bmore_set |>
#       select(SongName, p),
#     by = 'SongName') |> 
#   group_by(EventID, EventDate, Country, State, City, VenueName) |> 
#   summarize(unique_show_factor = mean(p)) |> 
#   arrange(desc(unique_show_factor)) |> 
#   View()
    
# pj_raw[[1]] |> 
#   filter(EventID == 11) |> 
#   inner_join(
#     pre_bmore_set |>
#       select(SongName, p),
#     by = 'SongName') |> 
#   View()
    
# pj_raw[[1]] |> 
#   filter(EventID == 2) |> 
#   inner_join(
#     pre_bmore_set |>
#       select(SongName, p),
#     by = 'SongName') |> 
#   View()
  