library(tidyverse)
library(ggthemes)
library(rvest)
library(knitr)
library(httr)
library(jsonlite)
library(scales)
library(dplyr)
library(magrittr)

source('functions/setlist_functions.R')

# Pull data from setlist.fm API
# Band Name (required)
# Musicbrainz Identifier (if known -- else will prompt)
# Number of shows to analyze (will prompt if left blank)
pj_raw <- getSongInfo(
  'Pearl Jam',
  '83b9cbe7-9857-49e2-ab8e-b57b01038103')

# Function produces two outputs
pj_raw[[1]] |> View() # Setlist for each event with venue location and date
pj_raw[[2]] |> View() # Aggregated statistics at song level -- number of times played and average position

pre_bmore_set <-
  pj_raw[[2]] |> 
  mutate(p = as.numeric(gsub('%', '', ProbPlay)) / 100)

dark_matter <- data.frame(
  SongName = c("Scared of Fear", "React, Respond", "Wreckage", "Dark Matter", "Won't Tell", "Upper Hand", "Waiting for Stevie",
               "Running", "Something Special", "Got to Give", "Setting Sun")) |> 
  mutate(dm = 1)

song_ct <-
  pre_bmore_set |> 
  left_join(
    dark_matter, by = 'SongName') |> 
  mutate(across(everything(), ~replace_na(.x, 0))) |> 
  mutate(SongType = as.factor(if_else(Cover == 'TRUE', 'cov',
                                      if_else(dm == 1, 'dm', 'prev'))),
         OrigArtist = if_else(Cover == FALSE, 'Pearl Jam', OrigArtist),
         FreqBin = 
          case_when(
            p < .1 ~ 'Rare',
            p < .5 ~ 'Unlikely',
            p < .75 ~ 'Likely',
            p <= 1 ~ 'Standards'
          ))



song_count <- song_ct |> tally() |> pull()

song_ct |> 
  # filter(Cover == 'FALSE') |> 
  # filter(TimesPlayed > 1 | dm == 1) |> 
  ggplot(aes(x = reorder(SongName, TimesPlayed), y = p, fill = SongType)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    labels = c('Cover Song', 'Dark Matter', 'Previous Album'),
    values = c('#25591f', '#A42820', 'grey70')) +
  coord_flip() +
  theme_wsj() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey30", linewidth = 0.2),
    panel.grid.minor.x = element_blank(),
    title = element_text(size = 26, color = 'grey30'),
    text = element_text(color = 'grey40', family = 'Lato'),
    legend.position = c(.7, .2),
    legend.direction = 'vertical',
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.caption = element_text(color = 'grey40', family = 'Lato', size = 14)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = 'Will you hear your favorite Pearl Jam song?',
    subtitle = 'Predictions for the 2024 Dark Matter tour^',
    caption = '^Source: All 2024 performances from setlist.fm'
  ) +
  scale_y_continuous(labels = scales::percent)

# 1100 x 1700 <- good export dimensions

population <- pre_bmore_set |> 
  select(p) |> 
  pull()

event_id_list <- pj_raw[[1]] |> distinct(EventID) |> pull()

event_uniqueness <- 
  data.frame(
    EventID = character(),
    variance_p = double(),
    mean_p = double()
  )

for (i in event_id_list){

  sample <- pj_raw[[1]] |> 
    filter(EventID == i) |> 
    inner_join(pre_bmore_set |> 
      select(SongName, p),
    by = 'SongName') |> 
    select(p) |> 
    pull()
  
  var_p <- var.test(population, sample, alternative = "two.sided")$p.value
  t_p <- t.test(population, sample, var.equal = TRUE)$p.value

  df <- data.frame(
    'EventID' = i, 
    'variance_p' = var_p,
    'mean_p' = t_p)
  
  event_uniqueness <- 
    event_uniqueness |> 
    bind_rows(df)

  rm(i, var_p, t_p, df)

}

event_uniqueness <-
  event_uniqueness |> 
  inner_join(
    pj_raw[[1]] |>
      inner_join(
        song_ct |>
          select(SongName, FreqBin),
        by = 'SongName') |>
      group_by(EventID, FreqBin) |>
      summarize(count = n()) |> 
      mutate(freq = count / sum(count)) |> 
      ungroup() |> 
      select(-count) |> 
      pivot_wider(names_from = FreqBin, values_from = freq),
    by = 'EventID') |> 
  inner_join(
    pj_raw[[1]] |> 
      distinct(EventID, EventDate, City, State, VenueName),
    by = 'EventID'
  ) |> 
  select(
    EventID, EventDate, 
    City, State, VenueName,
    variance_p, mean_p,
    Rare, Unlikely, Likely, Standards
  )


pj_raw[[1]] |> 
  filter(EventID == 13) |> 
  inner_join(
    pre_bmore_set |>
      select(SongName, p),
    by = 'SongName') |> 
  select(SongName, p) |> 
  View()



pj_raw[[1]] |> 
  inner_join(
    pre_bmore_set |>
      select(SongName, p),
    by = 'SongName') |> 
  mutate(EventID = paste0(EventID, ': ', VenueName)) |> 
  select(EventID, p) |> 
  ggplot(aes(x = p)) +
  geom_density() +
  facet_wrap(.~EventID) 
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
  