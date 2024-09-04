library(tidyverse)
library(ggthemes)
library(rvest)
library(knitr)
library(httr)
library(jsonlite)
library(scales)
library(dplyr)
library(magrittr)

source('setlist_functions_old.R')

pre_raleigh_set <- getSongInfo('Cake')

pre_raleigh_set %<>% 
  mutate(p = as.numeric(gsub('%', '', ProbPlay)) / 100)

song_ct <-
  pre_raleigh_set |> 
  mutate(SongType = as.factor(if_else(Cover == 'TRUE', 'cov', 'orig'))) |> 
  mutate(SongType = if_else(SongName == 'Jolene', 'cov', SongType)) |> 
  filter(!SongName %in% c('Tree Giveaway Intermission', 'Tree Giveaway (gave ponderosa pine to Dani)')) |> 
  mutate(SongName = if_else(SongName %in% c("Vince DiCola - War (Theme From 'Rocky IV')", 'War (Vince DeCola track)'), 'War', SongName)) |> 
  mutate(SongType = if_else(SongName == 'War', 'cov', SongType)) 

song_count <- song_ct |> tally() |> pull()

song_ct <-
  song_ct |> 
  group_by(SongName, SongType) |> 
  summarize(TimesPlayed = sum(TimesPlayed),
            .groups = 'drop') |> 
  mutate(p = TimesPlayed / 20)

song_ct |> 
  ggplot(aes(x = reorder(SongName, TimesPlayed), y = p, fill = SongType)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    labels = c('Cover Song', 'Original'),
    values = c('#25591f', 'grey70')) +
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
    title = 'Will you hear your favorite Cake song?',
    subtitle = 'Predictions for the 2024 tour^',
    caption = '^Based on last 20 performances scraped from setlist.fm'
  ) +
  scale_y_continuous(labels = scales::percent)
