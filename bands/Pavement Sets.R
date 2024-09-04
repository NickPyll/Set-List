library(tidyverse)
library(ggthemes)
library(rvest)
library(knitr)
library(httr)
library(jsonlite)
library(scales)
library(dplyr)


# source('setlist_functions_old.R')

pavementsets <- read_csv("data/pavementsets.csv") %>% 
  mutate(song_id = str_replace_all(song, "[^[:alnum:]]", ""))

pavementsongs <- read_csv("data/pavementsongs.csv") %>% 
  mutate(song_id = str_replace_all(song, "[^[:alnum:]]", ""))

pavementsets %>% 
  select(song) %>% 
  distinct() %>% 
  arrange(song) %>% 
  View()

add_song <-
  tribble(
    ~song, ~count, ~frequency,
    'Angel Carver Blues / Mellow Jazz Docent', 0, 0.02
  )

pre_atl_ct <-
  pavementsets %>% 
  filter(city != 'Atlanta') %>% 
  group_by(song) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(frequency = count / 19) %>% 
  bind_rows(add_song) %>% 
  mutate(song_id = str_replace_all(song, "[^[:alnum:]]", ""))

atl_song_ct <-
  pavementsets %>% 
  filter(city == 'Atlanta') %>% 
  group_by(song) %>% 
  summarize(atl = n()) %>% 
  ungroup() %>% 
  mutate(song_id = str_replace_all(song, "[^[:alnum:]]", ""))

song_ct <-
  pre_atl_ct %>% 
  left_join(
    atl_song_ct %>% select(song_id, atl), by = 'song_id') %>% 
  replace(is.na(.), 0)
# %>% 
#   left_join(
#     pavementsongs %>% 
#       select(song, album), 
#     by = 'song')


ggplot(song_ct, aes(x = reorder(song, count), y = frequency, fill = as.factor(atl))) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(
    labels = c('Not Played in ATL', 'Played Once in ATL', 'Played Both ATL Nights'),
    # values = c('#F8AFA8', '#74A089', '#9A8822'),
    values = c('#D3DDDC', '#C7B19C', '#446455')) +
  coord_flip() +
  theme_wsj() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    title = element_text(size = 22, color = 'grey30'),
    text = element_text(color = 'grey40', family = 'Lato'),
    legend.position = c(0.8, 0.2),
    legend.direction = 'vertical'
  ) +
  xlab('') + ylab('') + ggtitle('Pavement Song Frequency 2022 Tour') +
  labs(fill = '')
