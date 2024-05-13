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

pre_ritz_set <- getSongInfo('The Front Bottoms')

pre_ritz_set %<>% 
  mutate(p = as.numeric(gsub('%', '', ProbPlay)) / 100)

fb_ritz_set <- getSongInfo('The Front Bottoms')

fb_ritz_set %<>% 
  mutate(p = as.numeric(gsub('%', '', ProbPlay)) / 100,
         ritz = 1)

song_ct <-
  pre_ritz_set %>% 
  left_join(
    fb_ritz_set %>% select(SongName, ritz), by = 'SongName') %>% 
  replace(is.na(.), 0)


# ggplot(fb_set, aes(x = reorder(SongName, TimesPlayed), y = p)) + 
#   geom_bar(stat = "identity", fill = '#A42820') +
#   coord_flip() +
#   theme_wsj() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.grid.major.x = element_line(colour = "grey30", size = 0.2),
#     panel.grid.minor.x = element_blank(),
#     title = element_text(size = 26, color = 'grey30'),
#     text = element_text(color = 'grey40', family = 'Lato'),
#     legend.position = c(0.8, 0.2),
#     legend.direction = 'vertical',
#     plot.title.position = "plot",
#     plot.caption = element_text(color = 'grey40', family = 'Lato', size = 14)
#   ) +
#   labs(
#     x = NULL,
#     y = NULL,
#     title = 'The Front Bottoms Song Predictions^',
#     caption = '^Based on last 20 performances'
#   ) +
#   scale_y_continuous(labels = scales::percent)

ggplot(song_ct, aes(x = reorder(SongName, TimesPlayed), y = p, fill = as.factor(ritz))) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(
    labels = c('Not Played in Raleigh', 'Played in Raleigh'),
    # values = c('#F8AFA8', '#74A089', '#9A8822'),
    values = c('#D3DDDC', '#C7B19C')) +
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
  xlab('') + ylab('') + ggtitle('Front Bottoms Song Frequency 2022 Tour') +
  labs(fill = '')
