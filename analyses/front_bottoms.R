# This code was used to predict the setlist of the Front Bottoms
# at the Ritz on November 7, 2023 in Raleigh, NC

# Setup ----

source('util/setup.R')
source('util/setlist_functions.R')

# Load Data ----

# Pull data from setlist.fm API
# Band Name (required)
# Musicbrainz Identifier (if known -- else will prompt)
fb <- 
  getSongInfo(
    'The Front Bottoms',
    '622b6000-ff0d-4ab3-9e98-c4916f3692c3')

# Finding Your Way Home Tour ----
# The FYWH Tour kicked off September 12, 2023
# and ended December 14, 2023

fywh_tour <-
  fb |> 
  filter(
    EventDate >= as.Date('2023-09-12'),
    EventDate <= as.Date('2023-12-14'))

fywh_tour_pre_ritz <-
  fywh_tour |> 
  filter(EventDate < as.Date('2023-11-07'))

fywh_tour_ritz <-
  fywh_tour |> 
  filter(EventDate == as.Date('2023-11-07')) |> 
  mutate(ritz = 1)

agg_pre_ritz <- getSongAgg(fywh_tour_pre_ritz) 

agg_post_ritz <- 
  agg_pre_ritz |> 
  full_join(
    fywh_tour_ritz |> 
      select(SongName, ritz),
    by = 'SongName') |> 
  mutate(ritz = coalesce(ritz, 0))

ggplot(agg_pre_ritz, aes(x = reorder(SongName, TimesPlayed), y = p)) + 
  geom_bar(stat = "identity", fill = '#A42820') +
  coord_flip() +
  theme_wsj() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey30", linewidth = 0.2),
    panel.grid.minor.x = element_blank(),
    title = element_text(size = 26, color = 'grey30'),
    text = element_text(color = 'grey40', family = 'Lato'),
    legend.position = c(0.8, 0.2),
    legend.direction = 'vertical',
    plot.title.position = "plot",
    plot.caption = element_text(color = 'grey40', family = 'Lato', size = 14)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = 'The Front Bottoms Song Predictions^',
    caption = '^Based on last 20 performances'
  ) +
  scale_y_continuous(labels = scales::percent)

ggplot(agg_post_ritz, aes(x = reorder(SongName, TimesPlayed), y = p, fill = as.factor(ritz))) + 
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
  xlab('') + ylab('') + ggtitle('Front Bottoms Song Frequency FYWH Tour 2023') +
  labs(fill = '')
