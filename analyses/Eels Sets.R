# This code was used to predict the setlist of Eels
# at the Cats Cradle on June 29, 2023 in Raleigh, NC

# Setup ----

source('util/setup.R')
source('util/setlist_functions.R')

# Load Data ----

# Pull data from setlist.fm API
# Band Name (required)
# Musicbrainz Identifier (if known -- else will prompt)
eels <- 
  getSongInfo(
    'Eels',
    '14387b0f-765c-4852-852f-135335790466')

# Lockdown Hurricane Tour ----
# The LH Tour kicked off March 26, 2023
# and ended June 30, 2023

lh_tour <-
  eels |> 
  filter(
    EventDate >= as.Date('2023-03-26'),
    EventDate <= as.Date('2023-06-30'))

lh_tour_pre_cradle <-
  lh_tour |> 
  filter(EventDate < as.Date('2023-06-29'))

lh_tour_cradle <-
  lh_tour |> 
  filter(EventDate == as.Date('2023-06-29')) |> 
  mutate(cradle = 1)

agg_pre_cradle <- getSongAgg(lh_tour_pre_cradle) 

agg_post_cradle <- 
  agg_pre_cradle |> 
  full_join(
    lh_tour_cradle |> 
      select(SongName, cradle),
    by = 'SongName') |> 
  mutate(cradle = coalesce(cradle, 0))


ggplot(agg_pre_cradle, aes(x = reorder(SongName, TimesPlayed), y = p)) + 
  geom_bar(stat = "identity", fill = '#A42820') +
  coord_flip() +
  theme_wsj() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey30", size = 0.2),
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
    title = 'The Eels Song Predictions^',
    caption = '^Based on previous show on this tour'
  ) +
  scale_y_continuous(labels = scales::percent)

ggplot(agg_post_cradle, aes(x = reorder(SongName, TimesPlayed), y = p, fill = as.factor(cradle))) + 
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
  xlab('') + ylab('') + ggtitle('Eels Song Frequency 2022 Tour') +
  labs(fill = '')
