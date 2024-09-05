# This code was used to predict the setlist of Cake
# at Koka Booth on August 6, 2024 in Raleigh, NC

# Setup ----

source('util/setup.R')
source('util/setlist_functions.R')

# Load Data ----

# Pull data from setlist.fm API
# Band Name (required)
# Musicbrainz Identifier (if known -- else will prompt)
cake <- 
  getSongInfo(
    'Cake',
    'fa7b9055-3703-473a-8a09-adf2fe031a24')

# Cake Tour 2024 ----
# The Cake Tour kicked off May 10, 2024
# and ended September 11, 2024 (guess?)

cake_tour <-
  cake |> 
  filter(
    EventDate >= as.Date('2024-05-10'),
    EventDate <= as.Date('2024-09-11'))

cake_tour_pre_koka <-
  cake_tour |> 
  filter(EventDate < as.Date('2024-08-06'))

cake_tour_koka <-
  cake_tour |> 
  filter(EventDate == as.Date('2024-08-06')) |> 
  mutate(koka = 1)

agg_pre_koka <- getSongAgg(cake_tour_pre_koka) 

agg_post_koka <- 
  agg_pre_koka |> 
  full_join(
    cake_tour_koka |> 
      select(SongName, koka),
    by = 'SongName') |> 
  mutate(koka = coalesce(koka, 0))


# song_ct <-
#   pre_raleigh_set |> 
#   mutate(SongType = as.factor(if_else(Cover == 'TRUE', 'cov', 'orig'))) |> 
#   mutate(SongType = if_else(SongName == 'Jolene', 'cov', SongType)) |> 
#   filter(!SongName %in% c('Tree Giveaway Intermission', 'Tree Giveaway (gave ponderosa pine to Dani)')) |> 
#   mutate(SongName = if_else(SongName %in% c("Vince DiCola - War (Theme From 'Rocky IV')", 'War (Vince DeCola track)'), 'War', SongName)) |> 
#   mutate(SongType = if_else(SongName == 'War', 'cov', SongType)) 


ggplot(agg_post_koka, aes(x = reorder(SongName, TimesPlayed), y = p, fill = as.factor(koka))) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(
    labels = c('Not Played in Raleigh', 'Played in Raleigh'),
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
  xlab('') + ylab('') + ggtitle('Cake Tour 2024 Song Frequency') +
  labs(fill = '') +
  scale_y_continuous(labels = scales::percent)
