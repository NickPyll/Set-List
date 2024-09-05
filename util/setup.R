library(tidyverse)
library(ggthemes)
library(rvest)
library(knitr)
library(httr)
library(jsonlite)
library(scales)
library(dplyr)
library(magrittr)

theme_setlist <-
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
  )

theme_set(theme_setlist)