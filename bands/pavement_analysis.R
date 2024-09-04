library(tidyverse)
library(ggthemes)
library(rvest)
library(knitr)
library(httr)
library(jsonlite)
library(scales)
library(dplyr)

source('setlist_functions.R')

getArtistInfo('Pavement')

df <- getSetlistInfo('Pavement')

df <- getSongInfo('Pavement')
