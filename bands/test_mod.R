base <- "https://api.setlist.fm/rest"
artist_url <- "/1.0/artist/"
mbid <- '83b9cbe7-9857-49e2-ab8e-b57b01038103'
k <- 0

venue_info <- data.frame() 


for(j in 1:5){
  
  # Create the setlist url using the artist code from the artist data function
  setlists <- paste0(base, artist_url, mbid, "/setlists?p=", j) 
  # Get a list of at max, their last 20 shows
  setlist_list <- GET(setlists, add_headers("x-api-key" = Sys.getenv('SETLISTFM_API_KEY'))) 
  # Return the number of setlists within this list
  number_of_setlists <- as.data.frame(lengths(content(setlist_list)))['setlist',1]
  # Loop across the setlists and return various bits of information about the setlist and venue
  # then bind these together in a data frame appending each time. Pasting '' at the end of each
  # value so that it stores a value for it and not NULL if no data available.
  print(number_of_setlists)
  for(i in 1:number_of_setlists){
    new_data <- cbind(
      'EventID' = i + k,
      'EventDate' = paste0(content(setlist_list)$setlist[[i]]$eventDate, ""),
      'Country' = paste0(content(setlist_list)$setlist[[i]]$venue$city$country$name, ""),
      'State' = paste0(content(setlist_list)$setlist[[i]]$venue$city$state, ""),
      'City' = paste0(content(setlist_list)$setlist[[i]]$venue$city$name, ""),
      'VenueName' = paste0(content(setlist_list)$setlist[[i]]$venue$name, ""),
      'Latitude' = paste0(content(setlist_list)$setlist[[i]]$venue$city$coords$lat, ""),
      'Longitude' = paste0(content(setlist_list)$setlist[[i]]$venue$city$coords$long, "")
    )
    venue_info <- rbind(venue_info, new_data)
  }
  k <- i + k
}  
# Convert the date column to date format
venue_info$EventDate <- strptime(venue_info$EventDate, format = "%d-%m-%Y")
# Subset for sets before today 
venue_info <- subset(venue_info, as.Date(EventDate) < as.Date(Sys.Date()))

print(kable(venue_info[,-2], row.names = F))

number_sets <- 30

sets <- venue_info[1:number_sets,1]

venue_info <- venue_info |> head(as.numeric(number_sets))

# print(sets)

info_needed <- list(content(setlist_list), sets, venue_info)

dataset <- data.frame()
  
# Now, info_needed[[2]] is the SetID of the sets selected, we want to first create a loop across
# all selected sets.
for(set in as.numeric(info_needed[[2]])){

  set <- 2
  # We need to get the number of sets within each set e.g. main stage, side stage, encore count as 3.
  b <- sum(lengths(info_needed[[1]]$setlist[[set]]$sets))
  # We only want to continue the process if each setlist has more than one set.
  if(b > 0){
    
    # The next loop initiated loops across all of the mini sets within the big sets and returns the number
    # of songs in each of these
    for(i in 1:b){
      # The number of songs in each mini-set
      c <- length(lengths(info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song))
      
      # The final nested loop looks at all the songs within each min-set and larger-set and returns their name
      # i have also included code for if there is a cover song. So we return the name of the cover artist also.
      for(j in 1:c){
        # Begin to create the new dataframe by combining the set number and song title.
        newdata <- cbind('EventID' = set,
                         'SongName' = info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song[[j]]$name)
        
        # Next i want to see if the current son gi sa cover song or not. First try and allocate to 't'
        t <- try(info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song[[j]]$cover$name)
        
        # If class of 't' is NULL then it is not a cover song, else it is a cover song and grab the artist name
        if("NULL" %in% class(t)){
          newdata <- cbind(newdata,
                           'Cover' = FALSE,
                           'OrigArtist' = 'N/A')
        } else {
          newdata <- cbind(newdata,
                           'Cover' = TRUE,
                           'OrigArtist' = info_needed[[1]]$setlist[[set]]$sets$set[[i]]$song[[j]]$cover$name)
        }
        
        # Bind the new data and the old data together in a dataset.
        dataset <- rbind(dataset, newdata)
      }
    }
  }
}

venue_info <- as.data.frame(info_needed[[3]])

# The next part of the function is to convert the dataset into a better format.
show_detail <- dataset |>
  inner_join(venue_info, by = 'EventID')

aggregate <- dataset |> 
  # First we remove any whitespace
  mutate(across(.cols = (2:4), ~trimws(.x))) |> 
  # Group by the set number
  group_by(EventID) |> 
  # Add a row number value for the song position
  mutate(song = row_number()) |> 
  ungroup() |> 
  # Then we want to group by the following to summarise
  group_by(SongName, Cover, OrigArtist) |> 
  # Here we want the songs average position, the number of times played and then the probability
  # that the song will be played.
  summarise(AvgPosition = round(mean(song), 2),
            TimesPlayed = n(),
            ProbPlay = length(unique(EventID)),
            .groups = 'drop') |> 
  # Then overwrite the ProbPlay column with actual probability
  mutate(ProbPlay = percent(TimesPlayed / max(ProbPlay), accuracy = 0.01)) %>%
  arrange(AvgPosition) |> 
  # Finally filter out blanks
  filter(nchar(trimws(SongName)) > 0)

output <- list(show_detail, aggregate)

output[[1]] |> View()
