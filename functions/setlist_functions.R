base <- "https://api.setlist.fm/rest"

#----ARTIST INFORMATION FUNCTION-----# ####
getArtistInfo <- function(artist_name){
  #' This function takes an artists name and returns a table containing all of the artists
  #' which contain that name on setlist fm
  #' 
  #' @param artist_name - An artists name wrapped in quotes.
  #' 
  #' @return a dataframe containing all of the artists on the site with that name.
  #' 
  
  # Create artist url
  artist_name_url <- URLencode(paste0(base, "/1.0/search/artists", "?artistName=", artist_name, "&sort=sortName"))
    
  # Retrieve artist info using API key
  artist_df <- GET(artist_name_url, add_headers("x-api-key" = Sys.getenv('SETLISTFM_API_KEY'))) 
  artist_df <- suppressMessages(content(artist_df, "text"))
   
  # Convert to a dataframe from JSON
  artist_df <- as.data.frame(fromJSON(artist_df)$artist) |> 
    mutate(artist_number = row_number()) |> 
    select(artist_number, name, disambiguation, mbid)
    
  # Check to see if more than 1 artist return from name search
  if(nrow(artist_df) == 1){
    correct.artist <- artist_df[1,]
  } else {
    # Print out the table of artists
    print(kable(artist_df, row.names = F))
    cat("\n")
    # Prompt user to select the correct artist
    correct.artist <- readline(prompt = "Select the Correct Artist Number: ")
    # Get the row containing the correct artist
    correct.artist <- artist_df[correct.artist,] |> select(mbid) |> pull()
  }

  invisible(return(correct.artist))
  
}

#----RETURN THE SETLISTS FUNCTION----# ####
getSetlistInfo <- function(artist_name, mbid, number_sets){
  #' This is a function to take an artists name and return the setlists available for that artist
  #' up until a max of 20. If a setlist is returned later than the current date then it will be filtered
  #' out of the final setlist dataframe. The final input from the user will be the amount of setlists
  #' that they wish to return.
  #' 
  #' @param artist_name - The name of the artist you wish to search
  #' @param mbid - The Musicbrainz Identifier (if known)
  #' @param number_sets - How many sets to analyze?
  #' 
  
  # Artist base URL
  artist_url <- "/1.0/artist/"
  
  # Create an empty data frame to store venue info
  venue_info <- data.frame() 
  
  # Assign result of artist selection to an object
  
  # Create the setlist url using the artist code from the artist data function
  setlists <- paste0(base, artist_url, mbid, "/setlists") 
  # Get a list of at max, their last 20 shows
  setlist_list <- GET(setlists, add_headers("x-api-key" = Sys.getenv('SETLISTFM_API_KEY'))) 
  # Return the number of setlists within this list
  number_of_setlists <- as.data.frame(lengths(content(setlist_list)))['setlist',1]
  # Loop across the setlists and return various bits of information about the setlist and venue
  # then bind these together in a data frame appending each time. Pasting '' at the end of each
  # value so that it stores a value for it and not NULL if no data available.
  for(i in 1:number_of_setlists){
    new_data <- cbind(
      'EventID' = i,
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
  # Convert the date column to date format
  venue_info$EventDate <- strptime(venue_info$EventDate, format = "%d-%m-%Y")
  # Subset for sets before today 
  venue_info <- subset(venue_info, as.Date(EventDate) < as.Date(Sys.Date()))
  
  print(kable(venue_info[,-2], row.names = F))
  
  if(is.null(number_sets)){

    number_sets <- readline(prompt = "Select the Number of Sets to Include: ")

    } else {
    
  }

  sets <- venue_info[1:number_sets,1]
  
  venue_info <- venue_info |> head(as.numeric(number_sets))
  
  # print(sets)
  
  info_needed <- list(content(setlist_list), sets, venue_info)
  
  # return(venue_info)
  return(info_needed)
  
}

#----RETURN THE SONGS PLAYED---------# ####
getSongInfo <- function(artist_name, mbid = NULL, number_sets = NULL){
  #' This function takes the artist name as an argument and applies it to the previous function,
  #' Which in turn applies it to the one before that.
  #' Once it has the artist name and the number of sets to choose from it then gathers all of
  #' the songs for each set and turns them into a dataframe ordered by the songs average location in
  #' the set, also including the probability that the song will be played.
  #' 
  #' @param artist_name - The name of the chosen artist to be passed through functions.
  #' @param mbid - The Musicbrainz Identifier (if known)
  #' @param number_sets - How many sets to analyze?
  #' 
  #' @return - a dataframe of song info.
  #' 

  if(!is.null(mbid)){

    info_needed <- getSetlistInfo(artist_name, mbid, number_sets)

    } else {
    
    mbid <- getArtistInfo(artist_name)

    info_needed <- getSetlistInfo(artist_name, mbid, number_sets)
  }

  # Create a blank dataframe which can be used to store the results
  dataset <- data.frame()
  
  # Now, info_needed[[2]] is the SetID of the sets selected, we want to first create a loop across
  # all selected sets.
  for(set in as.numeric(info_needed[[2]])){
    # We need to get the number of sets within each set e.g. main stage, side stage, encore count as 3.
    b <- sum(lengths(info_needed[[1]]$setlist[[set]]$sets))
    # We only want to continue the process if each setlist has more than zero sets.
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
  
  return(output)
}







