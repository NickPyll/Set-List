base <- "https://api.setlist.fm/rest"

# These functions heavily inspired by and borrowed from Ben Cross.

#----ARTIST INFORMATION FUNCTION-----# ####
getArtistInfo <- function(artist_name) {
  #' This function takes an artists name and returns a table containing all of the artists
  #' which contain that name on setlist fm
  #'
  #' @param artist_name - An artists name wrapped in quotes.
  #'
  #' @return The artist's Musicbrainz identifier (MBID).
  #'

  # Create artist url
  artist_name_url <- URLencode(paste0(base, "/1.0/search/artists", "?artistName=", artist_name, "&sort=sortName"))

  # Retrieve artist info using API key
  artist_df <- GET(artist_name_url, add_headers("x-api-key" = Sys.getenv("SETLISTFM_API_KEY")))
  artist_df <- suppressMessages(content(artist_df, "text"))

  # Convert to a dataframe from JSON
  artist_df <- as.data.frame(fromJSON(artist_df)$artist) |>
    mutate(artist_number = row_number()) |>
    select(artist_number, name, disambiguation, mbid)

  # Check to see if more than 1 artist return from name search
  if (nrow(artist_df) == 1) {
    correct.artist <- artist_df[1, ]
  } else {
    # Print out the table of artists
    print(kable(artist_df, row.names = F))
    cat("\n")
    # Prompt user to select the correct artist
    correct.artist <- readline(prompt = "Select the Correct Artist Number: ")
    # Get the row containing the correct artist
    correct.artist <- artist_df[correct.artist, ] |>
      select(mbid) |>
      pull()
  }

  invisible(return(correct.artist))
}

#----RETURN THE SETLISTS FUNCTION----# ####
getSetlistInfo <- function(artist_name, mbid) {
  #' This is a function to take an artists name and return the setlists available for that artist
  #' up until a max of 20. If a setlist is returned later than the current date then it will be filtered
  #' out of the final setlist dataframe. The final input from the user will be the amount of setlists
  #' that they wish to return.
  #'
  #' @param artist_name - The name of the artist you wish to search
  #' @param mbid - The Musicbrainz Identifier (if known)
  #'
  #' @return - A list of information necessary for getSongInfo function
  #'


  # Artist base URL
  artist_url <- "/1.0/artist/"

  # Create an empty data frame to store venue info
  venue_info <- data.frame()

  # Create an empty list to store sets
  content_list <- list()

  # counter to help align event_ids -- otherwise starts over every 20
  k <- 0

  # Pull total event count for the artists
  total_events <-
    round(content(GET(
      paste0(base, artist_url, mbid, "/setlists"),
      add_headers("x-api-key" = Sys.getenv("SETLISTFM_API_KEY"))
    ))$total)

  # Pull page count (should be 20)
  events_per_page <-
    round(content(GET(
      paste0(base, artist_url, mbid, "/setlists"),
      add_headers("x-api-key" = Sys.getenv("SETLISTFM_API_KEY"))
    ))$itemsPerPage)

  # Determine how many pages to loop through
  total_pages <- round(total_events / events_per_page)

  # Loop through all pages (can hardcode for testing)
  pages <- total_pages
  # pages <- 3 # limit pages for troubleshooting

  for (page in 1:pages) {
    print(paste0("Scraping page ", page, " of ", pages)) # printing progress

    # Create the page specific setlist url
    setlists <- paste0(base, artist_url, mbid, "/setlists?p=", page)

    # Get a list of all the sets on the page
    setlist_list <- GET(setlists, add_headers("x-api-key" = Sys.getenv("SETLISTFM_API_KEY")))

    # Return the number of setlists within this list
    number_of_setlists <- as.data.frame(lengths(content(setlist_list)))["setlist", 1]

    # Loop across the setlists and return various bits of information about the setlist and venue
    # then bind these together in a data frame appending each time. Pasting '' at the end of each
    # value so that it stores a value for it and not NULL if no data available.
    for (i in 1:number_of_setlists) {
      new_data <- cbind(
        "event_id" = i + k,
        "event_date" = paste0(content(setlist_list)$setlist[[i]]$eventDate, ""),
        "country" = paste0(content(setlist_list)$setlist[[i]]$venue$city$country$name, ""),
        "state" = paste0(content(setlist_list)$setlist[[i]]$venue$city$state, ""),
        "city" = paste0(content(setlist_list)$setlist[[i]]$venue$city$name, ""),
        "venue_name" = paste0(content(setlist_list)$setlist[[i]]$venue$name, ""),
        "latitude" = paste0(content(setlist_list)$setlist[[i]]$venue$city$coords$lat, ""),
        "longitude" = paste0(content(setlist_list)$setlist[[i]]$venue$city$coords$long, "")
      )

      # add loop data to the main dataframe
      venue_info <- rbind(venue_info, new_data)
    }

    # iterate counter for event_id
    k <- i + k

    # add the page content into the list
    content_list[[page]] <- content(setlist_list)

    # system sleep for 2 seconds each loop to avoid throttle limits
    Sys.sleep(2)
  }

  # Convert the date column to date format
  venue_info$event_date <- strptime(venue_info$event_date, format = "%d-%m-%Y")

  # Subset for sets before today
  venue_info <- subset(venue_info, as.Date(event_date) < as.Date(Sys.Date()))

  # last page will have fewer than 20 events - modulo finds remainder
  remainder_events <- total_events %% events_per_page

  # organizes all output for use in next function
  info_needed <- list(content_list, venue_info, pages, total_pages, remainder_events)
  return(info_needed)
}

#----RETURN THE SONGS PLAYED---------# ####
getSongInfo <- function(artist_name, mbid = NULL) {
  #' This function takes the artist name as an argument and applies it to the previous function,
  #' Which in turn applies it to the one before that.
  #' Once it has the artist name and the number of sets to choose from it then gathers all of
  #' the songs for each set and turns them into a dataframe ordered by the songs average location in
  #' the set, also including the probability that the song will be played.
  #'
  #' @param artist_name - The name of the chosen artist to be passed through functions.
  #' @param mbid - The Musicbrainz Identifier (if known)
  #'
  #' @return - a dataframe of song info.
  #'

  # if user does not know mbid, then getArtistInfo is called
  # else move on to getSetlistInfo
  if (!is.null(mbid)) {
    info_needed <- getSetlistInfo(artist_name, mbid)
  } else {
    mbid <- getArtistInfo(artist_name)
    info_needed <- getSetlistInfo(artist_name, mbid)
  }

  # Create a blank dataframe which can be used to store the results
  setlist_info <- data.frame()

  # counter to help align event_ids -- otherwise starts over every 20
  # DEVNOTE: this logic fails when setlist.fm lists an upcoming show with no songs yet populated
  # need to design a better way to handle that scneario but for now just subtract 2 instead of 1
  k <- min(as.numeric(info_needed[[2]]$event_id)) - 1
  # k <- min(as.numeric(info_needed[[2]]$event_id)) - 2 # see DEVNOTE above

  # pull pages parameter from last function
  pages <- info_needed[[3]]

  for (page in 1:pages) {
    # logic to tell the loop how many events the current page should have
    if (page < pages) {
      events <- info_needed[[1]][[page]]$itemsPerPage
    } else {
      if (pages < info_needed[[4]]) {
        events <- info_needed[[1]][[page]]$itemsPerPage
      } else {
        events <- info_needed[[5]]
      }
    }

    for (event in 1:events) {
      print(paste0("Preparing page ", page, " of ", pages, ": event ", event, " of ", events)) # printing progress

      # We need to get the number of sets within each set e.g. main stage, side stage, encore count as 3.
      number_sets <- sum(lengths(info_needed[[1]][[page]]$setlist[[event]]$sets))

      # We only want to continue the process if each setlist has more than one set.
      if (number_sets > 0) {
        # The next loop initiated loops across all of the mini sets within the big sets and returns the number
        # of songs in each of these
        for (set in 1:number_sets) {
          # The number of songs in each mini-set
          song_count <- length(lengths(info_needed[[1]][[page]]$setlist[[event]]$sets$set[[set]]$song))

          # The final nested loop looks at all the songs within each min-set and larger-set and returns their name
          # i have also included code for if there is a cover song. So we return the name of the cover artist also.
          for (song in 1:song_count) {
            # Begin to create the new dataframe by combining the set number and song title.
            newdata <- cbind(
              "event_id" = event + k,
              "song_name" = info_needed[[1]][[page]]$setlist[[event]]$sets$set[[set]]$song[[song]]$name
            )

            # Next i want to see if the current song is a cover song or not. First try and allocate to 't'
            t <- try(info_needed[[1]][[page]]$setlist[[event]]$sets$set[[set]]$song[[song]]$cover$name)

            # If class of 't' is NULL then it is not a cover song, else it is a cover song and grab the artist name
            if ("NULL" %in% class(t)) {
              newdata <- cbind(newdata,
                "cover" = FALSE,
                "original_artist" = "N/A"
              )
            } else {
              newdata <- cbind(newdata,
                "cover" = TRUE,
                "original_artist" = info_needed[[1]][[page]]$setlist[[event]]$sets$set[[set]]$song[[song]]$cover$name
              )
            }

            # Bind the new data and the old data together in a dataset.
            setlist_info <- rbind(setlist_info, newdata)
          }
        }
      }
    }

    # iterate counter for event_id
    k <- event + k
  }

  # bring venue_Info dataframe in from previous function
  venue_info <- as.data.frame(info_needed[[2]])

  # join setlist_info to venue_info
  show_detail <- setlist_info |>
    inner_join(venue_info, by = "event_id") |>
    group_by(event_id) |>
    # Add a row number value for the song position
    mutate(song_position = row_number()) |>
    ungroup()

  # organize output into r session
  output <- show_detail

  return(output)
}

#----AGGREGATE OUTPUT AT SONG LEVEL---------# ####
getSongAgg <- function(input_data) {
  #' This function takes the output of getSongInfo and produces a song level aggregation.
  #'
  #' @param input_data - The output of getSongInfo.
  #'
  #' @return - a dataframe of song stats
  #'

  aggregate <- input_data |>
    # First we remove any whitespace
    mutate(across(.cols = (2:4), ~ trimws(.x))) |>
    # Then we want to group by the following to summarise
    group_by(song_name, cover, original_artist) |>
    # Here we want the songs average position, the number of times played and then the probability
    # that the song will be played.
    summarise(
      avg_song_position = mean(song_position),
      times_played = n(),
      num_events = length(unique(event_id)),
      .groups = "drop"
    ) |>
    # Then overwrite the ProbPlay column with actual probability
    mutate(
      p = times_played / max(num_events),
      p_bin =
        case_when(
          p < .1 ~ "Rare",
          p < .5 ~ "Unlikely",
          p < .75 ~ "Likely",
          p <= 1 ~ "Standards"
        )
    ) |>
    # Finally filter out blanks
    filter(nchar(trimws(song_name)) > 0)
}
