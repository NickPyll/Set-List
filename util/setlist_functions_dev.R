base <- "https://api.setlist.fm/rest"
artist_url <- "/1.0/artist/"
mbid <- "83b9cbe7-9857-49e2-ab8e-b57b01038103"
venue_info <- data.frame()
content_list <- list()
k <- 0

# total_events <-
#   round(content(GET(paste0(base, artist_url, mbid, "/setlists"),
#     add_headers("x-api-key" = Sys.getenv('SETLISTFM_API_KEY'))))$total)

# events_per_page <-
#   round(content(GET(paste0(base, artist_url, mbid, "/setlists"),
#     add_headers("x-api-key" = Sys.getenv('SETLISTFM_API_KEY'))))$itemsPerPage)

# pages <- round(total_events / events_per_page)
pages <- 5

for (page in 1:pages) {
  print(paste0("Scraping page ", page, " of ", pages))
  setlists <- paste0(base, artist_url, mbid, "/setlists?p=", page)
  setlist_list <- GET(setlists, add_headers("x-api-key" = Sys.getenv("SETLISTFM_API_KEY")))
  number_of_setlists <- as.data.frame(lengths(content(setlist_list)))["setlist", 1]
  for (i in 1:number_of_setlists) {
    new_data <- cbind(
      "EventID" = i + k,
      "EventDate" = paste0(content(setlist_list)$setlist[[i]]$eventDate, ""),
      "Country" = paste0(content(setlist_list)$setlist[[i]]$venue$city$country$name, ""),
      "State" = paste0(content(setlist_list)$setlist[[i]]$venue$city$state, ""),
      "City" = paste0(content(setlist_list)$setlist[[i]]$venue$city$name, ""),
      "VenueName" = paste0(content(setlist_list)$setlist[[i]]$venue$name, ""),
      "Latitude" = paste0(content(setlist_list)$setlist[[i]]$venue$city$coords$lat, ""),
      "Longitude" = paste0(content(setlist_list)$setlist[[i]]$venue$city$coords$long, "")
    )
    venue_info <- rbind(venue_info, new_data)
  }
  k <- i + k

  content_list[[page]] <- content(setlist_list)

  Sys.sleep(2)
}

venue_info$EventDate <- strptime(venue_info$EventDate, format = "%d-%m-%Y")
venue_info <- subset(venue_info, as.Date(EventDate) < as.Date(Sys.Date()))

info_needed <- list(content_list, venue_info, pages)

dataset <- data.frame()
# DEVNOTE: this logic fails when setlist.fm lists an upcoming show with no songs yet populated
# need to design a better way to handle that scneario but for now just subtract 2 instead of 1
k <- min(as.numeric(info_needed[[2]]$event_id)) - 1
# k <- min(as.numeric(info_needed[[2]]$event_id)) - 2 # see DEVNOTE above
pages <- info_needed[[3]]

for (page in 1:pages) {
  events <- info_needed[[1]][[page]]$itemsPerPage

  for (event in 1:events) {
    print(paste0("Preparing page ", page, " of ", pages, ": event ", event, " of ", events))

    number_sets <- sum(lengths(info_needed[[1]][[page]]$setlist[[event]]$sets))
    if (number_sets > 0) {
      for (set in 1:number_sets) {
        song_count <- length(lengths(info_needed[[1]][[page]]$setlist[[event]]$sets$set[[set]]$song))
        for (song in 1:song_count) {
          newdata <- cbind(
            "EventID" = event + k,
            "SongName" = info_needed[[1]][[page]]$setlist[[event]]$sets$set[[set]]$song[[song]]$name
          )
          t <- try(info_needed[[1]][[page]]$setlist[[event]]$sets$set[[set]]$song[[song]]$cover$name)
          if ("NULL" %in% class(t)) {
            newdata <- cbind(newdata,
              "Cover" = FALSE,
              "OrigArtist" = "N/A"
            )
          } else {
            newdata <- cbind(newdata,
              "Cover" = TRUE,
              "OrigArtist" = info_needed[[1]][[page]]$setlist[[event]]$sets$set[[set]]$song[[song]]$cover$name
            )
          }
          dataset <- rbind(dataset, newdata)
        }
      }
    }
  }

  k <- event + k
}

venue_info <- as.data.frame(info_needed[[2]])

show_detail <- dataset |>
  inner_join(venue_info, by = "EventID")

output <- show_detail
