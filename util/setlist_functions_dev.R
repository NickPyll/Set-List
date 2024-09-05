base <- "https://api.setlist.fm/rest"
artist_url <- "/1.0/artist/"
mbid <- '83b9cbe7-9857-49e2-ab8e-b57b01038103'
venue_info <- data.frame() 
content_list <- list()
k <- 0

total_events <- 
  round(content(GET(paste0(base, artist_url, mbid, "/setlists"),
    add_headers("x-api-key" = Sys.getenv('SETLISTFM_API_KEY'))))$total)

events_per_page <- 
  round(content(GET(paste0(base, artist_url, mbid, "/setlists"),
    add_headers("x-api-key" = Sys.getenv('SETLISTFM_API_KEY'))))$itemsPerPage)

pages <- round(total_events / events_per_page)

for(page in 1:pages){
  
  print(page)
  setlists <- paste0(base, artist_url, mbid, "/setlists?p=", page) 
  setlist_list <- GET(setlists, add_headers("x-api-key" = Sys.getenv('SETLISTFM_API_KEY'))) 
  number_of_setlists <- as.data.frame(lengths(content(setlist_list)))['setlist',1]
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

  content_list[[page]] <- content(setlist_list)

}  
