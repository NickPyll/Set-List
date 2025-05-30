# Set List Analysis
This repository contains functions which can scrape an artist's setlists from the web and aggregate, visualize, model the output

## Getting Set Up
You will need to register with [setlist.fm](https://api.setlist.fm/docs/1.0/index.html) and apply for an API key.

Once you are set up with an API key, click the dropdown by your username (top right) and click API. Copy your API Key and save it in your `.Renviron` file as `SETLISTFM_API_KEY`.

## Using the Functions

Look for the `guided_example.R` for examples of how to use the following fuctions

`getArtistInfo` returns the band's MBID (Musicbrainz Identifier) when given a band name. Will prompt user to make a choice if multiple entires returned in search.

`getSetlistInfo` loops through the pages and collects the content. 

`getSongInfo` will call `getArtistInfo` unless mbid provided. Function then calls `getSetlistInfo` and processes the output of that function.

`getSongAgg` Calculates percentages and aggregates by song.

## Note of Acknowledgment

The functions portion of this work borrows VERY heavily from Ben Cross -- while I had manual workarounds for much of this stuff, his elegant functions provided a foundation for me to build on. While I've modified and added to those functions substantially, this work would not exist in its current form if not for the work he shared.
