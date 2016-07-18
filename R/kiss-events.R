# KissEvents

# Used to get list of events table from Kissmetrics
# Example: listEvents <- read(KissEvents())
# Also used to separate functions to easily access attributes
# See: attribute-explorers.R

KissEvents <- function() {
  url <- httr::parse_url("https://api.kissmetrics.com/core/events")

  structure(list(
    url = url),
    class = "KissEvents")
}


#' Read a list of events from a KissEvents object
#'
#' This function reads all the data from a KissMetrics events table
#' returning the index,product_id,name, and display_name columns in a data frame.
#'
#'
read.KissEvents <- function(events) {
  # Make request
  response <- readUrl(events$url, events)

  # Get list of reports
  links  <- jsonlite::fromJSON(httr::content(response, "text"))$links

  eventsLink <- links[links$name == "First", "href"]
  print("Getting all events")
  results <- as.data.frame(loadPages(events, eventsLink), stringsAsFactors = FALSE)

  # We only need a few columns from the generated table
  results <- results[,c("index","product_id","name", "display_name")]
  results
}
