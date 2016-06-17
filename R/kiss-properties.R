# KissProperties

# Used to get list of properties table from Kissmetrics
# Example: listProperties <- read(KissProperties())
# Also used to separate functions to easily access attributes
# See: attribute-explorers.R

KissProperties <- function() {
  url <- httr::parse_url("https://api.kissmetrics.com/core/properties")

  structure(list(
    url = url),
    class = "KissProperties")
}


#' Read a list of properties from a KissProperties object
#'
#' This function reads all the data from a KissMetrics properties table
#' returning the index,product_id,name, and display_name columns in a data frame.
#'

read.KissProperties <- function(properties) {
  # Make request
  response <- readUrl(properties$url, properties)

  # Get list of reports
  links  <- jsonlite::fromJSON(httr::content(response, "text"))$links

  propertiesLink <- links[links$name == "First", "href"]
  print("Getting all properties")
  results <- as.data.frame(loadPages(properties, propertiesLink), stringsAsFactors = FALSE)

  # We only need a few columns from the generated table
  results <- results[,c("index","product_id","name", "display_name")]
  results
}
