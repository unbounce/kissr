authorizationHeader <- function() {
  apiToken <- Sys.getenv("KM_API_TOKEN")
  if (is.null(apiToken) || apiToken == "") stop("Please set your KissMetrics api token with Sys.setenv('KM_API_TOKEN')")

  c("Authorization" = paste("Bearer", apiToken))
}

jsonHeader <- function() {
  c("Content-Type" = "application/json")
}

as.list.Interval <- function(interval) {
  list( start_date = lubridate::int_start(interval),
        end_date = lubridate::int_end(interval))
}

# convert dates to timestamps
as.timestamp <- function(date) {
  strftime(date, format = "%Y-%m-%dT%H:%M:%S%z")
}

# Format an httr::url to a simple string
format.url <- function(url) {
  httr::build_url(x)
}

# generate a cache key from an object
as.cacheKey <- function(object) {
  if (!is.character(object)) object <- sapply(object, format)

  paste0(object, collapse=":")
}

# return whatever is stored by cache
readCache <- function (object, key) {
  key <- as.cacheKey(key)
  object$cache[[key]]
}

# return old value
writeCache <- function (object, key, value) {
  key <- as.cacheKey(key)

  old = object$cache[[key]]
  object$cache[[key]] = value

  invisible(old)
}

# Build a set of urls matching all the offsets from the url template
buildPaginationUrls <- function(urlTemplate, offsets) {
  matches <- "\\{\\{OFFSET\\}\\}"
  stringr::str_replace_all(urlTemplate, matches, offsets)
}

# load data from a page
loadPage <- function (url, object) {
  headers <- c(authorizationHeader(), jsonHeader())
  requestKey = c(url, headers)
  response <- readCache(object, requestKey)
  if (is.null(response)) {
    response <- httr::GET(url, httr::add_headers(headers))
    writeCache(object, requestKey, response)
  }
  results <- jsonlite::fromJSON(httr::content(response, "text"))

  results$data[,-1]
}

# Load all data from a series of pages
loadPages <- function (object, url) {
  headers <- c(authorizationHeader(), jsonHeader())
  requestKey = c(url, headers)

  response <- readCache(object, requestKey)
  if (is.null(response)) {
    response <- httr::GET(url, httr::add_headers(headers))
    writeCache(object, requestKey, response)
  }
  pages <- jsonlite::fromJSON(httr::content(response, "text"))

  totalItems <- pages$pagination$total
  itemsPerPage <- pages$pagination$limit

  if(totalItems > 0) {
    urlTemplate <- stringr::str_c(url,
                                         "?limit=", itemsPerPage, "\u0026offset={{OFFSET}}")


    urls <- buildPaginationUrls(urlTemplate = urlTemplate,
                                       offsets = seq(from=0,
                                                     to=totalItems,
                                                     by=itemsPerPage))

    result <- do.call(rbind, lapply(urls, loadPage, object=object))
  } else if (is.null(object$columnNames)) {
    result <- data.frame(product_id=c(), account_id=c(), report_type=c(), name=c(), created_at=c(), links=c())
  } else {
    result <- setNames(data.frame(matrix(ncol=length(object$columnNames), nrow=0)),
                        object$columnNames)
  }
  return(result)
}

# We only support a specific format returned by KissMetrics, so we'll use
# lubridate::fast_strptime instead of parse_date_time.
# Take a character vector, and if we think it is a time that can be parsed by
# format we'll return a vector of times, if it can't be converted (any of the
# non-NA elements fail to parse) we return the original vector
try_convert_time <- function(char_vector, formats = "%Y-%m-%d %H:%M:%S") {
  result <- char_vector

  timezone <- Sys.getenv("FROM_TIMEZONE")
  if(is.na(timezone)) timezone <- "UTC"

  converted <- tryCatch(
    lubridate::fast_strptime(char_vector, format = formats, tz = timezone),
    error = function(e) char_vector
  )
  if( isTRUE(all.equal(is.na(converted), is.na(char_vector)))) {
    result <-with_tz(converted, "UTC")
  }
  result
}
