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

as.timestamp <- function(date) {
  strftime(date, format = "%Y-%m-%dT%H:%M:%S%z")
}
