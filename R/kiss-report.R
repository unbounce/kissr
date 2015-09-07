#' Create a new KissReport
#'
#' \code{KissReport} creates a new S3 object of with class "KissReport"
#'
#' This is a constructor function.
#'
#' @param url An HREF to the report in KM as described by
#'   \link{http://support.kissmetrics.com/api-update.html}
#' @param interval The time range you want the report run for as a \code{lubridate::interval}
#'   (optional - can specify start and end instead)
#' @param start The start date and time for the report (optional - can specify an interval instead)
#' @param end The end date and time for the report (optional - can specify an interval instead)
#' @return If url is a valid string or url (from \code{httr::url}) and the start and end dates are specified
#'   This returns a new KissReport object that can be passed to the read S3 generic

#' @export
KissReport <- function(url, start, end, interval) {
  if (!is.character(url) && !httr::is.url(url)) stop("url must be a string or url class")
  if(is.null(interval)) {
    if (!lubridate::is.Date(start)) stop("If not creating with an interval, start must be a date")
    if (!lubridate::is.Date(end)) stop("If not creating with an interval, end must be a date")
    interval <- interval(start, end)
  } else {
    if (!lubridate::is.interval(interval)) stop("interval must be a valid interval")
  }

  url <- httr::parse_url(url)
  structure(list(
    url = url,
    interval = interval),
    class = "KissReport")
}

#' Read data from a KissReport
#'
#' This function reads all the data from a KissMetrics people report
#' returning everything in a data frame. Please note this is a synchronous function
#' that can take quite some time to complete.
#'
#' @return  A \code{data.frame} containing all the data in the report
#' @export
read.KissReport <- function(report) {
  # Make request
  headers <- c(authorizationHeader(), jsonHeader())
  response <- httr::POST(report$url,
                         body = lapply(as.list(report$interval), as.timestamp),
                         encode = "json",
                         httr::add_headers(.headers = headers))
  httr::stop_for_status(response)

  # Get report status link from the response
  links  <- jsonlite::fromJSON(httr::content(response, "text"))$links
  statusLink <- links[links$name == "Status", "href"]

  # Check status until it returns results with data["completed" == TRUE]
  tries <- 0
  maxTries <- 10
  reportComplete <- function (response) {
    response$data$completed
  }

  resultsLink <- ""
  repeat {
    # Wait 15 seconds for report to complete
    Sys.sleep(15)

    print("Checking status of report")
    currentStatus <- jsonlite::fromJSON(httr::content(
        httr::GET(statusLink, httr::add_headers(headers)),
        "text"))
    tries <- tries + 1
    if (reportComplete(currentStatus)) {
      resultsLink <- with(currentStatus, links[links$name == "Results", "href"])
      break
    }
    else if (tries >= maxTries) {
      stop("Report hasn't completed - timing out.")
    }
  }

  # resultsLink should now
  print("Report ready, pulling results")





}
