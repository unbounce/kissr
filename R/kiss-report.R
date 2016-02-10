#' Create a new KissReport
#'
#' \code{KissReport} creates a new S3 object of with class "KissReport"
#'
#' This is a constructor function.
#'
#' @param url An HREF to the report in KM as described by
#'   \link{http://support.kissmetrics.com/api-update.html}
#' @param interval The time range you want the report run for as a
#'   \code{lubridate::interval}
#'   (optional - can specify start and end instead)
#' @param start The start date and time for the report (optional - can specify
#'   an interval instead)
#' @param end The end date and time for the report (optional - can specify an
#'   interval instead)
#' @param columnNames The names of columns for any produced reports
#' @return If url is a valid string or url (from \code{httr::url}) and the start
#'   and end dates are specified this returns a new KissReport object that can
#'   be passed to the read S3 generic
#' @examples
#'    reportUrl <- "https://query.kissmetrics.com/v2/products/6581c29e-ab13-1030-97f2-22000a91b1a1/reports/1c564450-3586-0133-85e2-22000a9a8afc/run"
#'    report <- KissReport(reportUrl,
#'                        interval = lubridate::interval(as.Date("2015-06-01"), as.Date("2015-06-02")),
#'                        columnNames = c("KM_Email", "KM_FirstUserId", "FirstVisitDate",
#'                                        "FirstSource", "FirstMedium",
#'                                        "FirstCampaignName", "FirstCampaignContent", "FirstCampaignTerms",
#'                                        "FirstReferrer"))
#'    reportResults <- read(report)
#' @export
KissReport <- function(url, start, end, interval, columnNames) {
  if (!is.character(url) && !httr::is.url(url)) stop("url must be a string or url class")
  if (is.null(interval)) {
    if (!lubridate::is.Date(start)) stop("If not creating with an interval, start must be a date")
    if (!lubridate::is.Date(end)) stop("If not creating with an interval, end must be a date")
    interval <- interval(start, end)
  } else {
    if (!lubridate::is.interval(interval)) stop("interval must be a valid interval")
  }

  cache <- new.env(parent = emptyenv())

  url <- httr::parse_url(url)
  structure(list(
    url = url,
    interval = interval,
    cache = cache,
    columnNames = columnNames),
    class = "KissReport")
}


#' Read data from a KissReport
#'
#' This function reads all the data from a KissMetrics people report
#' returning everything in a data frame. Please note this is a synchronous
#' function that can take quite some time to complete.
#'
#' @return  A \code{data.frame} containing all the data in the report
#' @export
read.KissReport <- function(report) {
  # Make request
  headers <- c(authorizationHeader(), jsonHeader())
  body <- lapply(as.list(report$interval), as.timestamp)
  encoding <- "json"

  requestKey <- c(report$url, headers, body, encoding)
  response <- readCache(report, requestKey)
  if (is.null(response)) {
    response <- httr::POST(report$url,
                         body = body,
                         encode = encoding,
                         httr::add_headers(.headers = headers))

    httr::stop_for_status(response)
    writeCache(report, requestKey, response)
  }

  # Get report status link from the response
  links  <- jsonlite::fromJSON(httr::content(response, "text"))$links
  statusLink <- links[links$name == "Status", "href"]

  # Check status until it returns results with data["completed" == TRUE]
  tries <- 0
  maxTries <- 10
  reportComplete <- function (response) {
    response$data$completed
  }

  resultsLink <- NULL
  repeat {
    print("Checking status of report")
    requestKey <- c(statusLink, headers)
    response <- readCache(report, requestKey)
    if (is.null(response)) {
      # Wait 15 seconds for report to complete
      Sys.sleep(15)
      response <- httr::GET(statusLink, httr::add_headers(headers))
    }
    httr::stop_for_status(response)
    currentStatus <- jsonlite::fromJSON(httr::content(response, "text"))

    tries <- tries + 1
    if (reportComplete(currentStatus)) {
      writeCache(report, requestKey, response)
      resultsLink <- with(currentStatus, links[links$name == "Results", "href"])
      break
    }
    else if (tries >= maxTries) {
      stop("Report hasn't completed - timing out.")
    }
  }

  # resultsLink should now be ready
  print("Report ready, pulling results")
  results <- as.data.frame(loadPages(report, resultsLink), stringsAsFactors = FALSE)
  if (!is.null(report$columnNames)) {
    names(results) <- report$columnNames
  }

  results
}

