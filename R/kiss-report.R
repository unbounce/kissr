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
#'    # Using a people_search_v2 report
#'    reportUrl <- "https://query.kissmetrics.com/v2/products/6581c29e-ab13-1030-97f2-22000a91b1a1/reports/1c564450-3586-0133-85e2-22000a9a8afc/run"
#'    report <- KissReport(reportUrl,
#'                        interval = lubridate::interval(as.Date("2015-06-01"), as.Date("2015-06-02")),
#'                        columnNames = c("KM_Email", "KM_FirstUserId", "FirstVisitDate",
#'                                        "FirstSource", "FirstMedium",
#'                                        "FirstCampaignName", "FirstCampaignContent", "FirstCampaignTerms",
#'                                        "FirstReferrer"))
#'    reportResults <- read(report)
#'    # Or using a people_search_v3 report
#'    reportUrlV3 <- "https://query.kissmetrics.com/v2/products/6581c29e-ab13-1030-97f2-22000a91b1a1/reports/893583b0-0bec-0134-ab92-22000ab4dcd7/run"
#'    reportV3 <- KissReport(reportUrlV3,
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
#' NOTE:KissMetrics returns times in the time zone the product is configured in.
#' If you would like to change the times to be in UTC you can specify the Time
#' Zone Name used by KissMetrics in the
#' KISSR__KISSMETRICS_CONFIGURED_TIMEZONE_ZONENAME env var and this will return
#' all times translated back to UTC from that timezone.
#' @examples
#'    Sys.setenv(KISSR__KISSMETRICS_CONFIGURED_TIMEZONE_ZONENAME="America/Vancouver")
#'    reportUrl <- "https://query.kissmetrics.com/v2/products/6581c29e-ab13-1030-97f2-22000a91b1a1/reports/1c564450-3586-0133-85e2-22000a9a8afc/run"
#'    report <- KissReport(reportUrl,
#'                        interval = lubridate::interval(as.Date("2015-06-01"), as.Date("2015-06-02")),
#'                        columnNames = c("KM_Email", "KM_FirstUserId", "FirstVisitDate",
#'                                        "FirstSource", "FirstMedium",
#'                                        "FirstCampaignName", "FirstCampaignContent", "FirstCampaignTerms",
#'                                        "FirstReferrer"))
#'    reportResults <- read(report)
#'    # All times from KissMetrics will be treated as if they are in America/Vancouver
#'    # time while the timez in reportResults will be in UTC (so 7 or 8 hours later
#'    # depending on DST)
#'
#' @return  A \code{data.frame} containing all the data in the report
#' @export
read.KissReport <- function(report) {
  # Make request
  headers <- c(authorizationHeader(), jsonHeader())
  body <- buildReportRequestPayload()
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

  # We don't necessarily want the times in the timezone KM returns them in.
  # Use try_convert_time to see if any of the columns are filled with times as
  # strings. If so then convert them to POSIX.ct classes with the timezone data
  # from FROM_TIMEZONE. try_convert_time will then force the UTC timezone for
  # all returned time values.
  map_dates_results <- dplyr::mutate_each_(results, dplyr::funs(try_convert_time), names(results))
  map_dates_results
}

