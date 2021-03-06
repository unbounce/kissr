#' Create a new KissReport
#'
#' \code{KissReport} creates a new S3 object of with class "KissReport"
#'
#' This is a constructor function.
#'
#' @param productId The product id to run the report against
#' @param segment - a \link{KissSegment} specifying the segment to run the report
#'   against.
#' @param calculations - a list of \link{KissCalculation} objects representing
#'   the columns of the report - any calculations with type of
#'   \code{first_date_in_range} or \code{last_date_in_range} will be returned as
#'   a \code{POSIXct} class when report is read. Anything else will be a
#'   \code{character}.
#' @param interval The time range you want the report run for as a
#'   \code{lubridate::interval}
#'   (optional - can specify start and end instead)
#' @return If url is a valid string or url (from \code{httr::url}) and the start
#'   and end dates are specified this returns a new KissReport object that can
#'   be passed to the read S3 generic
#' @examples
#'    reportDates <- lubridate::interval(as.Date("2015-06-01"), as.Date("2015-06-02"))
#'    rules <- list(KissRule.Event(FALSE, 72, 1, "at_least", "any_value",
#'                                interval = reportDates))
#'    segment <- KissSegment(type = "and",
#'                 rules = rules,
#'                 defaultInterval = reportDates)
#'    report <- KissReport(productId = "6581c29e-ab13-1030-97f2-22000a91b1a1",
#'                 segment = segment,
#'                 calculations = list(
#'                   KissCalculation.Event(label = "First time of visited site",
#'                      eventId = 6,
#'                      type = "first_date_in_range",
#'                      frequencyValue = 1,
#'                      frequencyOccurance = "at_least")),
#'                 interval = reportDates
#'                 )
#'    reportResults <- read(report)
#' @export
KissReport <- function(productId, segment, calculations, interval) {
  if (!is.character(productId)) stop("productId must be a string")
  if (!lubridate::is.interval(interval)) stop("interval must be a valid interval")
  if (!is(segment, "KissSegment"))  stop("segment must be a KissSegment object")
  if (!is.list(calculations))  stop("calculations must be a list")
  if ( length(calculations) > 0 & all(!sapply(calculations, is, "KissCalculation"))) {
    stop("calculations must contain only KissCalculations")
  }

  cache <- new.env(parent = emptyenv())

  urlTemplate <- "https://query.kissmetrics.com/v2/products/{{product_id}}/reports/people_search"

  url <- httr::parse_url(replacePlaceholder(urlTemplate, "\\{\\{product_id\\}\\}", productId))
  structure(list(
    productId = productId,
    url = url,
    interval = interval,
    cache = cache,
    segment = segment,
    calculations = calculations),
    class = "KissReport")
}


#' Read data from a KissReport
#'
#' This function reads all the data from a KissMetrics people report
#' returning everything in a data frame. Please note this is a synchronous
#' function that can take quite some time to complete.
#'
#' NOTE:KissMetrics returns times in UTC
#' @examples
#'    reportDates <- lubridate::interval(as.Date("2015-06-01"), as.Date("2015-06-02"))
#'    rules <- list(KissRule.Event(FALSE, 72, 1, "at_least", "any_value"))
#'    segment <- KissSegment(type = "and",
#'                 rules = rules,
#'                 defaultInterval = reportDates)
#'    report <- KissReport(productId = "6581c29e-ab13-1030-97f2-22000a91b1a1",
#'                 segment = segment,
#'                 calculations = list(
#'                   KissCalculation.Event(label = "First time of visited site",
#'                      eventId = 6,
#'                      type = "first_date_in_range",
#'                      negate = FALSE,
#'                      frequencyValue = 1,
#'                      frequencyOccurance = "at_least")),
#'                 interval = reportDates
#'                 )
#'    reportResults <- read(report)
#'
#' @return  A \code{data.frame} containing all the data in the report. The
#'    columns in the data frame will be "identity" + any columns defined by the
#'    report's calculations. Any time values will be converted to POSIXct and
#'    will be in UTC.
#' @export
read.KissReport <- function(report) {
  # Make request
  headers <- c(authorizationHeader(), jsonHeader())
  body <- asJson(report)
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

  # Set the names of the results to be "identity" + the labels used for the report calculations.
  # We have to handle this a little differently if the report generates no results.
  if(ncol(results) == 0) {
    results <- as.data.frame(setNames(replicate(length(names(report)), character(0)), names(report)))
  } else {
    names(results) <- names(report)
  }

  # KissMetrics returns times as unix timestamps. However seconds are
  # from timezone set within KissMetrics Product Info (not necessarily UTC)
  # Update every column generated from a calculation with type matching *_date_*
  # to be POSIX.ct time.
  convertTime <- function(x) {
    timeVariable <- as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC")
    # Correct timezone to UTC
    systemTimezone <- Sys.getenv("KISSR__KISSMETRICS_CONFIGURED_TIMEZONE_ZONENAME")
    if(systemTimezone == "") {
      systemTimezone <- "UTC"
    }
    timeVariable <- as.POSIXct(as.character(timeVariable),
                               systemTimezone)
    attr(timeVariable,"tzone") <- "UTC"
    timeVariable
  }
  results <- dplyr::mutate_each(results,
                                dplyr::funs(convertTime),
                                which(reportCalculationClasses(report) == "timestamp"))

  results
}

#' Convert a KissCalculation into a json structure understood by the KM API.
#' Used internally by read.KissReport but also available for external
#' introspection
#' @export
asJson.KissReport <- function(report) {
  template <- '
  {
    "sort":"0",
    "order":"asc",
    "product_id":"{{product_id}}",
    "query_params": {
      "type":"group",
      "filter":{{segment}},
      "defaultCalculationDateRange":{{defaultCalculationDateRange}},
      "calculations":[{{calculations}}]
    }
  }'

  json <- template
  json <- replacePlaceholder(json, "\\{\\{product_id\\}\\}", report$productId)
  json <- replacePlaceholder(json, "\\{\\{segment\\}\\}", asJson(report$segment))
  json <- replacePlaceholder(json, "\\{\\{defaultCalculationDateRange\\}\\}", jsonlite::toJSON(makeKMDateRange(report$interval), auto_unbox = TRUE))
  calculationsJson <- lapply(report$calculations, asJson)
  json <- replacePlaceholder(json, "\\{\\{calculations\\}\\}", paste(calculationsJson, collapse=","))
  return(json)
}

#' Get the column names for a report
#' @example
#'    reportDates <- lubridate::interval(as.Date("2015-06-01"), as.Date("2015-06-02"))
#'    rules <- list(KissRule.Event(FALSE, 72, 1, "at_least", "any_value"))
#'    segment <- KissSegment(type = "and",
#'                 rules = rules,
#'                 defaultInterval = reportDates)
#'    report <- KissReport(productId = "6581c29e-ab13-1030-97f2-22000a91b1a1",
#'                 segment = segment,
#'                 calculations = list(
#'                   KissCalculation.Event(label = "First time of visited site",
#'                      eventId = 6,
#'                      type = "first_date_in_range",
#'                      negate = FALSE,
#'                      frequencyValue = 1,
#'                      frequencyOccurance = "at_least")),
#'                 interval = reportDates
#'                 )
#'    cat("Generating report with columns: ", paste(names(report), collapse=" | "))
#' @export
names.KissReport <- function(report) {
  c('identity', sapply(report$calculations, function(calculation) calculation$label))
}
