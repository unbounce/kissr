#' S3 Object Describing a set of Kiss Reports
#'
#'  \code{KissReports} creates a new S3 object of with class "KissReports"
#'
#'  This is a constructor function.
#'
#' @return A new KissReports object that can be passed to the read S3 generic to
#'   access all reports available in your configured account
#' @examples
#'    reports <- read(KissReports())
#'    signups_report <- reports[reports$name == "Datawarehouse - KM Signed Up User Extract", ]
#'    report <- KissReport(signups_report$url,
#'                        interval = lubridate::interval(as.Date("2015-06-01"), as.Date("2015-06-02")),
#'                        columnNames = c("KM_Email", "KM_FirstUserId", "FirstVisitDate",
#'                                        "FirstSource", "FirstMedium",
#'                                        "FirstCampaignName", "FirstCampaignContent", "FirstCampaignTerms",
#'                                        "FirstReferrer"))
#'    reportResults <- read(report)
#' @export
KissReports <- function() {
  url <- httr::parse_url("https://api.kissmetrics.com/core/reports")

  structure(list(
    url = url),
    class = "KissReports")
}


#' Read a list of reports from a KissReports object
#'
#' This function reads all the data from a KissMetrics people report
#' returning everything in a data frame. Please note this is a synchronous
#' function that can take quite some time to complete.
#'
#' @return  A \code{data.frame} containing report \code{product_id},
#'   \code{account_id}, \code{report_type}, \code{name}, \code{created_at} date
#'   and \code{url}. Note - only reports with \code{report_type} ==
#'   \code{people_search_v2} can be queried currently
#' @export
read.KissReports <- function(reports) {
  # Make request
  headers <- c(authorizationHeader(), jsonHeader())
  body <- ""
  encoding <- "json"

  requestKey <- c(reports$url, headers, body, encoding)
  response <- readCache(reports, requestKey)
  if (is.null(response)) {
    response <- httr::GET(reports$url,
                           encode = encoding,
                           httr::add_headers(.headers = headers))

    httr::stop_for_status(response)
    writeCache(reports, requestKey, response)
  }

  # Get list of reports
  links  <- jsonlite::fromJSON(httr::content(response, "text"))$links

  reportsLink <- links[links$name == "First", "href"]
  print("Getting all reports")
  results <- as.data.frame(loadPages(reports, reportsLink), stringsAsFactors = FALSE)

  # We want to get only the url we can use to run the v2 version of the report
  # from the links associated with the report. None of the other links are useful
  urlExtractor <- function(df) { df[df$rel == 'run_v2', 'href'] }
  results$url  <- sapply(results$links, urlExtractor)
  results$links <- NULL
  results
}
