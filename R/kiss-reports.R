#' S3 Object Describing a set of Kiss Reports
#'
#'  \code{KissReports} creates a new S3 object of with class "KissReports"
#'
#'  This is a constructor function.
#'
#'  @export
KissReports <- function() {
  url <- httr::parse_url("https://api.kissmetrics.com/core/reports")
  structure(list(
    url = url),
    class = "KissReports")
}
