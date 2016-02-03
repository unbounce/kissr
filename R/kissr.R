#' kissr: A package for loading report data from Kiss Metrics
#'
#' The kissr package provides one function:
#' `read`
#'
#' You can call the read function with a \link{KissReports} object to get a list
#' of all reports associated with your KissMetrics account and you can call the
#' read function with a \link{KissReport} object to get the actual data from a
#' report.
#'
#' Please note the KissMetrics API requires some time to access, kissr will wait
#' for up to 150 seconds for KissMetrics to fully generate a report. If it takes
#' longer, kissr will time out.
#'
#' @docType package
#' @name kissr
NULL

