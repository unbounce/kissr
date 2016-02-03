#' Read s3 generic
#'
#' This is a generic function that dispatches to the specific read
#' implementation. For instance \link{read.KissReport} or
#' \link{read.KissReports}
#'
#' @export
read <- function(object, ...) {
  UseMethod("read", object)
}
