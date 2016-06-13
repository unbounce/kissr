#' asJson s3 generic
#'
#' This is a generic function that dispatches to the specific asJson
#' implementation. For instance \link{asJson.KissReport} or
#' \link{asJson.KissRule}
#'
#' @export
asJson <- function(object, ...) {
  UseMethod("asJson", object)
}

#' @export
asJson.default <- function(object, ...) {
  jsonlite::toJSON(object)
}
