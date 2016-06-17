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

#' Default asJson implementation - by default we just call jsonlite::toJSON
#' @export
asJson.default <- function(object, ...) {
  jsonlite::toJSON(object)
}

