#' Create a new KissSegment
#'
#' \code{KissSegment} creates a new S3 object of with class "KissSegment"
#'
#' This is a constructor function.
#' @examples
#'  rules <- list(KissRule.Event(FALSE, 72, 1, "at_least", "any_value"))
#'  segment <- KissSegment(type = "and",
#'                         rules = rules,
#'                         defaultInterval = lubridate::interval(as.Date("2015-06-01"), as.Date("2015-06-02")))
#'
#' @param type 'and' or 'or' - how should the rules for the segment be combined
#' @export
KissSegment <- function(type, rules, defaultInterval = NA) {
  options <- c()
  if(!is.na(defaultInterval)) {
    options <- list(defaultDateRange = defaultInterval)
  }

  segment <- structure(list(
    type = type,
    rules = rules,
    options = options),
    class = "KissSegment")
}


#' @export
asJson.KissSegment <- function(segment) {
  segmentTemplate <- '{"type":"{{type}}", "operands":[{{rules}}], "options": {{options}}}'

  json <- segmentTemplate
  json <- replacePlaceholder(json, "\\{\\{type\\}\\}", segment$type)
  rulesJson <- lapply(segment$rules, asJson)
  json <- replacePlaceholder(json, "\\{\\{rules\\}\\}", paste(rulesJson, collapse=","))
  json <- replacePlaceholder(json,
                             "\\{\\{options\\}\\}",
                             jsonlite::toJSON(lapply(segment$options, as.list), auto_unbox=TRUE))
  return(json)
}
