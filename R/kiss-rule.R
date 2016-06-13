
#' @export
KissRule.Event <- function(negate, eventId, frequencyValue, frequencyOccurance, comparisonMode) {
  structure(list(
    type = "event",
    negate = negate,
    event = eventId,
    frequencyValue = frequencyValue,
    frequencyOccurance = frequencyOccurance,
    comparisonMode = comparisonMode),
    class = c("KissRule.Event", "KissRule"))
}

#' @export
KissRule.Property <- function(negate, propertyId, comparisonMode) {
  structure(list(
    type = "property",
    negate = negate,
    property = propertyId,
    comparisonMode = comparisonMode),
    class = c("KissRule.Property", "KissRule"))
}

#' @export
asJson.KissRule <- function(rule) {
  json <- jsonlite::toJSON(c(rule), auto_unbox=TRUE)
  return(json)
}

