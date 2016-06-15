
#' @export
KissCalculation.Event <- function(label, eventId, type, negate, frequencyValue, frequencyOccurance, defaultInterval = NA) {
  options <- c()
  if(!is.na(defaultInterval)) {
    options <- c(defaultDateRange = defaultInterval)
  }

  structure(list(
    type = type,
    subject = list(
      type = "event",
      negate = negate,
      event = eventId,
      frequencyValue = frequencyValue,
      frequencyOccurance = frequencyOccurance,
      version = 2,
      options = options),
    label = label),
    class = c("KissCalculation.Event", "KissCalculation"))
}

#' @export
KissCalculation.Property <- function(label, propertyId, type, negate, comparisonMode, defaultInterval = NA) {
  options <- c()
  if(!is.na(defaultInterval)) {
    options <- c(defaultDateRange = defaultInterval)
  }


  structure(list(
    type = type,
    subject = list(
      type = "event",
      negate = negate,
      property = propertyId,
      comparisonMode = comparisonMode,
      version = 2,
      options = options),
    label = label),
    class = c("KissCalculation.Property", "KissCalculation"))
}

#' @export
asJson.KissCalculation <- function(calculation) {
  json <- jsonlite::toJSON(c(calculation), auto_unbox=TRUE)
  return(json)
}

