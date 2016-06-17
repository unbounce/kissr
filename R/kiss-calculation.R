#' A \code{KissCalculation} represents a column for a report. To create a
#' calculation you need to know if you want the column to represent a value of
#' an event or of a property. \code{KissCalculation.Event} should be used
#' when you are generating a report that shows the first time, last time, or
#' total times an event has occured
#' @param eventId Which event are you calculating on. We need the index or id.
#' @param label The column name in the final report results.
#' @parma type What sort of calculation do you want to make? For a
#'   \code{KissCalculation.Event} you have the following options:
#'   \code{first_date_in_range}, \code{last_date_in_range},
#'   \code{total_times_in_range}.
#' @param frequencyValue - unclear what this does - defaults to \code{1}
#' @param frequencyOccurance - unclear what this does - defaults to
#'   \code{'at_least'}
#' @param interval What time frame are we looking at the events over? Defaults
#'   to NA which uses the overall report time frame
#' @param negate Unclear what this does - defaults to \code{FALSE}
#'
#' @export
KissCalculation.Event <- function(label,
                                  eventId,
                                  type,
                                  frequencyValue = 1,
                                  frequencyOccurance = 'at_least',
                                  interval = NA,
                                  negate = FALSE) {
  options <- c()
  if(!is.na(interval)) {
    options <- c(defaultDateRange = interval)
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

#' A \code{KissCalculation} represents a column for a report. To create a
#' calculation you need to know if you want the column to represent a value of
#' an event or of a property.
#' @param eventId Which event are you calculating on. We need the index or id.
#' @param label The column name in the final report results.
#' @parma type What sort of calculation do you want to make? For a
#'   \code{KissCalculation.Event} you have the following options:
#'   \code{first_date_in_range}, \code{last_date_in_range},
#'   \code{total_times_in_range}.
#' @param frequencyValue - unclear what this does - defaults to \code{1}
#' @param frequencyOccurance - unclear what this does - defaults to
#'   \code{'at_least'}
#' @param negate Unclear what this does - defaults to \code{FALSE}
#'
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

