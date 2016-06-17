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
#' @examples
#'    firstTimeVisitedCalculation <- KissCalculation.Event(
#'                      label = "First time of visited site",
#'                      eventId = 6,
#'                      type = "first_date_in_range")
#'    lastTimeVisitedCalculation <- KissCalculation.Event(
#'                      label = "Last time of visited site",
#'                      eventId = 6,
#'                      type = "last_date_in_range")
#'    reportDates <- lubridate::interval(as.Date("2015-06-01"), as.Date("2015-06-02"))
#'    rules <- list(KissRule.Event(FALSE, 72, 1, "at_least", "any_value"))
#'    segment <- KissSegment(type = "and",
#'                 rules = rules,
#'                 defaultInterval = reportDates)
#'    report <- KissReport(productId = "6581c29e-ab13-1030-97f2-22000a91b1a1",
#'                 segment = segment,
#'                 calculations = list(
#'                   firstTimeVisitedCalculation,
#'                   lastTimeVisitedCalculation
#'                 ),
#'                 interval = reportDates
#'              )
#'    reportResults <- read(report)
#' @export
KissCalculation.Event <- function(label,
                                  eventId,
                                  type,
                                  interval = NA,
                                  frequencyValue = 1,
                                  frequencyOccurance = 'at_least',
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
#' @param propertyId Which property are you calculating on. We need the index or
#'   id.
#' @param label The column name in the final report results.
#' @parma type What sort of calculation do you want to make? For a
#'   \code{KissCalculation.Property} you have the following options:
#'   \code{first_date_in_range}, \code{last_date_in_range},
#'   \code{first_value_in_range}, \code{last_value_in_range}.
#' @param comparisonMode - unclear what this does - defaults to
#'   \code{'any_value'}
#' @param interval What time frame are we looking at the events over? Defaults
#'   to NA which uses the overall report time frame
#' @param negate Unclear what this does - defaults to \code{FALSE}
#'
#' @examples
#'    firstCampaignSourceCalculation <- KissCalculation.Property(
#'                      label = "First campaign source",
#'                      propertyId = 7,
#'                      type = "first_value_in_range")
#'    lastCampaignSourceCalculation <- KissCalculation.Property(
#'                      label = "Last campaign source",
#'                      propertyId = 7,
#'                      type = "last_value_in_range")
#'    reportDates <- lubridate::interval(as.Date("2015-06-01"), as.Date("2015-06-02"))
#'    rules <- list(KissRule.Event(FALSE, 72, 1, "at_least", "any_value"))
#'    segment <- KissSegment(type = "and",
#'                 rules = rules,
#'                 defaultInterval = reportDates)
#'    report <- KissReport(productId = "6581c29e-ab13-1030-97f2-22000a91b1a1",
#'                 segment = segment,
#'                 calculations = list(
#'                   firstCampaignSourceCalculation,
#'                   lastCampaignSourceCalculation
#'                 ),
#'                 interval = reportDates
#'              )
#'    reportResults <- read(report)
#'
#' @export
KissCalculation.Property <- function(label,
                                     propertyId,
                                     type,
                                     interval = NA,
                                     comparisonMode = 'any_value',
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
      property = propertyId,
      comparisonMode = comparisonMode,
      version = 2,
      options = options),
    label = label),
    class = c("KissCalculation.Property", "KissCalculation"))
}

#' Convert a KissCalculation into a json structure understood by the KM API.
#' @export
asJson.KissCalculation <- function(calculation) {
  json <- jsonlite::toJSON(c(calculation), auto_unbox=TRUE)
  return(json)
}

