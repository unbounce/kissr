#' \code{KissRule.Event} objects are used to define target segments to run a report on.
#' @param eventId Which event are you targeting on? We need the index or id.
#' @param frequencyValue How many times the event needs to have happened for the
#'   rule
#' @param frequencyOccurance How we are comparing against the
#'   \code{frequencyValue}. Must be \code{at_least}, \code{at_most}, or
#'   \code{exactly}
#' @param interval What time frame are we looking at the events over? Defaults
#'   to NA which uses the overall report time frame
#' @param comparisonMode - unclear what this does. Defaults to 'any_value'
#' @param negate Is this an inclusive rule or an exclusionary rule?
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
KissRule.Event <- function(negate, eventId, frequencyValue, frequencyOccurance,
                           interval = NA, comparisonMode = 'any_value') {
  if (!lubridate::is.interval(interval) & !is.na(interval))
    stop("interval must be a valid interval")
  structure(list(
    type = "event",
    negate = negate,
    event = eventId,
    frequencyValue = frequencyValue,
    frequencyOccurance = frequencyOccurance,
    comparisonMode = comparisonMode,
    interval = interval),
    class = c("KissRule.Event", "KissRule"))
}

#' \code{KissRule.Property} objects are used to define target segments to run a
#' report on.
#' @param propertyId Which property are you targeting? We need the index or id.
#' @param comparisonMode How are we comparing the property? Can be any of
#'    \code{any_value}, \code{empty}, \code{equals}, \code{contains},
#'    \code{begins_with}, \code{ends_with}
#' @param comparisonString What should we compare against? Only used if
#'    \code{comparisonMode} is \code{equals}, \code{contains},
#'    \code{begins_with}, or \code{ends_with}
#' @param interval What time frame are we looking at the properties over?
#'   Defaults to NA which uses the overall report time frame
#' @param negate Is this an inclusive rule or an exclusionary rule?
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
#'    rules <- list(KissRule.Property(negate = FALSE, propertyId = 10, comparisonMode = "any_value"),
#'                  KissRule.Property(negate = FALSE, propertyId = 2, comparisonMode = "begins_with", comparisonString = "foo"))
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
KissRule.Property <- function(negate, propertyId, comparisonMode, comparisonString = NA, interval = NA) {
  property <- list(
    type = "property",
    negate = negate,
    property = propertyId,
    comparisonMode = comparisonMode,
    interval = interval)
  if(comparisonMode %in% c("equals", "contains", "begins_with", "ends_with")) {
    if(is.na(comparisonString)) stop("You must provide a comparison string for KissRule.Property comparison modes of 'equals', 'contains', 'begins_with', 'ends_with'")
    property["comparisonString"] <- comparisonString
  }
  structure(property,
    class = c("KissRule.Property", "KissRule"))
}

#' Generates json for a KissRule.
#' @export
asJson.KissRule <- function(rule) {
  template <- '
  {
    "type":"{{type}}",
    "negate": {{negate}},
    "event":{{event}},
    "frequencyValue":{{frequencyValue}},
    "frequencyOccurance":"{{frequencyOccurance}}",
    "comparisonMode":"{{comparisonMode}}",
    "dateRange":{{dateRange}}
  }
  '

  if (!lubridate::is.interval(rule$interval))
    stop("rule must have a valid interval")

  json <- template
  json <- replacePlaceholder(json, "\\{\\{type\\}\\}", rule$type)
  json <- replacePlaceholder(json, "\\{\\{negate\\}\\}", tolower(rule$negate))
  json <- replacePlaceholder(json, "\\{\\{event\\}\\}",rule$event)
  json <- replacePlaceholder(json, "\\{\\{frequencyValue\\}\\}", rule$frequencyValue)
  json <- replacePlaceholder(json, "\\{\\{frequencyOccurance\\}\\}", rule$frequencyOccurance)
  json <- replacePlaceholder(json, "\\{\\{comparisonMode\\}\\}", rule$comparisonMode)
  json <- replacePlaceholder(json, "\\{\\{dateRange\\}\\}",
                               jsonlite::toJSON(makeKMDateRange(rule$interval),
                                                auto_unbox = TRUE))

  json
}

