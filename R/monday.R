#' Monday
#'
#' @description A convenience wrapper to get the monday associated with a particular date.
#'
#' @param date `str / date` Date for which to find monday.
#'
#' @examples
#' monday('2023-06-25')
#'
#' @importFrom lubridate floor_date
#' @export
monday <- function(date) {
  out <- date %>%
    as.Date() %>%
    lubridate::floor_date(week_start = 2) %>%
    as.Date()

  return(out)
}

