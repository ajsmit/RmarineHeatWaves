#' Constructs a Continuous, Uninterrupted Time Series of Temperatures.
#'
#' Takes a series of dates and times, and if irregular (but ordered), inserts
#' missing dates and fills correpsonding temperatures with NAs.
#'
#' @param data A data frame with columns headed \code{t} and \code{temp} for
#' date and temperature data, respectively. Daily data are expected, and
#' although missing values (NA) can be accommodated, the function is only
#' recommended when NAs occur infrequently, preferably at no more than 3
#' consequtive days.
#'
#' @details
#' Upon import, the package uses `zoo` and `lubridate` to process the input
#' date and temperature data. It reads in daily data with the time vector
#' specified as either \code{POSIXct} (e.g. "1982-01-01 02:00:00") or class
#'\code{date} (e.g. "1982-01-01"). The data may be an irregular time series,
#' but date must be ordered. The function constructs a complete time series
#' from the start date to the end date, and fills in the regions in the time
#' series where data are missing with NAs. Leap years are automatically
#' accommodated by 'zoo'.
#'
#' This function can handle any amount of missing days, but this is not a
#' licence to actually use these data for the detection of anomalous thermal
#' events. Hobday et al. (2016) recommend gaps of no more than 3 days, which
#' may be adjusted by setting the \code{max_pad_length} argument of the
#' \code{\link{detect}} function. The longer and more frequent the gaps become
#' the lower the fidelity of the annual climatology and threshold that can be
#' calculated, which will not only have repercussions for the accuracy at which
#' the event metrics can be calculated, but also for the number of events that
#' can be detected.
#'
#' @return The function will return a data frame with three columns. The column
#' headed \code{doy} (day-of-year) is the Julian day running from 1 to 366, but
#' modified so that the day-of-year series for non-leap-years runs 1...59 and
#' then 61...366. For leap years the 60th day is February 29. See the example,
#' below. The \code{date} column is a series of dates of class \code{Date},
#' while \code{temp} is temperature. This time series will be uninterrupted and
#' continuous between the first and last dates of the input data.
#' @export
#'
#' @author Smit, A. J.
#'
#' @examples
#' require(dplyr); require(reshape2); require(lubridate)
#' ts_dat <- make_whole(sst_WA)
#' clim_start <- "1983-01-01"
#' clim_end <- "2012-12-31"
#' ts_dat %>%
#'    filter(date >= clim_start & date <= clim_end) %>%
#'    dcast(doy ~ year(date), value.var = "temp", fill = as.numeric(NA)) %>%
#'    as.data.frame() %>%
#'    filter(doy >= 55 & doy <= 65)
# TODO: cause function to provide helpful output, such as % NAs per year, the
# number of missing dates filled in, and the start and end dates of the time
# series.
make_whole <- function(data) {
  # TODO: make function stop if neither POSIXct or Date class...
  if (lubridate::is.POSIXct(data$t)) {
    data$t <- as.Date(data$t)
    tSeries <- zoo::zoo(data$temp, data$t) # still with gaps
  } else {
    tSeries <- zoo::zoo(data$temp, data$t) # still with gaps
  }
  strt <- start(tSeries)
  fin <- end(tSeries)
  ser <-
    data.frame(t = seq(strt, fin, by = "1 day")) # creates a reg. time series
  ser <- zoo::zoo(rep(NA, length(ser$t)), order.by = ser$t)
  tSeries <- merge(ser, tSeries)[, 2] # fills gaps with NAs

  # Modify day-of-year vector so that non-leap-years run 1...59 then 61...366.
  feb28 <- 59
  doy <- NULL ###
  tSeries <-
    data.frame(
      doy = lubridate::yday(tSeries),
      date = as.Date(as.POSIXct(tSeries)),
      temp = tSeries,
      row.names = NULL
    ) %>%
    dplyr::mutate(doy = ifelse(
      lubridate::leap_year(lubridate::year(tSeries)) == FALSE,
      ifelse(doy > feb28, doy + 1, doy),
      doy
    ))
}