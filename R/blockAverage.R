#' Calculate Yearly Means for Event Metrics.
#'
#' @param data Accepts the data returned by the \code{\link{detect}} function.
#' @param report Specify either 'full' or 'partial'. Selecting 'full' causes
#' the report to contain NAs for any years in which no events were detected
#' (except for \code{count}, which will be zero in those years), while 'partial'
#' reports only the years wherein events were detected. The default is 'full'.
#'
#' @details
#' This function needs to be provided with the full output from the \code{detect}
#' function. Note that the yearly averages are calculted only for "full" years
#' (i.e. years that start/end part-way through the year at the beginning or end
#' of the original time series are removed from the calculations).
#'
#' This function differs from the python implementation of the function of the
#' same name (i.e., \code{blockAverage}, see \url{https://github.com/ecjoliver/marineHeatWaves})
#' in that we only provide the ability to calculate the average (or aggregate)
#' event metrics in 'blocks' of one year, while the python version allows
#' arbitrary (integer) block sizes.
#'
#' @return The function will return a data frame of the averaged (or aggregate)
#' metrics. It includes the following:
#'   \item{year}{The year over which the metrics were averaged.}
#'   \item{temp_mean}{Seawater temperature for the specified year [deg. C].}
#'   \item{temp_min}{The minimum temperature for the specified year [deg. C].}
#'   \item{temp_max}{The maximum temperature for the specified year [deg. C].}
#'   \item{count}{The number of events per year.}
#'   \item{duration}{The average duration of events per year [days].}
#'   \item{int_mean}{The average event "mean intensity" in each year [deg. C].}
#'   \item{int_max}{The average event "maximum (peak) intensity" in each year
#'   [deg. C].}
#'   \item{int_var}{The average event "intensity variability" in each year
#'   [deg. C].}
#'   \item{int_cum}{The average event "cumulative intensity" in each year
#'   [deg. C x days].}
#'   \item{rate_onset}{Average event onset rate in each year [deg. C / days].}
#'   \item{rate_decline}{Average event decline rate in each year [deg. C / days].}
#'   \item{total_days}{Total number of events days in each year [days].}
#'   \item{total_icum}{Total cumulative intensity over all events in each year [deg. C x days].}
#'
#' \code{int_max_rel_thresh}, \code{int_mean_rel_thresh},
#' \code{int_var_rel_thresh}, and \code{int_cum_rel_thresh}
#' are as above except relative to the threshold (e.g., 90th percentile) rather
#' than the seasonal climatology.
#'
#' \code{int_max_abs}, \code{int_mean_abs}, \code{int_var_abs}, and
#' \code{int_cum_abs} are as above except as absolute magnitudes
#' rather than relative to the seasonal climatology or threshold.
#'
#' \code{int_max_norm} and \code{int_mean_norm} are as above except
#' units are in multiples of threshold exceedances, i.e., a value of 1.5
#' indicates the event intensity (relative to the climatology) was 1.5 times the
#' value of the threshold (relative to climatology,
#' i.e., threshold - climatology.)
#'
#' @author Albertus J. Smit, Eric C. J. Oliver
#'
#' @references Hobday, A.J. et al. (2016), A hierarchical approach to defining
#' marine heatwaves, Progress in Oceanography, 141, pp. 227-238,
#' doi: 10.1016/j.pocean.2015.12.014
#'
#' @export
#'
#' @examples
#' t_dat <- make_whole(sst_Med)
#' res <- detect(t_dat) # using default values
#' out <- block_average(res)
#' summary(glm(count ~ year, out, family = "poisson"))
#'
#' \dontrun{
#' plot(out$year, out$count, col = "salmon", pch = 16,
#'      xlab = "Year", ylab = "Number of events")
#' lines(out$year, out$count)
#' }
block_average <-
  function(data,
           report = "full") {
    temp_yr <- data.frame(
      year = unique(lubridate::year(data$clim$date)),
      temp_mean = aggregate(data$clim$temp, list(lubridate::year(data$clim$date)), mean, na.rm = TRUE)[, 2],
      temp_min = aggregate(data$clim$temp, list(lubridate::year(data$clim$date)), min)[, 2],
      temp_max = aggregate(data$clim$temp, list(lubridate::year(data$clim$date)), max)[, 2],
      check.names = FALSE
    )

    event_block <- data.frame(
      year = unique(lubridate::year(data$event$date_start)),
      count = aggregate(data$event$duration, list(lubridate::year(
        data$event$date_start
      )), length)[, 2],
      duration = aggregate(data$event$duration, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_mean = aggregate(data$event$int_mean, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_max = aggregate(data$event$int_max, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_var = aggregate(data$event$int_var, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_cum = aggregate(data$event$int_cum, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_mean_rel_thresh = aggregate(data$event$int_mean_rel_thresh, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_max_rel_thresh = aggregate(data$event$int_max_rel_thresh, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_var_rel_thresh = aggregate(data$event$int_var_rel_thresh, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_cum_rel_thresh = aggregate(data$event$int_cum_rel_thresh, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_mean_abs = aggregate(data$event$int_mean_abs, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_max_abs = aggregate(data$event$int_max_abs, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_var_abs = aggregate(data$event$int_var_abs, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_cum_abs = aggregate(data$event$int_cum_abs, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_mean_norm = aggregate(data$event$int_mean_norm, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      int_max_norm = aggregate(data$event$int_max_norm, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      rate_onset = aggregate(data$event$rate_onset, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      rate_decline = aggregate(data$event$rate_decline, list(lubridate::year(
        data$event$date_start
      )), mean)[, 2],
      total_days = aggregate(data$event$duration, list(lubridate::year(
        data$event$date_start
      )), sum)[, 2],
      total_icum = aggregate(data$event$int_cum, list(lubridate::year(
        data$event$date_start
      )), sum)[, 2]
    )

    if (report == "full") {
      event_block <- dplyr::left_join(temp_yr, event_block, by = "year")
    } else if (report == "partial") {
      event_block <-
      dplyr::inner_join(temp_yr, event_block, by = "year")
    } else stop("Oops, 'report' must be either 'full' or 'partial'!")

    event_block$count[is.na(event_block$count)] <- 0

    return(event_block)
  }