#' Detect Marine Heat Waves and Marine Cold Spells.
#'
#' Applies the Hobday et al. (2016) marine heat wave definition to an input time
#' series of temperature along with a daily date vector.
#'
#' @importFrom magrittr %>%
#' @importFrom plyr .
#' @param data A data frame with three columns. They are headed \code{doy},
#' which is the Julian day running from 1 to 366, but modified so that the
#' day-of-year (doy) vector for non-leap-years runs 1...59 and then
#' 61...366. For leap years the 60th day is February 29. The \code{date} column
#' is a vector of dates of class \code{Date}, while \code{temp} is the
#' temperature. Data of the appropriate format are created by the function
#' \code{\link{make_whole}}, but your own data can be supplied if they meet the
#' criteria specified by \code{\link{make_whole}}.
#' @param climatology_start The first full year from which the (varying by
#' day-of-year) seasonal cycle and extremes threshold are calculated (full being
#' 366 days if leap year, else 365 days). Note that a default value is provided
#' but that it may not be suitable for your own data. Default is \code{1983}.
#' @param climatology_end The last full year up to which the  seasonal cycle
#' and extremes threshold are calculated. Default is \code{2012} (but see comment
#' above).
#' @param pctile Threshold percentile (\%) for detection of extreme values.
#' Default is \code{90}th percentile for marine heat waves and \code{10}th
#' percentile for marine cold spells.
#' @param window_half_width Width of sliding window about day-of-year (to one
#' side of the center day-of-year) used for the pooling of values and
#' calculation of climatology and threshold percentile. Default is \code{5}
#' days, which gives a window width of 11 days centered on the 6th day of the
#' series of 11 days.
#' @param smooth_percentile Full width of moving average window for smoothing
#' climatology and threshold. Default is \code{31} days.
#' @param smooth_percentile_width Boolean switch selecting whether to smooth the
#' climatology and threshold percentile timeseries with a moving average of
#' width \code{smooth_percentile}. Default is \code{TRUE}.
#' @param min_duration Minimum duration for acceptance of detected MHWs.
#' Default is \code{5} days.
#' @param join_across_gaps Boolean switch indicating whether to join MHWs which
#' occur before/after a short gap as specified by \code{max_gap}. Default
#' is \code{TRUE}.
#' @param max_gap Maximum length of gap allowed for the joining of MHWs. Default
#' is \code{2} days.
#' @param max_pad_length Specifies the maximum length of days over which to
#' interpolate (pad) missing data (specified as \code{NA}) in the input
#' temperature time series; i.e., any consecutive blocks of NAs with length
#' greater than \code{max_pad_length} will be left as \code{NA}. Set as an
#' integer. Default is \code{3} days.
#' @param cold_spells Boolean specifying if the code should detect cold events
#' instead of heat events. Default is \code{FALSE}.
#'
#' @details
#' \enumerate{
#' \item This function assumes that the input time series consists of continuous
#' daily values with few missing values. Time ranges which start and end
#' part-way through the calendar year are supported. The accompanying function
#' \code{\link{make_whole}} aids in the preparation of a time series that is
#' suitable for use with \code{detect}, although this may also be accomplished
#' 'by hand' as long as the criteria are met as discussed in the documentation
#' to \code{\link{make_whole}}.
#' \item It is recommended that a climatology period of at least 30 years is
#' specified in order to capture decadal thermal periodicities. Currently the
#' function will only compute climatologies starting from 1 January of the
#' specified \code{climatology_start} and ending on 31 December of the specified
#' \code{climatology_end}. Even one day short of a full year (i.e. 365 day during
#' non-leap years and 366 days during leap years) at the beginning/end of the
#' climatology period will cause the function to fail. This may be changed in
#' future versions of the function.
#' \item This function supports leap years. This is done by ignoring Feb 29s
#' for the initial calculation of the climatology and threshold. The values for
#' Feb 29 are then linearly interpolated from the values for Feb 28 and Mar 1.
#' \item The calculation of onset and decline rates assumes that the events
#' started a half-day before the start day and ended a half-day after the
#' end-day. This is consistent with the duration definition as implemented,
#' which assumes duration = end day - start day + 1.
#' \item For the purposes of event detection, any missing temperature values not
#' interpolated over (through optional \code{maxPadLLength}) will be set equal
#' to the seasonal climatology. This means they will trigger the end/start of
#' any adjacent temperature values which satisfy the event definition criteria.
#' \item If the code is used to detect cold events (\code{coldSpells} = TRUE),
#' then it works just as for heat waves except that events are detected as
#' deviations below the (100 - pctile)th percentile  (e.g., the 10th instead of
#' 90th) for at least 5 days. Intensities are reported as negative values and
#' represent the temperature anomaly below climatology.
#' }
#' The original Python algorithm was written by Eric Oliver, Institute for
#' Marine and Antarctic Studies, University of Tasmania, Feb 2015, and is
#' documented by Hobday et al. (2016). The marine cold spell option was
#' implemented in version 0.13 (21 Nov 2015) of the Python module as a result
#' of our preparation of Schlegel et al. (submitted), wherein the cold events
#' receive a brief overview.
#'
#' @return The function will return a list of two components, \code{clim} and
#' \code{event}, which are the climatology and MHW (or MCS) events, respectively. The
#' climatology contains the full time series of daily temperatures, as well as
#' the the seasonal climatology, the threshold and various aspects of the
#' events that were detected:
#'   \item{doy}{Julian day (day-of-year). For non-leap years it runs 1...59 and
#'   61...366, while leap years run 1...366.}
#'   \item{date}{The date of the temperature measurement.}
#'   \item{temp}{Seawater temperature on the specified date [deg. C].}
#'   \item{seas_clim_year}{Climatological seasonal cycle [deg. C].}
#'   \item{thresh_clim_year}{Seasonally varying threshold (e.g., 90th
#'   percentile) [deg. C].}
#'   \item{thresh_criterion}{Boolean indicating if \code{temp} exceeds
#'   \code{thresh_clim_year}.}
#'   \item{duration_criterion}{Boolean indicating whether periods of consecutive
#'   \code{thresh_criterion} are >= \code{min_duration}.}
#'   \item{event}{Boolean indicting if all criteria that define a MHW or MCS are
#'   met.}
#'   \item{event_no}{A sequential number indicating the ID and order of
#'   occurence of the MHWs or MCSs.}
#'
#' The events are summarised using a range of event metrics:
#'   \item{index_start}{Start index of event.}
#'   \item{index_stop}{Stop index of event.}
#'   \item{event_no}{A sequential number indicating the ID and order of
#'   the events.}
#'   \item{duration}{Duration of event [days].}
#'   \item{date_start}{Start date of event [date].}
#'   \item{date_stop}{Stop date of event [date].}
#'   \item{date_peak}{Date of event peak [date].}
#'   \item{int_mean}{Mean intensity [deg. C].}
#'   \item{int_max}{Maximum (peak) intensity [deg. C].}
#'   \item{int_var}{Intensity variability (standard deviation) [deg. C].}
#'   \item{int_cum}{Cumulative intensity [deg. C x days].}
#'   \item{rate_onset}{Onset rate of event [deg. C / day].}
#'   \item{rate_decline}{Decline rate of event [deg. C / day].}
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
#' @author Albertus J. Smit, Robert W. Schlegel, Eric C. J. Oliver
#'
#' @references Hobday, A.J. et al. (2016). A hierarchical approach to defining
#' marine heatwaves, Progress in Oceanography, 141, pp. 227-238,
#' doi:10.1016/j.pocean.2015.12.014
#'
#' Schlegel, R. W., Oliver, C. J., Wernberg, T. W., Smit, A. J. (submitted).
#' Coastal and offshore co-occurrences of marine heatwaves and cold-spells.
#' Progress in Oceanography.
#'
#' @export
#'
#' @examples
#' t_dat <- make_whole(sst_WA)
#' res <- detect(t_dat, climatology_start = 1983, climatology_end = 2012)
#' # show a portion of the climatology:
#' res$clim[1:10, ]
#' # show some of the heat waves:
#' res$event[1:5, 1:10]
detect <-
  function(data,
           climatology_start = 1983,
           climatology_end = 2012,
           pctile = 90,
           window_half_width = 5,
           smooth_percentile = TRUE,
           smooth_percentile_width = 31,
           min_duration = 5,
           join_across_gaps = TRUE,
           max_gap = 2,
           max_pad_length = 3,
           cold_spells = FALSE) {

    t_series <- data
    t_series$temp <- zoo::na.approx(t_series$temp, maxgap = max_pad_length)

    if (missing(climatology_start))
      stop("Oops! Please provide a complete year for the start of the climatology.")

    if (missing(climatology_end))
      stop("Bummer! Please provide a complete year for the end of the climatology.")

    clim_start <- paste(climatology_start, "01", "01", sep = "-")
    if (t_series$date[1] > clim_start) {
      stop(paste("The specified start date precedes the first day of series, which is", t_series$date[1]))
    }

    clim_end <- paste(climatology_end, "12", "31", sep = "-")
    if (clim_end > t_series$date[nrow(t_series)])
      stop(paste("The specified end date follows the last day of series, which is", t_series$date[nrow(t_series)]))

    if (cold_spells)
      t_series$temp <- -t_series$temp

    tDat <- t_series %>%
      dplyr::filter(date >= clim_start & date <= clim_end) %>%
      dplyr::mutate(date = lubridate::year(date)) %>%
      tidyr::spread(date, temp)

    all_NA <- apply(tDat[59:61, ], 2, function(x) !all(is.na(x)))
    no_NA <- names(all_NA[all_NA > 0]) # compatibility with zoo < 1.7.13...
    tDat[59:61, no_NA] <- zoo::na.approx(tDat[59:61, no_NA], maxgap = 1, na.rm = TRUE)
    tDat <- rbind(utils::tail(tDat, window_half_width),
                  tDat, utils::head(tDat, window_half_width))
    seas_clim_year <- thresh_clim_year <- rep(NA, nrow(tDat))

    for (i in (window_half_width + 1):((nrow(tDat) - window_half_width))) {
      seas_clim_year[i] <-
        mean(c(t(tDat[(i - (window_half_width)):(i + window_half_width), 2:ncol(tDat)])), na.rm = TRUE)
      thresh_clim_year[i] <-
        raster::quantile(
          c(t(tDat[(i - (window_half_width)):(i + window_half_width), 2:ncol(tDat)])),
          probs = 0.9,
          type = 7,
          na.rm = TRUE,
          names = FALSE
        )
    }

    len_clim_year <- 366
    clim <-
      data.frame(
        doy = tDat[(window_half_width + 1):((window_half_width) + len_clim_year), 1],
        seas_clim_year = seas_clim_year[(window_half_width + 1):((window_half_width) + len_clim_year)],
        thresh_clim_year = thresh_clim_year[(window_half_width + 1):((window_half_width) + len_clim_year)]
      )

    if (smooth_percentile) {
      clim %<>%
        dplyr::mutate(
          seas_clim_year = raster::movingFun(
            seas_clim_year,
            n = smooth_percentile_width,
            fun = mean,
            type = "around",
            circular = TRUE,
            na.rm = FALSE
          )
        ) %>%
        dplyr::mutate(
          thresh_clim_year = raster::movingFun(
            thresh_clim_year,
            n = smooth_percentile_width,
            fun = mean,
            type = "around",
            circular = TRUE,
            na.rm = FALSE
          )
        )
    }

    t_series %<>% dplyr::inner_join(clim, by = "doy")
    t_series$temp[is.na(t_series$temp)] <- t_series$seas_clim_year[is.na(t_series$temp)]

    t_series$thresh_criterion <- t_series$temp > t_series$thresh_clim_year
    ex1 <- rle(t_series$thresh_criterion)
    ind1 <- rep(seq_along(ex1$lengths), ex1$lengths)
    s1 <- split(zoo::index(t_series$thresh_criterion), ind1)
    proto_events <- s1[ex1$values == TRUE]
    index_stop <- index_start <- NULL ###
    proto_events_rng <-
      lapply(proto_events, function(x)
        data.frame(index_start = min(x), index_stop = max(x)))

    duration <- NULL ###

    protoFunc <- function(proto_data) {
      out <- proto_data %>%
        dplyr::mutate(duration = index_stop - index_start + 1) %>%
        dplyr::filter(duration >= min_duration) %>%
        dplyr::mutate(date_start = t_series[index_start, "date"]) %>%
        dplyr::mutate(date_stop = t_series[index_stop, "date"])
    }

    proto_events <- do.call(rbind, proto_events_rng) %>%
      dplyr::mutate(event_no = cumsum(ex1$values[ex1$values == TRUE])) %>%
      protoFunc()

    t_series$duration_criterion <- rep(FALSE, nrow(t_series))

    for (i in 1:nrow(proto_events)) {
      t_series$duration_criterion[proto_events$index_start[i]:proto_events$index_stop[i]] <-
        rep(TRUE, length = proto_events$duration[i])
    }

    ex2 <- rle(t_series$duration_criterion)
    ind2 <- rep(seq_along(ex2$lengths), ex2$lengths)
    s2 <- split(zoo::index(t_series$thresh_criterion), ind2)
    proto_gaps <- s2[ex2$values == FALSE]
    proto_gaps_rng <-
      lapply(proto_gaps, function(x) data.frame(index_start = min(x), index_stop = max(x)))

    proto_gaps <- do.call(rbind, proto_gaps_rng) %>%
      dplyr::mutate(event_no = c(1:length(ex2$values[ex2$values == FALSE]))) %>%
      dplyr::mutate(duration = index_stop - index_start + 1)

    if (any(proto_gaps$duration >= 1 & proto_gaps$duration <= max_gap)) {
    proto_gaps %<>%
        dplyr::mutate(date_start = t_series[index_start, "date"]) %>%
        dplyr::mutate(date_stop = t_series[index_stop, "date"]) %>%
        dplyr::filter(duration >= 1 & duration <= max_gap)
    } else {
        join_across_gaps <- FALSE
      }

    if (join_across_gaps) {
      t_series$event <- t_series$duration_criterion
      for (i in 1:nrow(proto_gaps)) {
        t_series$event[proto_gaps$index_start[i]:proto_gaps$index_stop[i]] <-
          rep(TRUE, length = proto_gaps$duration[i])
      }
    } else {
      t_series$event <- t_series$duration_criterion
      }

    ex3 <- rle(t_series$event)
    ind3 <- rep(seq_along(ex3$lengths), ex3$lengths)
    s3 <- split(zoo::index(t_series$event), ind3)
    events <- s3[ex3$values == TRUE]
    event_no <- NULL ###
    events_rng <-
      lapply(events, function(x)
        data.frame(index_start = min(x), index_stop = max(x)))

    events <- do.call(rbind, events_rng) %>%
      dplyr::mutate(event_no = cumsum(ex3$values[ex3$values == TRUE])) %>%
      protoFunc()

    t_series$event_no <- rep(NA, nrow(t_series))
    for (i in 1:nrow(events)) {
      t_series$event_no[events$index_start[i]:events$index_stop[i]] <-
        rep(i, length = events$duration[i])
    }

    events_list <- plyr::dlply(events, .(event_no), function(x)
      with(
        t_series,
        data.frame(
          date = c(date[x$index_start:x$index_stop]),
          temp = c(temp[x$index_start:x$index_stop]),
          seas_clim_year = c(seas_clim_year[x$index_start:x$index_stop]),
          thresh_clim_year = c(thresh_clim_year[x$index_start:x$index_stop]),
          mhw_rel_seas = c(temp[x$index_start:x$index_stop]) - c(seas_clim_year[x$index_start:x$index_stop]),
          mhw_rel_thresh = c(temp[x$index_start:x$index_stop]) - c(thresh_clim_year[x$index_start:x$index_stop]),
          rel_thresh_norm = c(temp[x$index_start:x$index_stop]) - c(thresh_clim_year[x$index_start:x$index_stop]) /
            c(thresh_clim_year[x$index_start:x$index_stop]) - c(seas_clim_year[x$index_start:x$index_stop])
        )
      ))

    int_mean <- int_max <- int_cum <- int_mean_rel_thresh <-
      int_max_rel_thresh <- int_cum_rel_thresh <- int_mean_abs <-
      int_max_abs <- int_cum_abs <- int_mean_norm <- int_max_norm <-
      temp <- doy <- NULL ###
    events$date_peak <-
      plyr::ldply(events_list, function(x)
        x$date[x$mhw_rel_seas == max(x$mhw_rel_seas)][1])[, 2]
    events$int_mean <-
      plyr::ldply(events_list, function(x) mean(x$mhw_rel_seas))[, 2]
    events$int_max <-
      plyr::ldply(events_list, function(x) max(x$mhw_rel_seas))[, 2]
    events$int_var <-
      plyr::ldply(events_list, function(x) sqrt(stats::var(x$mhw_rel_seas)))[, 2]
    events$int_cum <-
      plyr::ldply(events_list, function(x) max(cumsum(x$mhw_rel_seas)))[, 2]
    events$int_mean_rel_thresh <-
      plyr::ldply(events_list, function(x) mean(x$mhw_rel_thresh))[, 2]
    events$int_max_rel_thresh <-
      plyr::ldply(events_list, function(x) max(x$mhw_rel_thresh))[, 2]
    events$int_var_rel_thresh <-
      plyr::ldply(events_list, function(x) sqrt(stats::var(x$mhw_rel_thresh)))[, 2]
    events$int_cum_rel_thresh <-
      plyr::ldply(events_list, function(x) max(cumsum(x$mhw_rel_thresh)))[, 2]
    events$int_mean_abs <-
      plyr::ldply(events_list, function(x) mean(x$temp))[, 2]
    events$int_max_abs <-
      plyr::ldply(events_list, function(x) max(x$temp))[, 2]
    events$int_var_abs <-
      plyr::ldply(events_list, function(x) sqrt(stats::var(x$temp)))[, 2]
    events$int_cum_abs <-
      plyr::ldply(events_list, function(x) max(cumsum(x$temp)))[, 2]
    events$int_mean_norm <-
      plyr::ldply(events_list, function(x) mean(x$rel_thresh_norm))[, 2]
    events$int_max_norm <-
      plyr::ldply(events_list, function(x) max(x$rel_thresh_norm))[, 2]

    mhw_rel_seas <- t_series$temp - t_series$seas_clim_year
    A <- mhw_rel_seas[events$index_start]
    B <- t_series$temp[events$index_start - 1]
    C <- t_series$seas_clim_year[events$index_start - 1]
    mhw_rel_seas_start <- 0.5 * (A + B - C)
    start_type <- ifelse(
      events$index_start > 1,
      "case1",
      ifelse(
        events$index_start == 1 &
          difftime(events$date_peak, events$date_start, units = "days") > 0,
        "case2",
        "case3"
      )
    )[1]
    rateOnset <- function(x, type) {
      switch(
        type,
        case1 = (x$int_max - mhw_rel_seas_start) / (as.numeric(
          difftime(x$date_peak, x$date_start, units = "days")
        ) + 0.5),
        case2 = (x$int_max - A) / 1,
        case3 = (x$int_max - A) / as.numeric(difftime(x$date_peak, x$date_start, units = "days"))
      )
    }
    events$rate_onset <- rateOnset(events, start_type)

    D <- mhw_rel_seas[events$index_stop]
    E <- t_series$temp[events$index_stop + 1]
    F <- t_series$seas_clim_year[events$index_stop + 1]
    mhw_rel_seas_end <- 0.5 * (D + E - F)
    stop_type <- ifelse(
      events$index_stop < nrow(t_series),
      "case4",
      ifelse(
        events$index_stop == nrow(t_series) &
          difftime(events$date_peak, t_series[nrow(t_series), "date"], units = "days") < 0,
        "case5",
        "case6"
      )
    )[nrow(events)]

    rateDecline <- function(x, type) {
      switch(
        type,
        case4 = (x$int_max - mhw_rel_seas_end) / (as.numeric(
          difftime(x$date_stop, x$date_peak, units = "days")
        ) + 0.5),
        case5 = (x$int_max - mhw_rel_seas_end) / 1,
        case6 = (x$int_max - mhw_rel_seas_end) / as.numeric(difftime(x$date_stop, x$date_peak, units = "days"))
      )
    }
    events$rate_decline <- rateDecline(events, stop_type)

    if (cold_spells) {
      events <- events %>% dplyr::mutate(
        int_mean = -int_mean,
        int_max = -int_max,
        int_cum = -int_cum,
        int_mean_rel_thresh = -int_mean_rel_thresh,
        int_max_rel_thresh = -int_max_rel_thresh,
        int_cum_rel_thresh = -int_cum_rel_thresh,
        int_mean_abs = -int_mean_abs,
        int_max_abs = -int_max_abs,
        int_cum_abs = -int_cum_abs,
        int_mean_norm = -int_mean_norm,
        int_max_norm = -int_max_norm
      )
      t_series <- t_series %>% dplyr::mutate(
        temp = -temp,
        seas_clim_year = -seas_clim_year,
        thresh_clim_year = -thresh_clim_year
      )
    }

    list(clim = dplyr::group_by(t_series, event_no),
         event = dplyr::group_by(events, event_no))
  }
