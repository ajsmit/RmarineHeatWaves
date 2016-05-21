#' Detect Marine Heat Waves and Marine Cold Spells.
#'
#'Applies the Hobday et al. (2016) marine heat wave definition to an input time
#'series of temperature along with a time vector.
#'
#' @importFrom dplyr %>%
#' @importFrom plyr .
#' @param data A data frame with three columns. They are headed \code{doy},
#' which is the Julian day running from 1 to 366, but modified so that the
#' day-of-year (doy) vector for non-leap-years runs 1...59 and then
#' 61...366. For leap years the 60th day is February 29. The \code{date} column
#' is a vector of dates of class \code{Date}, while \code{temp} is the
#' temperatures. Data of the appropriate format are created by the function
#' \code{\link{make_whole}}, but your own data can be supplied if they meet the
#' criteria specified by \code{make_whole}.
#' @param climatology_period Period over which the (varying by day-of-year)
#' seasonal cycle and extremes threshold are calculated. The function checks if
#' the years are full years (either 365 or 366 days), and if not, will find the
#' next (and/or previous) available full year. It is recommended that a
#' climatology period of at least 30 years is specified in order to capture
#' decadal thermal periodicities. Default is \code{c(1983, 2012)}.
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
#' is \code{1} day.
#' @param max_pad_length Specifies the maximum length of days over which to
#' interpolate (pad) missing data (specified as \code{NA}) in the input
#' temperature time series; i.e., any consecutive blocks of NAs with length
#' greater than \code{max_pad_length} will be left as \code{NA}. Set as an
#' integer. Default is \code{3} days.
#' @param cold_spells Boolean specifying if the code should detect cold events
#' instead of heat events. Default is \code{FALSE}.
#'
#' @details
#'
#' @return The function will return a list of two components, \code{clim} and
#' \code{mhw}, which are the climatology and MHW events, respectively. The
#' climatology contains the full time series of daily temperatures, as well as
#' the the seasonal climatology, the threshold and various aspects of the
#' events that were detected. The events are summarised using a range of event
#' metrics:
#'   \item{index_start}{Start index of MHW.}
#'   \item{index_stop}{Stop index of MHW}
#'   \item{event_no}{A sequential ID number indicating the identity and order of
#'   the events.}
#'   \item{duration}{Duration of MHW [days].}
#'   \item{date_start}{Start date of MHW [date].}
#'   \item{date_stop}{Stop date of MHW [date].}
#'   \item{date_peak}{Date of MHW peak [date].}
#'   \item{int_mean}{Mean intensity [deg. C].}
#'   \item{int_max}{Maximum (peak) intensity [deg. C].}
#'   \item{int_var}{Intensity variability (standard deviation) [deg. C].}
#'   \item{int_cum}{Cumulative intensity [deg. C x days].}
#'   \item{rate_onset}{Onset rate of MHW [deg. C / days].}
#'   \item{rate_decline}{Decline rate of MHW [deg. C / days].}
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
#' indicates the MHW intensity (relative to the climatology) was 1.5 times the
#' value of the threshold (relative to climatology,
#' i.e., threshold - climatology.)
#'
#' @author Albertus J. Smit, Robert W. Schlegel, Eric C. J. Oliver
#'
#' @references Hobday, A.J. et al. (2016), A hierarchical approach to defining
#' marine heatwaves, Progress in Oceanography, 141, pp. 227-238,
#' doi: 10.1016/j.pocean.2015.12.014
#'
#' Schlegel, R. W., Oliver, C. J., Wernberg, T. W., Smit, A. J. (submitted)
#' Coastal and offshore co-occurrences of marine heatwaves and cold-spells.
#' Progress in Oceanography.
#'
#' @export
#'
#' @examples
#' t_dat <- make_whole(sst_WA)
#' res <- detect(t_dat, climatology_period = c(1983, 2012))
#' # show a portion of the climatology:
#' head(res$clim)
#' # show some of the heat waves:
#' res$mhw[1:5, 1:10]
detect <-
  function(data,
           climatology_period = c(1983, 2012),
           pctile = 90,
           window_half_width = 5,
           smooth_percentile = TRUE,
           smooth_percentile_width = 31,
           min_duration = 5,
           join_across_gaps = TRUE,
           max_gap = 2,
           max_pad_length = 3,
           cold_spells = FALSE) {
    #
    # Sort out the dates.
    #===========================================================================
    # TODO: test if specified years are full years...
    # TODO: what happens when climatology_period is given but dates outside of
    # the time series are provided? It should fail with the appropriate error
    # message retruned - possibly something helpful such as start and end dates.
    clim_start <- paste(climatology_period[1], "01", "01", sep = "-")
    clim_end <- paste(climatology_period[2], "12", "31", sep = "-")

    t_series <- data

    # Handles missing days.
    t_series$temp <-
      zoo::na.approx(t_series$temp, maxgap = max_pad_length)

    # Flip temperature in time series if detecting cold spells.
    if (cold_spells)
      t_series <- -1 * t_series

    #
    # Calculate start and end dates if not provided.
    #===========================================================================
    # Start and end indices: find first day of the first full year and the last
    # day of the last full year for climatological period.
    len_clim_year <- 366
    # Check to see if clim_start and clim_end were provided; if not, calculate
    # them based on the first and last full year available.
    if (exists("clim_start") & exists("clim_end")) {
      clim_start <- clim_start
      clim_end <- clim_end
    } else {
      t1 <-
        head(plyr::ddply(
          data.frame(yr = lubridate::year(t_series$date)),
          .(lubridate::year(t_series$date)),
          nrow
        ), 2)
      t2 <-
        tail(plyr::ddply(
          data.frame(yr = lubridate::year(t_series$date)),
          .(lubridate::year(t_series$date)),
          nrow
        ), 2)
      clim_start <-
        ifelse((
          lubridate::leap_year(t1[1, 1]) && max(t1[1, 2]) == len_clim_year ||
            !lubridate::leap_year(t1[1, 1]) &&
            max(t1[1, 2]) == 365
        ),
        t1[1, 1],
        t1[2, 1]
        )
      clim_start <-
        head(dplyr::filter(t_series, lubridate::year(date) == clim_start), 1)[, "date"]
      clim_end <-
        ifelse((
          lubridate::leap_year(t2[2, 1]) && max(t2[2, 2]) == len_clim_year ||
            !lubridate::leap_year(t2[2, 1]) &&
            max(t2[2, 2]) == 365
        ),
        t2[2, 1],
        t2[1, 1]
        )
      clim_end <-
        tail(dplyr::filter(t_series, lubridate::year(date) == clim_end), 1)[, "date"]
    }

    # Calculate threshold and seasonal climatology (varying with day-of-year).
    #===========================================================================
    # Use a 366-day year.
    tDat <- t_series %>%
      dplyr::filter(date >= clim_start & date <= clim_end) %>%
      reshape2::dcast(doy ~ lubridate::year(date), value.var = "temp") %>%
      as.data.frame()

    # For non-leap years, replace feb29 (doy = 60) with a value interpolated
    # across Feb 28 and Mar 1, ONLY IF the latter days exist.
    tDat[59:61,] <- zoo::na.approx(tDat[59:61,], maxgap = 1)
    tDat <-
      rbind(tail(tDat, window_half_width),
            tDat,
            head(tDat, window_half_width)) # allows 'wrap-around' window
    seas_clim_year <- thresh_clim_year <- rep(NA, nrow(tDat))
    for (i in (window_half_width + 1):((nrow(tDat) - window_half_width))) {
      seas_clim_year[i] <-
        mean(c(t(tDat[(i - (window_half_width)):(i + window_half_width), 2:ncol(tDat)])), na.rm = TRUE)
      thresh_clim_year[i] <-
        quantile(
          c(t(tDat[(i - (window_half_width)):(i + window_half_width), 2:ncol(tDat)])),
          probs = 0.9,
          type = 7,
          na.rm = TRUE,
          names = FALSE
        )
    }

    clim <-
      data.frame(
        doy = tDat[(window_half_width + 1):((window_half_width) + len_clim_year), 1],
        seas_clim_year = seas_clim_year[(window_half_width + 1):((window_half_width) + len_clim_year)],
        thresh_clim_year = thresh_clim_year[(window_half_width + 1):((window_half_width) + len_clim_year)]
      )
    if (smooth_percentile) {
      clim <- clim %>%
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

    # Generate climatology and threshold for full time series.
    t_series <- dplyr::inner_join(t_series, clim, by = "doy")
    # Set all remaining missing temp values equal to the climatology.
    t_series$temp[is.na(t_series$temp)] <-
      t_series$seas_clim_year[is.na(t_series$temp)]

    #
    # Detect events
    #===========================================================================
    # Find 'proto-events' as exceedances above the threshold and find gaps
    # inbetween these proto-events. Proto-events are regions of the time series
    # where temperatures exceed the threshold, but where criteria pertaining to
    # the event duration (min_duration) and gaps between regions of exceedances
    # (max_gap) are not yet met.
    t_series$thresh_criterion <-
      t_series$temp > t_series$thresh_clim_year
    # Find lengths of TRUE (stretches where temperature > threshold)
    # and FALSE (otherwise).
    ex1 <- rle(t_series$thresh_criterion)
    ind1 <- rep(seq_along(ex1$lengths), ex1$lengths)
    s1 <- split(zoo::index(t_series$thresh_criterion), ind1)
    # Find contiguous regions of thresh_criterion = TRUE.
    proto_events <- s1[ex1$values == TRUE]
    proto_events_rng <-
      lapply(proto_events, function(x)
        data.frame(index_start = min(x), index_stop = max(x)))

    # Set mode = TRUE for detecting 'proto-events' (i.e. tresholds exceeded but
    # not yet meeting the min_duration criterion), and mode = FALSE for
    # detecting the gaps between protoevents.
    protoFunc <- function(data, mode = TRUE)  {
      out <- dplyr::mutate(data, duration = index_stop - index_start + 1)
      if (mode)
        out <- dplyr::filter(out, duration >= min_duration)
      else
        out <-
          dplyr::filter(out, duration >= 1 & duration <= max_gap)
      out <-
        dplyr::mutate(out, date_start = t_series[index_start, "date"])
      out <-
        dplyr::mutate(out, date_stop = t_series[index_stop, "date"])
      return(out)
    }

    # Find proto-events that are >= min_duration.
    proto_events <- do.call(rbind, proto_events_rng) %>%
      dplyr::mutate(event_no = cumsum(ex1$values[ex1$values == TRUE])) %>%
      protoFunc(mode = TRUE)
    t_series$duration_criterion <- rep(FALSE, nrow(t_series))
    for (i in 1:nrow(proto_events)) {
      t_series$duration_criterion[proto_events$index_start[i]:proto_events$index_stop[i]] <-
        rep(TRUE, length = proto_events$duration[i])
    }

    # Find gaps between proto-events that are <= minGap.
    ex2 <- rle(t_series$duration_criterion)
    ind2 <- rep(seq_along(ex2$lengths), ex2$lengths)
    s2 <- split(zoo::index(t_series$thresh_criterion), ind2)

    # Find contiguous regions of proto-events = FALSE.
    proto_gaps <- s2[ex2$values == FALSE]
    proto_gaps_rng <-
      lapply(proto_gaps, function(x)
        data.frame(index_start = min(x), index_stop = max(x)))
    proto_gaps <- do.call(rbind, proto_gaps_rng) %>%
      dplyr::mutate(event_no = seq(1:length(ex2$values[ex2$values == FALSE]))) %>%
      protoFunc(mode = FALSE)

    # Link proto-events that occur before and after gaps <= max_gap.
    if (join_across_gaps) {
      t_series$event <- t_series$duration_criterion
      for (i in 1:nrow(proto_gaps)) {
        t_series$event[proto_gaps$index_start[i]:proto_gaps$index_stop[i]] <-
          rep(TRUE, length = proto_gaps$duration[i])
      }
    } else {
      print("STOP! The option to not join across gaps is not yet implemented.")
    }

    # Summarise events (i.e. where temperature thresholds are exceeded for
    # >= min_duration, and without gaps that are <= max_gaps).
    ex3 <- rle(t_series$event)
    ind3 <- rep(seq_along(ex3$lengths), ex3$lengths)
    s3 <- split(zoo::index(t_series$event), ind3)
    events <- s3[ex3$values == TRUE]
    events_rng <-
      lapply(events, function(x)
        data.frame(index_start = min(x), index_stop = max(x)))
    events <- do.call(rbind, events_rng) %>%
      dplyr::mutate(event_no = cumsum(ex3$values[ex3$values == TRUE])) %>%
      protoFunc(mode = TRUE)

    t_series$event_no <- rep(NA, nrow(t_series))
    for (i in 1:nrow(events)) {
      t_series$event_no[events$index_start[i]:events$index_stop[i]] <-
        rep(i, length = events$duration[i])
    }

    #
    # Calculate the event metrics.
    #===========================================================================
    # Make a list of time series of temperatures, seasonal climatologies and
    # thresholds for each event from which to calculate the metrics.
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

    # intensity metrics
    ### index_peak
    events$date_peak <-
      plyr::ldply(events_list, function(x)
        x$date[x$mhw_rel_seas == max(x$mhw_rel_seas)][1])[, 2]
    events$int_mean <-
      plyr::ldply(events_list, function(x)
        mean(x$mhw_rel_seas))[, 2]
    events$int_max <-
      plyr::ldply(events_list, function(x)
        max(x$mhw_rel_seas))[, 2]
    events$int_var <-
      plyr::ldply(events_list, function(x)
        sqrt(var(x$mhw_rel_seas)))[, 2]
    events$int_cum <-
      plyr::ldply(events_list, function(x)
        max(cumsum(x$mhw_rel_seas)))[, 2]
    events$int_mean_rel_thresh <-
      plyr::ldply(events_list, function(x)
        mean(x$mhw_rel_thresh))[, 2]
    events$int_max_rel_thresh <-
      plyr::ldply(events_list, function(x)
        max(x$mhw_rel_thresh))[, 2]
    events$int_var_rel_thresh <-
      plyr::ldply(events_list, function(x)
        sqrt(var(x$mhw_rel_thresh)))[, 2]
    events$int_cum_rel_thresh <-
      plyr::ldply(events_list, function(x)
        max(cumsum(x$mhw_rel_thresh)))[, 2]
    events$int_mean_abs <-
      plyr::ldply(events_list, function(x)
        mean(x$temp))[, 2]
    events$int_max_abs <-
      plyr::ldply(events_list, function(x)
        max(x$temp))[, 2]
    events$int_var_abs <-
      plyr::ldply(events_list, function(x)
        sqrt(var(x$temp)))[, 2]
    events$int_cum_abs <-
      plyr::ldply(events_list, function(x)
        max(cumsum(x$temp)))[, 2]
    events$int_mean_norm <-
      plyr::ldply(events_list, function(x)
        mean(x$rel_thresh_norm))[, 2]
    events$int_max_norm <-
      plyr::ldply(events_list, function(x)
        max(x$rel_thresh_norm))[, 2]

    # rate metrics - requires getting MHW strength at "start" and "end" of
    # event (continuous: assume start / end half-day before / after first /
    # last point)
    # rate of onset
    mhw_rel_seas <- t_series$temp - t_series$seas_clim_year

    A <- mhw_rel_seas[events$index_start]
    B <- t_series$temp[events$index_start - 1]
    C <- t_series$seas_clim_year[events$index_start - 1]
    mhw_rel_seas_start <- 0.5 * (A + B - C)
    startType <- ifelse(
      events$index_start > 1,
      "case1",
      # event not at beginning of time series
      ifelse(
        events$index_start == 1 &
          difftime(events$date_peak, events$date_start, units = "days") > 0,
        "case2",
        # event starts at beginning of time series
        "case3" # peak also at begn. of time series, assume onset time = 1 day
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
    events$rate_onset <- rateOnset(events, startType)

    # rate of decline
    D <- mhw_rel_seas[events$index_stop]
    E <- t_series$temp[events$index_stop + 1]
    F <- t_series$seas_clim_year[events$index_stop + 1]
    mhw_rel_seas_end <- 0.5 * (D + E - F)
    stopType <- ifelse(
      events$index_stop < nrow(t_series),
      "case4",
      # event does not finish at end of time series
      ifelse(
        events$index_stop == nrow(t_series) &
          difftime(events$date_peak, t_series[nrow(t_series), "date"], units = "days") < 0,
        "case5",
        # event finishes at end of time series
        "case6" # peak also at begn of time series, assume onset time = 1 day
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
    events$rate_decline <- rateDecline(events, stopType)

    list(clim = t_series,
         mhw = events)
  }