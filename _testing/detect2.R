detect2 <-
  function(data,
           climatology_start = 1983,
           climatology_end = 2012,
           pctile = 90,
           window_half_width = 5,
           smooth_percentile = TRUE,
           smooth_percentile_width = 31,
           clim_only = FALSE,
           min_duration = 5,
           join_across_gaps = TRUE,
           max_gap = 2,
           max_pad_length = 3,
           cold_spells = FALSE,
           diff_baseline = FALSE,
           baseline_data = 0) {
    # baseline_data MUST BE SAME STRUCTURE AS data

    t_series <- data
    t_series$temp <-
      zoo::na.approx(t_series$temp, maxgap = max_pad_length)
    if (missing(climatology_start))
      stop("Oops! Please provide a complete year for the start of the climatology.")
    if (missing(climatology_end))
      stop("Bummer! Please provide a complete year for the end of the climatology.")
    clim_start <- paste(climatology_start, "01", "01", sep = "-")
    if (t_series$date[1] > clim_start)
      stop(
        paste(
          "The specified start date precedes the first day of series, which is",
          t_series$date[1]
        )
      )
    clim_end <- paste(climatology_end, "12", "31", sep = "-")
    if (clim_end > t_series$date[nrow(t_series)])
      stop(paste(
        "The specified end date follows the last day of series, which is",
        t_series$date[nrow(t_series)]
      ))
    if (cold_spells)
      t_series$temp <- -t_series$temp

    # Include possibility of getting climatology out of another dataset

    if (diff_baseline) {
      tDat <- baseline_data %>%
        dplyr::filter(date >= clim_start & date <= clim_end) %>%
        dplyr::mutate(date = lubridate::year(date)) %>%
        tidyr::spread(date, temp)
    } else {
      tDat <-
        t_series %>% dplyr::filter(date >= clim_start & date <= clim_end) %>%
        dplyr::mutate(date = lubridate::year(date)) %>%
        tidyr::spread(date, temp)
    }

    all_NA <- apply(tDat[59:61,], 2, function(x)
      !all(is.na(x)))
    no_NA <- names(all_NA[all_NA > 0])
    tDat[59:61, no_NA] <- zoo::na.approx(tDat[59:61, no_NA],
                                         maxgap = 1, na.rm = TRUE)
    tDat <- rbind(utils::tail(tDat, window_half_width),
                  tDat,
                  utils::head(tDat, window_half_width))
    seas_clim_year <- thresh_clim_year <- var_clim_year <- rep(NA, nrow(tDat))

    for (i in (window_half_width + 1):((nrow(tDat) - window_half_width))) {
      seas_clim_year[i] <- mean(c(t(tDat[(i - (window_half_width)):(i + window_half_width),
                                         2:ncol(tDat)])), na.rm = TRUE)

      thresh_clim_year[i] <-
        raster::quantile(c(t(tDat[(i - (window_half_width)):(i + window_half_width), 2:ncol(tDat)])),
                         probs = pctile / 100,
                         type = 7,
                         na.rm = TRUE,
                         names = FALSE
                         )

      var_clim_year[i] <-
        sd(c(t(tDat[(i - (window_half_width)):(i + window_half_width),
                    2:ncol(tDat)])),
           na.rm = TRUE)
    }

    len_clim_year <- 366
    clim <-
      data.frame(
        doy = tDat[(window_half_width + 1):((window_half_width) +
                                              len_clim_year), 1],
        seas_clim_year = seas_clim_year[(window_half_width + 1):((window_half_width) +
                                                                   len_clim_year)],
        thresh_clim_year = thresh_clim_year[(window_half_width + 1):((window_half_width) +
                                                                       len_clim_year)],
        var_clim_year = var_clim_year[(window_half_width + 1):((window_half_width) +
                                                                 len_clim_year)]
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
        ) %>%
        dplyr::mutate(
          var_clim_year = raster::movingFun(
            var_clim_year,
            n = smooth_percentile_width,
            fun = mean,
            type = "around",
            circular = TRUE,
            na.rm = FALSE
          )
        )
    }

    if (clim_only) {
      t_series <- merge(data, clim, by = "doy")
      t_series <- t_series[order(t_series$date),]
      return(t_series)
    }
    else {
      t_series %<>% dplyr::inner_join(clim, by = "doy")
      t_series$temp[is.na(t_series$temp)] <- t_series$seas_clim_year[is.na(t_series$temp)]
      t_series$thresh_criterion <- t_series$temp > t_series$thresh_clim_year
      ex1 <- rle(t_series$thresh_criterion)
      ind1 <- rep(seq_along(ex1$lengths), ex1$lengths)
      s1 <- split(zoo::index(t_series$thresh_criterion), ind1)
      proto_events <- s1[ex1$values == TRUE]
      index_stop <- index_start <- NULL
      proto_events_rng <- lapply(proto_events, function(x)
        data.frame(index_start = min(x),
                   index_stop = max(x)))
      duration <- NULL
      protoFunc <- function(proto_data) {
        out <- proto_data %>% dplyr::mutate(duration = index_stop - index_start + 1) %>%
          dplyr::filter(duration >= min_duration) %>%
          dplyr::mutate(date_start = t_series$date[index_start]) %>%
          dplyr::mutate(date_stop = t_series$date[index_stop])
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
      proto_gaps_rng <- lapply(proto_gaps, function(x)
        data.frame(index_start = min(x), index_stop = max(x)))
      proto_gaps <- do.call(rbind, proto_gaps_rng) %>%
        dplyr::mutate(event_no = c(1:length(ex2$values[ex2$values == FALSE]))) %>%
        dplyr::mutate(duration = index_stop - index_start + 1)

      if (any(proto_gaps$duration >= 1 & proto_gaps$duration <= max_gap)) {
        proto_gaps %<>%
          dplyr::mutate(date_start = t_series$date[index_start]) %>%
          dplyr::mutate(date_stop = t_series$date[index_stop]) %>%
          dplyr::filter(duration >= 1 & duration <= max_gap)
      }

      else {
        join_across_gaps <- FALSE
      }

      if (join_across_gaps) {
        t_series$event <- t_series$duration_criterion
        for (i in 1:nrow(proto_gaps)) {
          t_series$event[proto_gaps$index_start[i]:proto_gaps$index_stop[i]] <-
            rep(TRUE, length = proto_gaps$duration[i])
          }
        }

      else {
        t_series$event <- t_series$duration_criterion
      }

      ex3 <- rle(t_series$event)
      ind3 <- rep(seq_along(ex3$lengths), ex3$lengths)
      s3 <- split(zoo::index(t_series$event), ind3)
      events <- s3[ex3$values == TRUE]
      event_no <- NULL
      events_rng <- lapply(events, function(x)
        data.frame(index_start = min(x), index_stop = max(x)))
      events <- do.call(rbind, events_rng) %>%
        dplyr::mutate(event_no = cumsum(ex3$values[ex3$values == TRUE])) %>%
        protoFunc()
      t_series$event_no <- rep(NA, nrow(t_series))

      for (i in 1:nrow(events)) {
        t_series$event_no[events$index_start[i]:events$index_stop[i]] <-
          rep(i, length = events$duration[i])
        }

      events_list <-
        plyr::dlply(events, c("event_no"), function(x)
          with(
            t_series,
            data.frame(
              date = c(date[x$index_start:x$index_stop]),
              temp = c(temp[x$index_start:x$index_stop]),
              seas_clim_year = c(seas_clim_year[x$index_start:x$index_stop]),
              thresh_clim_year = c(thresh_clim_year[x$index_start:x$index_stop]),
              mhw_rel_seas = c(temp[x$index_start:x$index_stop]) -
                c(seas_clim_year[x$index_start:x$index_stop]),
              mhw_rel_thresh = c(temp[x$index_start:x$index_stop]) -
                c(thresh_clim_year[x$index_start:x$index_stop]),
              rel_thresh_norm = c(temp[x$index_start:x$index_stop]) -
                c(thresh_clim_year[x$index_start:x$index_stop]) /
                c(thresh_clim_year[x$index_start:x$index_stop]) -
                c(seas_clim_year[x$index_start:x$index_stop])
              )
            )
          )

      int_mean <- NULL
        int_max <- NULL
        int_cum <- NULL
        int_mean_rel_thresh <- NULL
        int_max_rel_thresh <- NULL
        int_cum_rel_thresh <- NULL
        int_mean_abs <- NULL
        int_max_abs <- NULL
        int_cum_abs <- NULL
        int_mean_norm <- NULL
        int_max_norm <-  NULL
        temp <- NULL
        doy <-  NULL
        rate_onset <-  NULL
        rate_decline <- NULL

        events$date_peak <- plyr::ldply(events_list, function(x)
          x$date[x$mhw_rel_seas == max(x$mhw_rel_seas)][1])[, 2]
        events$int_mean <- plyr::ldply(events_list, function(x)
          mean(x$mhw_rel_seas))[, 2]
        events$int_max <- plyr::ldply(events_list, function(x)
          max(x$mhw_rel_seas))[, 2]
        events$int_var <- plyr::ldply(events_list, function(x)
          sqrt(stats::var(x$mhw_rel_seas)))[, 2]
        events$int_cum <- plyr::ldply(events_list, function(x)
          max(cumsum(x$mhw_rel_seas)))[, 2]
        events$int_mean_rel_thresh <- plyr::ldply(events_list, function(x)
          mean(x$mhw_rel_thresh))[, 2]
        events$int_max_rel_thresh <- plyr::ldply(events_list, function(x)
          max(x$mhw_rel_thresh))[, 2]
        events$int_var_rel_thresh <- plyr::ldply(events_list, function(x)
          sqrt(stats::var(x$mhw_rel_thresh)))[, 2]
        events$int_cum_rel_thresh <- plyr::ldply(events_list, function(x)
          max(cumsum(x$mhw_rel_thresh)))[, 2]
        events$int_mean_abs <- plyr::ldply(events_list, function(x)
          mean(x$temp))[, 2]
        events$int_max_abs <- plyr::ldply(events_list, function(x)
          max(x$temp))[, 2]
        events$int_var_abs <- plyr::ldply(events_list, function(x)
          sqrt(stats::var(x$temp)))[, 2]
        events$int_cum_abs <- plyr::ldply(events_list, function(x)
          max(cumsum(x$temp)))[, 2]
        events$int_mean_norm <- plyr::ldply(events_list, function(x)
          mean(x$rel_thresh_norm))[, 2]
        events$int_max_norm <- plyr::ldply(events_list, function(x)
          max(x$rel_thresh_norm))[, 2]
        mhw_rel_seas <- t_series$temp - t_series$seas_clim_year
        A <- mhw_rel_seas[events$index_start]
        B <- t_series$temp[events$index_start - 1]
        C <- t_series$seas_clim_year[events$index_start - 1]
        if (length(B) + 1 == length(A)) {
          B <- c(NA, B)
          C <- c(NA, C)
          }
        mhw_rel_seas_start <- 0.5 * (A + B - C)
        events$rate_onset <- ifelse(
          events$index_start > 1,
          (events$int_max - mhw_rel_seas_start) /
            (as.numeric(difftime(events$date_peak,
                                 events$date_start, units = "days")) + 0.5), NA
          )
        D <- mhw_rel_seas[events$index_stop]
        E <- t_series$temp[events$index_stop + 1]
        F <- t_series$seas_clim_year[events$index_stop + 1]
        mhw_rel_seas_end <- 0.5 * (D + E - F)
        events$rate_decline <- ifelse(
          events$index_stop < nrow(t_series),
          (events$int_max - mhw_rel_seas_end) /
            (as.numeric(difftime(events$date_stop,
                                 events$date_peak, units = "days")) + 0.5), NA
          )
        if (cold_spells) {
          events <- events %>%
            dplyr::mutate(
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
              int_max_norm = -int_max_norm,
              rate_onset = -rate_onset,
              rate_decline = -rate_decline
              )
          t_series <- t_series %>% dplyr::mutate(
            temp = -temp,
            seas_clim_year = -seas_clim_year,
            thresh_clim_year = -thresh_clim_year
            )
          }
        list(clim = dplyr::group_by(t_series),
             event = dplyr::group_by(events))
        }
    }
