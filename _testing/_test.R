library(tidyverse)
library(plyr)
library(tidyr)
library(ggplot2)
library(RmarineHeatWaves)

## test setup
sst_Med2 <- sst_Med
colnames(sst_Med2) <- c("datetime", "temperature")
ts_dat <- make_whole(sst_Med2, x = datetime, y = temperature)
data <- detect(ts_dat, x = datetime, y = temperature, climatology_start = 1983, climatology_end = 2012)
# min_duration <- 5
# spread <- 150
# metric <- "int_cum"
# start_date <- "2010-10-01"
# end_date <- "2011-08-30"
# quo_x <- rlang::quo(datetime)
# quo_y <- rlang::quo(temperature)

## tested function

block_average <-
  function(data,
           x = t,
           y = temp,
           report = "full") {

    quo_x <- rlang::enquo(x)
    quo_y <- rlang::enquo(y)

    clim <- data$clim %>%
      dplyr::rename(t = !!quo_x,
                    temp = !!quo_y)

    year <- temp <- date_start <- temp_mean <- temp_min <- temp_max <- NULL ###
    temp_yr <- clim %>%
      dplyr::group_by(year = lubridate::year(t)) %>%
      dplyr::summarise(temp_mean = mean(temp, na.rm = TRUE),
                       temp_min = min(temp),
                       temp_max = max(temp))

    duration <- count <- int_mean <- int_max <- int_var <- int_cum <-
      int_mean_rel_thresh <- int_max_rel_thresh <- int_var_rel_thresh <-
      int_cum_rel_thresh <- int_mean_abs <- int_max_abs <- int_var_abs <-
      int_cum_abs <- int_mean_norm <- int_max_norm <- rate_onset <-
      rate_decline <- total_days <- total_icum <- NULL ###
    event_block <- data$event %>%
      dplyr::group_by(year = lubridate::year(date_start)) %>%
      dplyr::summarise(count = length(duration),
                       int_mean = mean(int_mean),
                       int_max = mean(int_max),
                       int_var = mean(int_var),
                       int_cum = mean(int_cum),
                       int_mean_rel_thresh = mean(int_mean_rel_thresh),
                       int_max_rel_thresh = mean(int_max_rel_thresh),
                       int_var_rel_thresh = mean(int_var_rel_thresh),
                       int_cum_rel_thresh = mean(int_cum_rel_thresh),
                       int_mean_abs = mean(int_mean_abs),
                       int_max_abs = mean(int_max_abs),
                       int_var_abs = mean(int_var_abs),
                       int_cum_abs = mean(int_cum_abs),
                       int_mean_norm = mean(int_mean_norm),
                       int_max_norm = mean(int_max_norm),
                       rate_onset = mean(rate_onset),
                       rate_decline = mean(rate_decline),
                       total_days = sum(duration),
                       total_icum = sum(int_cum))

    if (report == "full") {
      event_block <- dplyr::left_join(temp_yr, event_block, by = "year")
    } else if (report == "partial") {
      event_block <-
        dplyr::inner_join(temp_yr, event_block, by = "year")
    } else stop("Oops, 'report' must be either 'full' or 'partial'!")

    event_block$count[is.na(event_block$count)] <- 0

    return(event_block)
  }

