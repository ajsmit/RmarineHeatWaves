library(tidyverse)
library(plyr)
library(tidyr)
library(ggplot2)
library(RmarineHeatWaves)

dt1 <- sst_WA
colnames(dt1) <- c("date", "temperature")

ts_dat <- make_whole(dt1, x = date, y = temperature)
res <- detect(ts_dat, x = date, y = temperature, climatology_start = 1983, climatology_end = 2012)

event_line(res, x = date, y = temperature, spread = 200, metric = "int_cum",
           start_date = "2010-10-01", end_date = "2011-08-30")




data <- res
min_duration <- 5
spread <- 150
metric <- "int_cum"
start_date <- "2010-10-01"
end_date <- "2011-08-30"
quo_x <- rlang::quo(t)
quo_y <- rlang::quo(temp)


event_line <- function(data,
                       x = t,
                       y = temp,
                       min_duration = 5,
                       spread = 150,
                       metric = "int_cum",
                       start_date,
                       end_date) {

  temp <- date_stop <- date_start <- int_max <- int_mean <- int_cum <- duration <- NULL

  event <- data$event %>%
    dplyr::filter(date_stop >= start_date & date_start <= end_date)
  if (nrow(event) == 0) stop("No events detected!\nConsider changing the 'start_date' or 'end_date' values.")
  event <- event[order(-abs(event[colnames(event) == metric])),]
  event_top <- event[1, ]

  date_spread <- seq((event_top$date_start - spread), (event_top$date_stop + spread), by = 1)

  # quo_x <- rlang::enquo(x)
  # quo_y <- rlang::enquo(y)

  clim <- data$clim %>%
    dplyr::rename(t = !! quo_x,
                  temp = !! quo_y) %>%
    dplyr::filter(t %in% date_spread)

  event_no <- thresh_clim_year <- seas_clim_year <- NULL

  # clim.ev <- clim %>%
  #   filter(complete.cases(.)) %>%
  #   complete(t = full_seq(t, 1L), fill = list(value = 0))

  for (i in min(clim$event_no, na.rm = TRUE):max(clim$event_no, na.rm = TRUE)) {
    x <- clim[stats::complete.cases(clim$event_no) & clim$event_no == i,]
    grid.df <-
      data.frame(t = seq(x$t[1], x$t[nrow(x)], by = "day"))
    x <- merge(x, grid.df, by = "t", all.y = TRUE)

    if (nrow(x[x$thresh_criterion != FALSE,]) != nrow(x)) {
      ex1 <- rle(x$thresh_criterion)
      ind1 <- rep(seq_along(ex1$lengths), ex1$lengths)
      s1 <- split(zoo::index(x$thresh_criterion), ind1)
      proto_events <- s1[ex1$values == TRUE]
      index_stop <- index_start <- NULL ###
      proto_events_rng <-
        lapply(proto_events, function(x)
          data.frame(index_start = min(x), index_stop = max(x)))
      duration <- NULL ###
      # min_duration <- NULL ###
      protoFunc <- function(proto_data) {
        out <- proto_data %>%
          dplyr::mutate(duration = index_stop - index_start + 1) %>%
          dplyr::filter(duration >= min_duration) %>%
          dplyr::mutate(date_start = x[index_start, "t"]) %>%
          dplyr::mutate(date_stop = x[index_stop, "t"])
      }
      proto_events <- do.call(rbind, proto_events_rng) %>%
        dplyr::mutate(event_no = cumsum(ex1$values[ex1$values == TRUE])) %>%
        protoFunc()
      sub.event <- function(proto_event) {
        df <-  x[proto_event$index_start:proto_event$index_stop,]
        df$event_no_sub <- paste(df$event_no, proto_event$event_no, sep = ".")
        return(df)
      }
      x <- plyr::ddply(proto_events, .(index_start), sub.event)
      x$event_no_sub <- as.character(x$event_no_sub)
    } else {
      event_no_sub <- NULL
      x$event_no_sub <- x$event_no
    }

    mirror <- function(x) {
      event_no_sub <- NULL
      y <- data.frame(
        temp = x$temp,
        t = x$t,
        event_no = x$event_no,
        event_no_sub = x$event_no_sub
      )
      z <-
        rbind(y, data.frame(
          temp = rev(x$thresh_clim_year),
          t = rev(x$t),
          event_no = x$event_no,
          event_no_sub = x$event_no_sub
        ))
      z$order <- rep(c(1, 2), each = nrow(x))
      return(z)
    }

    z <- plyr::ddply(x, .(event_no_sub), mirror)
    z$event_no_sub <- as.character(z$event_no_sub)
  }

  lineCol <- c(
    "temperature" = "black",
    "climatology" = "blue",
    "threshold" = "darkgreen"
  )

  if (event_top$int_mean > 0) {
    fillCol <- c("events" = "salmon", "peak event" = "red")
  } else {
    fillCol <- c("events" = "steelblue3", "peak event" = "navy")
  }

  # yaxis = "int_max" yaxis = "int_mean" yaxis = "int_cum" yaxis = "duration"
  if (metric == "int_max") ylabel <- expression(paste("Maximum intensity [", degree, "C]"))
  if (metric == "int_mean") ylabel <- expression(paste("Mean intensity [", degree, "C]"))
  if (metric == "int_cum") ylabel <- expression(paste("Cumulative intensity [", degree, "C x days]"))
  if (metric == "duration") ylabel <- "Duration [days]"
  if (!exists("ylabel")) ylabel <- metric

  ggplot(data = clim, aes(x = t, y = temp)) +
    geom_polygon(data = z,
                 aes(x = t, y = temp, group = event_no_sub, fill = "events"), size = 0.5) +
    geom_polygon(data = z[z$event_no == event_top$event_no[1],],
                 aes(x = t, y = temp, group = event_no_sub, fill = "peak event"),
                 size = 0.5) +
    geom_line(aes(y = seas_clim_year, col = "climatology"),
              size = 0.7, alpha = 1) +
    geom_line(aes(y = thresh_clim_year, col = "threshold"),
              size = 0.7, alpha = 1) +
    geom_line(aes(y = temp, col = "temperature"), size = 0.6) +
    scale_colour_manual(name = NULL, values = lineCol) +
    scale_fill_manual(name = NULL, values = fillCol, guide = FALSE) +
    scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
    ylab(ylabel) + xlab("Date") +
    theme(plot.background = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.75),
          panel.grid.minor = element_line(colour = NA),
          panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dotted"),
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
          axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
          axis.ticks.length = unit(-0.25, "cm"),
          legend.background = element_rect(colour = "black"),
          legend.direction = "horizontal",
          legend.justification = c(0, 0),
          legend.position = c(0.005, 0.015),
          legend.key = element_blank()
    )
}
