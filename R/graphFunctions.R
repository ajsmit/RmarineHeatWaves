#' Create a Line Plot of Marine Heat Waves or Cold Spells.
#'
#' Creates a graph of warm or cold events as per the second row of Figure 3 in
#' Hobday et al. (2016).
#'
#' @importFrom ggplot2 ggplot aes geom_polygon geom_line scale_colour_manual
#' scale_fill_manual scale_x_date xlab ylab theme theme_grey element_text
#' element_blank
#' @importFrom grid unit
#'
#' @param data The function receives the output from the \code{\link{detect}} function.
#' @param spread The the number of days leading and trailing the largest event
#' (as per \code{metric}) detected within the time period specified by
#' \code{start_date} and \code{end_date}. The default is 150 days.
#' @param metric One of the following options: \code{int_mean}, \code{int_max}, \code{int_var},
#' \code{int_cum}, \code{int_mean_rel_thresh}, \code{int_max_rel_thresh}, \code{int_var_rel_thresh},
#' \code{int_cum_rel_thresh}, \code{int_mean_abs}, \code{int_max_abs}, \code{int_var_abs},
#' \code{int_cum_abs}, \code{int_mean_norm}, \code{int_max_norm}, \code{rate_onset}, \code{rate_decline}.
#' Partial name matching is currently not supported so please specify the matric
#' name precisely. The default is \code{int_cum}.
#' @param start_date The start date of a period of time within which the largest
#' event (as per \code{metric}) is retrieved and plotted. This may not necessarily
#' correspond to the biggest event of the specified metric within the entire
#' data set. To plot the biggest event within the whole time series, make sure
#' \code{start_date} and \code{end_date} straddle this event, or simply specify
#' the start and end dates of the full time series given to \code{\link{detect}}.
#' @param end_date The end date of a period of time within which the largest
#' event (as per \code{metric}) is retrieved and plotted. See \code{start_date}
#' for additional information.
#'
#' @return The function will return a line plot indicating the climatology,
#' threshold and temperature, with the hot or cold events that meet the
#' specifications of Hobday et al. (2016) shaded in as appropriate. The plotting
#' of hot or cold events depends on which option is specified in \code{\link{detect}}.
#'
#' @author Robert W. Schlegel
#'
#' @references Hobday, A.J. et al. (2016), A hierarchical approach to defining
#' marine heatwaves, Progress in Oceanography, 141, pp. 227-238,
#' doi: 10.1016/j.pocean.2015.12.014
#'
#' @export
#'
#' @examples
#' t_dat <- make_whole(sst_WA)
#' res <- detect(t_dat, climatology_start = 1983, climatology_end = 2012) # using default values
#'
#' \dontrun{
#' event_line(res, spread = 200, metric = "int_cum",
#' start_date = "2010-10-01", end_date = "2011-08-30",
#' file_name = "WA_event.pdf")
#' }
event_line <- function(data,
                       spread = 150,
                       metric = "int_cum",
                       start_date = "1999-06-30",
                       end_date = "2000-05-30") {
  date_stop <- date_start <- int_max <- int_mean <- int_cum <- duration <- NULL

  event <- data$event %>%
    dplyr::filter(date_stop >= start_date & date_start <= end_date)
  if (nrow(event) == 0) stop("No events detected!")
  event <- event[order(-abs(event[colnames(event) == metric])),]
  event_top <- event[1, ]

  date_spread <- seq((event_top$date_start - spread), (event_top$date_stop + spread), by = 1)

  clim <- dplyr::filter(data$clim, date %in% date_spread)

  temp <- event_no <- thresh_clim_year <- seas_clim_year <- NULL # avoids annoying notes during check...
  dat3 <- data.frame()
  for (i in min(clim$event_no, na.rm = TRUE):max(clim$event_no, na.rm = TRUE)) {
    x <- clim[stats::complete.cases(clim$event_no) & clim$event_no == i,]
    grid.df <-
      data.frame(date = seq(x$date[1], x$date[nrow(x)], by = "day"))
    x <- merge(x, grid.df, by = "date", all.y = TRUE)
    y <- data.frame(
      temp = x$temp,
      date = x$date,
      event_no = x$event_no
    )
    z <-
      rbind(y, data.frame(
        temp = rev(x$thresh_clim_year),
        date = rev(x$date),
        event_no = x$event_no
      ))
    z$order <- rep(c(1, 2), each = nrow(x))
    dat3 <- rbind(dat3, z)
  }

  lineCol <- c(
    "temperature" = "black",
    "climatology" = "blue",
    "threshold" = "darkgreen"
  )

  if (event_top$int_mean > 0) {
    fillCol <- c("events" = "salmon", "peak event" = "red")
  } else {
    fillCol <- c("events" = "steelblue3", "peak event" = "royalblue4")
  }

  ggplot(data = clim, aes(x = date, y = temp)) +
    geom_polygon(data = dat3,
                 aes(x = date, y = temp, group = event_no, fill = "events"), size = 0.5) +
    geom_polygon(data = dat3[dat3$event_no == event_top$event_no[1],],
                 aes(x = date, y = temp, group = event_no, fill = "peak event"),
                 size = 0.5) +
    geom_line(aes(y = seas_clim_year, col = "climatology"),
              size = 0.7, alpha = 1) +
    geom_line(aes(y = thresh_clim_year, col = "threshold"),
              size = 0.7, alpha = 1) +
    geom_line(aes(y = temp, col = "temperature"), size = 0.6) +
    scale_colour_manual(name = NULL, values = lineCol) +
    scale_fill_manual(name = NULL, values = fillCol) +
    scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
    ylab(expression(paste("[", degree, "C]"))) + xlab(NULL) +
    theme(
      axis.text = element_text(colour = "black"),
      legend.position = c(0, 1),
      legend.box = "vertical",
      legend.box.just = "left",
      legend.justification = c(0, 1),
      legend.direction = "horizontal",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      panel.grid.minor = element_blank()
    )
}

#' Create a Timeline of Selected Event Metrics.
#'
#' Visualise a timeline of several event metrics as 'lollipop' graphs.
#'
#' @importFrom magrittr %<>%
#' @importFrom ggplot2 aes_string geom_segment geom_point scale_x_continuous
#'
#' @param data Output from the \code{\link{detect}} function.
#' @param metric One of \code{int_mean}, \code{int_max}, \code{int_cum} and \code{duration}.
#' Default is \code{int_cum}.
#' @param event_count The number of top events to highlight. Default is 3.
#' @param xaxis One of \code{event_no}, \code{date_start} or \code{date_peak}.
#' Default is \code{date_start}.
#'
#' @return The function will return a graph of the intensity of the selected
#' metric along the y-axis versus either \code{date} or \code{event_no}.
#'
#' @author Albertus J. Smit and Robert W. Schlegel
#'
#' @export
#'
#' @examples
#' t_dat <- make_whole(sst_NW_Atl)
#' res <- detect(t_dat, climatology_start = 1983, climatology_end = 2012) # using default values
#'
#' \dontrun{
#' lolli_plot(res, metric = "int_cum", event_count = 3, xaxis = "date_peak")
#' }
lolli_plot <- function(data,
                       metric = "int_max",
                       event_count = 3,
                       xaxis = "date_start") {

  event <- data$event
  if(nrow(event) == 0) stop("No events detected!")

  peak_sort <- NULL
  expr <- lazyeval::interp(~abs(x), x = as.name(metric))
  event %<>%
    dplyr::select_("event_no", "date_start", "date_peak", metric) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_(.dots = stats::setNames(list(expr), "peak_sort")) %>%
    dplyr::arrange(dplyr::desc(peak_sort))

    event$col <- "event"
    event[1:event_count, 6] <- "peak event"

  if(event[1, 4] < 0){
    lolli_col <- c("steelblue3", "navy")
  } else {
    lolli_col <- c("salmon", "firebrick")
  }

  # Create y and x axis labels
  # xaxis = "event_no" xaxis = "date_start" xaxis = "date_peak"
  if(xaxis == "event_no") xlabel <- "Event number"
  if(xaxis == "date_start") xlabel <- "Start date"
  if(xaxis == "date_peak") xlabel <- "Peak date"
  # yaxis = "int_max" yaxis = "int_mean" yaxis = "int_cum" yaxis = "duration"
  if(metric == "int_max") ylabel <- expression(paste("Maximum intensity [", degree, "C]"))
  if(metric == "int_mean") ylabel <- expression(paste("Mean intensity [", degree, "C]"))
  if(metric == "int_cum") ylabel <- expression(paste("Cumulative intensity [", degree, "C x days]"))
  if(metric == "duration") ylabel <- "Duration [days]"
  if(!exists("ylabel")) ylabel <- metric

  # Create the figure
  lolli <- ggplot(data = event, aes_string(x = xaxis, y = metric)) +
    geom_segment(aes_string(xend = xaxis, yend = 0, colour = "col"),
                 size = 0.6, lineend = "butt", show.legend = F) +
    geom_point(aes_string(colour = "col", fill = "col"), shape = 21, size = 2.2) +
    # geom_text(data = event_top, aes_string(label = index, x = xaxis, y = yaxis), size = 2.0) +
    scale_colour_manual(name = NULL, values = lolli_col) +
    scale_fill_manual(name = NULL, values = c("ivory1", "wheat1")) +
    xlab(xlabel) +
    ylab(ylabel) +
    theme(
      axis.text = element_text(colour = "black"),
      legend.position = c(0, 1),
      legend.box = "vertical",
      legend.box.just = "left",
      legend.justification = c(0, 1),
      legend.direction = "horizontal",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      panel.grid.minor = element_blank()
    )
  if(xaxis == "event_no"){
    lolli <- lolli +
      scale_x_continuous(breaks = seq(from = 0, to = nrow(data$event), by = 5))
  }
  if(event[1, 4] < 0 & metric != "duration"){
    lolli <- lolli +
      theme(legend.justification = c(0, 4.85))
  }
  lolli
}