#' Create a Line Plot of Marine Heat Waves or Cold Spells.
#'
#' Creates a graph of warm or cold events as per the second row of Figure 3 in
#' Hobday et al. (2016).
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_polygon
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_grey
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom grid unit
#'
#' @param data The function receives the output from the \code{\link{detect}} function.
#' @param spread The the number of days leading and trailing the largest event
#' (as per \code{metric}) detected within the time period specified by
#' \code{start_date} and \code{end_date}. The default is 150 days.
#' @param metric One of the following options: \code{mean}, \code{maximum}, \code{cumulative} or
#' \code{duration}. These refer to the mean intensity [deg. C], maximum (peak)
#' intensity [deg. C], cumulative intensity [deg. C x days] or duration of the MHW
#' or MCS [days], respectively. The default is \code{cum_sum}.
#' @param start_date The start date of a period of time within which the largest
#' event (as per \code{metric}) is retrieved and plotted. This may not necessarily
#' correspond to the biggest event of the specified metric within the entire
#' data set. To plot the biggest event within the whole time series, make sure
#' \code{start_date} and \code{end_date} straddle this event, or simply specify
#' the start and end dates of the full time series given to \code{\link{detect}}.
#' @param end_date The end date of a period of time within which the largest
#' event (as per \code{metric}) is retrieved and plotted. See \code{start_date}
#' for additional information.
#' @param file_name The name of the file to be saved. By default a file named
#' 'eventPlot.pdf' will be saved in the working directory. Specify the path
#' and file name to change.
#'
#' @return The function will return a line plot indicating the climatology,
#' threshold and temperature, with the hot or cold events that meet the
#' specifications of Hobday et al. (2016) shaded in as appropriate. The plotting
#' of hot or cold events depends on which option is specified in \code{\link{detect}}. The graph
#' will be saved to disk.
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
#' res <- detect(t_dat) # using default values
#' \dontrun{
#' event_line(res, spread = 200, metric = "int_cum",
#' start_date = "2010-10-01", end_date = "2011-08-30",
#' file_name = "WA_event.pdf")
#' }
event_line <- function(data,
                      spread = 150,
                      metric = "cumulative",
                      start_date = "1999-06-30",
                      end_date = "2000-05-30",
                      file_name = "eventPlot.pdf"){
# The start and end dates are intentionally switched to allow for events that
# end or begin within the designated time to be scanned for the size of their metrics.
  date_stop <- date_start <- int_max <- int_mean <- int_cum <- duration <- NULL # avoids annoying notes during check...
  event <- dplyr::filter(data$event, date_stop >= start_date & date_start <= end_date)
  if(nrow(event) == 0) stop("No events detected!")
  if(metric == "maximum"){
    event <- dplyr::arrange(event, abs(int_max))
  } else if(metric == "mean"){
    event <- dplyr::arrange(event, abs(int_mean))
  } else if(metric == "cumulative"){
    event <- dplyr::arrange(event, abs(int_cum))
  } else if(metric == "duration"){
    event <- dplyr::arrange(event, abs(duration))
  }
# TODO: Must insert a bit of logic here if there is a tie for largest value
  eventTop <- event[nrow(event), ]

# Create index of dates by which to plot as determined by eventTop
  date_spread <- seq((eventTop$date_start - spread), (eventTop$date_stop + spread), by = 1)

# Subset only the given range of dates as discerned by the "spread" variable
  clim <- dplyr::filter(data$clim, date %in% date_spread)

# Create closed pathways for each event so they plot correctly.
#
# TODO: Rob, this bit here which results in 'dat3' does not work for MCSs
# detected in the sst_NW_Atl data set...

  temp <- event_no <- thresh_clim_year <- seas_clim_year <- NULL # avoids annoying notes during check...
  dat3 <- data.frame()
  for (i in min(clim$event_no, na.rm = TRUE):max(clim$event_no, na.rm = TRUE)) {
    x <- clim[complete.cases(clim$event_no) & clim$event_no == i,]
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

# Plot and save
  lineCol <- c(
    "temperature" = "black",
    "climatology" = "blue",
    "threshold" = "darkgreen"
  )

  if(eventTop$int_mean > 0){
    fillCol <- c("events" = "salmon", "peak event" = "red")
  } else {
    fillCol <- c("events" = "steelblue3", "peak event" = "royalblue4")
  }

  ggplot(data = clim, aes(x = date, y = temp)) +
    geom_polygon(data = dat3,
                 aes(x = date, y = temp, group = event_no, fill = "events"), size = 0.5) +
    geom_polygon(data = dat3[dat3$event_no == eventTop$event_no[1],],
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
    # put theme here for reasons to do with building the package... will fix later
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
  ggsave(file_name, width = 8.0, height = 3.0, pointsize = 16)
}
