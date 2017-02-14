#' Create a Timeline geom of a Selected Event Metric.
#'
#' Visualise a timeline of several event metrics as 'lollipops'.
#'
#' @import ggplot2
#' @importFrom ggplot2 ggproto
#' @inheritParams ggplot2::layer
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning. If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#' @param n The number of top events to highlight. Default is 1.
#'
#' @return The function will return a graph of the intensity of the selected
#' metric along the y-axis versus a time variable along the x axis.
#' The number of top events (\code{n}) from the chosen metric may be highlighted
#' in a brighter colour with the aesthetic value \code{coulour.n}.
#' This function differs in use from \code{\link{lolli_plot}}
#' in that it must be created as a ggplot object. The benefit of this being
#' that one may add additional information to the figure as may be necessary.
#'
#' @author Robert W. Schlegel
#'
#' @export
#'
#' @examples
#' t_dat <- make_whole(sst_NW_Atl)
#' res <- detect(t_dat, climatology_start = 1983, climatology_end = 2012) # using default values
#'
#' \dontrun{
#' require(lubridate)
#' ggplot(res$event, aes(x = res$event$date_peak, y = res$event$duration)) +
#'   geom_lolli(n = 0, shape = 20, aes(colour = res$event$int_cum)) +
#'   scale_color_distiller(palette = "Spectral", name = "Cumulative \nintensity") +
#'   xlab("Date") + ylab("Event duration [days]")
#' }

geom_lolli <- function(mapping = NULL, data = NULL,
                      ...,
                      n = 1,
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  ggplot2::layer(
    geom = GeomLolli,
    data = data,
    mapping = mapping,
    stat = "identity",
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      n = n,
      ...
    )
  )
}

GeomLolli <- ggplot2::ggproto("GeomLolli", ggplot2::Geom,
                              required_aes = c("x", "y"),
                              default_aes = aes(shape = 19, colour = "grey35", size = 1, fill = NA,
                                                alpha = NA, stroke = 1, colour.n = "black"),

                              draw_key = draw_key_point,

                              draw_group = function(data, panel_scales, coord, n) {
                                data$xend = data$x
                                data$yend = 0
                                data = data[order(abs(data$y), decreasing = T),]

                                # Define the big points
                                big_points = data
                                big_points$size = data$size*2

                                # Define the top n events
                                data_n = data[1:n,]
                                data_n$colour = data$colour.n[1:n]
                                big_points_n = big_points[1:n,]
                                big_points_n$colour = data$colour.n[1:n]

                                # Define the look of the small white fillings
                                small_points = data
                                small_points$size = data$size/2
                                small_points$colour = "white"

                                grid::gList(
                                  ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
                                  ggplot2::GeomPoint$draw_panel(big_points, panel_scales, coord),
                                  ggplot2::GeomSegment$draw_panel(data_n, panel_scales, coord),
                                  ggplot2::GeomPoint$draw_panel(big_points_n, panel_scales, coord),
                                  ggplot2::GeomPoint$draw_panel(small_points, panel_scales, coord)
                                )

                              }
)
