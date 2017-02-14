#' Create flame ploygons of Marine Heat Waves or Cold Spells.
#'
#' Creates a ggplot2 geom of warm or cold events as per the second row of Figure 3 in
#' Hobday et al. (2016).
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
#' @param stat.top Choose which statistic to use to highlight the top event
#' found within the time series. Options are: \code{int_max} for the maximum
#' intensity of the event, \code{int_cum} for the cummulative intensity of
#' the event, \code{int_mean} for the mean intensity of the event and \code{dur}
#' for the duration of the event.
#'
#' @return The function will create a ggplot2 geom indicating the climatology,
#' threshold and temperature, with the hot or cold events that meet the
#' specifications of Hobday et al. (2016) shown as flame polygons. The function
#' automagically detects if one is plotting MHWs or MCS and adjusts the colour
#' palette accordingly. These default colours may still be overridden as normal.
#' The top event detect during the selected time period, as determined by the
#' selected statistics given in \code{stat.top} will be visible in a
#' brighter colour. The default colour may be overidden in the aesthetics as well.
#' This function differs in use from \code{\link{event_line}}
#' in that it must be created as a ggplot() object. The benefit of this being
#' that one may add additional information to the figure as may be necessary.
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
#' mhw <- res$clim
#' mhw <- mhw[10580:10690,]
#'
#' \dontrun{
#' library(ggplot2)
#' ggplot(mhw, aes(x = date, y = temp, thresh = thresh_clim_year, seas = seas_clim_year, event = event_no)) +
#'  geom_flame() +
#'  geom_text(aes(x = as.Date("2011-02-01"), y = 28, label = "Wow. Such heatwave. Many warm."))
#' }

geom_flame <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       stat.top = "int_cum",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlameOn,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      stat.top = stat.top,
      na.rm = na.rm,
      ...
    )
  )
}

GeomFlame <- ggplot2::ggproto("GeomFlame", ggplot2::Geom,

                              required_aes = c("x", "y", "thresh"),

                              default_aes = aes(colour = NA, fill = NA,
                                                size = 0.5, linetype = 1, alpha = NA),

                              # draw_key = draw_key_polygon,

                              draw_group = function(data, panel_scales, coord, na.rm = FALSE) {
                                if (na.rm) data <- data[stats::complete.cases(data[c("x", "y", "thesh")]), ]

                                # Check that aesthetics are constant
                                aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
                                if (nrow(aes) > 1) {
                                  stop("Aesthetics must be consistent")
                                }

                                aes <- as.list(aes)

                                missing_pos <- !stats::complete.cases(data[c("x", "y", "thresh")])
                                ids <- cumsum(missing_pos) + 1
                                ids[missing_pos] <- NA

                                positions <- plyr::summarise(data,
                                                             x = c(x, rev(x)), y = c(y, rev(thresh)), id = c(ids, rev(ids)))
                                munched <- coord_munch(coord, positions, panel_scales)

                                grid::polygonGrob(
                                  munched$x, munched$y, id = munched$id,
                                  default.units = "native",
                                  gp = grid::gpar(
                                    fill = alpha(aes$fill, aes$alpha),
                                    col = aes$colour,
                                    lwd = aes$size * .pt,
                                    lty = aes$linetype)
                                )
                              }
)

GeomFlameOn <- ggplot2::ggproto("GeomFlameOn", ggplot2::Geom,
                                required_aes = c("x", "y", "thresh", "seas"), # "event" intentionally not included here as a required aes, but it does require it...

                                default_aes = aes(colour = NA, fill.MHW = "salmon", fill.MHW.top = "red",
                                                  fill.MCS = "steelblue3", fill.MCS.top = "navy",
                                                  size = 0.5, linetype = 1, alpha = NA,
                                                  temp.line = "black", seas.line = "grey60", thresh.line = "forestgreen"),

                                # draw_key = draw_key_polygon,

                                draw_group = function(data, panel_scales, coord, stat.top, na.rm = FALSE){

                                  # Deduce if MHWs or MCSs are being measured and set default fill colours accordingly
                                  flame_all <- data
                                  if(flame_all$y[complete.cases(flame_all$event)][1] > flame_all$thresh[complete.cases(flame_all$event)][1]){
                                    flame_all$y[flame_all$y < flame_all$thresh] <- NA
                                    flame_top <- flame_all
                                    flame_all$fill <- data$fill.MHW
                                    flame_top$fill <- data$fill.MHW.top
                                  } else if (flame_all$y[complete.cases(flame_all$event)][1] < flame_all$thresh[complete.cases(flame_all$event)][1]){
                                    flame_all$y[flame_all$y > flame_all$thresh] <- NA
                                    flame_top <- flame_all
                                    flame_all$fill <- data$fill.MCS
                                    flame_top$fill <- data$fill.MCS.top
                                  }

                                  # Choose event to subset
                                  flame_stats <- plyr::ddply(data[complete.cases(data$event),], "event", plyr::summarise,
                                                             int_max = max(abs(y-thresh),na.rm = T),
                                                             int_cum = abs(sum(y-thresh, na.rm = T)),
                                                             int_mean = abs(mean(y-thresh, na.rm = T)),
                                                             dur = length(y))
                                  flame_var <- data.frame(event = flame_stats$event, var = flame_stats[,colnames(flame_stats) == as.character(stat.top)])
                                  flame_top <- flame_top[flame_top$event == flame_var$event[flame_var$var == max(flame_var$var)],]

                                  # Create temperature line info
                                  temp_line <- data
                                  temp_line$colour <- data$temp.line

                                  # Create seasonal climatology line info
                                  seas_line <- data
                                  seas_line$y <- data$seas
                                  seas_line$colour <- data$seas.line

                                  # Create threshold line info
                                  thresh_line <- data
                                  thresh_line$y <- data$thresh
                                  thresh_line$colour <- data$thresh.line

                                  # The list of geoms to create
                                  grid::gList(
                                    GeomFlame$draw_panel(flame_all, panel_scales, coord),
                                    GeomFlame$draw_panel(flame_top, panel_scales, coord),
                                    ggplot2::GeomLine$draw_panel(temp_line, panel_scales, coord),
                                    ggplot2::GeomLine$draw_panel(seas_line, panel_scales, coord),
                                    ggplot2::GeomLine$draw_panel(thresh_line, panel_scales, coord)
                                  )
                                }
)
