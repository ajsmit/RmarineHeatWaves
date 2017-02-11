#' Create a Line Plot geom of Marine Heat Waves or Cold Spells.
#'
#' Creates a ggplot2 geom of warm or cold events as per the second row of Figure 3 in
#' Hobday et al. (2016).
#'
#' @importFrom ggplot2 ggplot aes geom_polygon geom_line scale_colour_manual
#' scale_fill_manual scale_x_date xlab ylab theme theme_grey element_text
#' element_blank element_rect element_line
#' @importFrom grid unit
#' @importFrom magrittr %>%
#'
#' @param data The function receives the output from the \code{\link{detect}} function.
#' @param spread The the number of days leading and trailing the largest event
#' (as per \code{metric}) detected within the time period specified by
#' \code{start_date} and \code{end_date}. The default is 150 days.
#' @param metric One of the following options: \code{int_mean}, \code{int_max}, \code{int_var},
#' \code{int_cum}, \code{int_mean_rel_thresh}, \code{int_max_rel_thresh}, \code{int_var_rel_thresh},
#' \code{int_cum_rel_thresh}, \code{int_mean_abs}, \code{int_max_abs}, \code{int_var_abs},
#' \code{int_cum_abs}, \code{int_mean_norm}, \code{int_max_norm}, \code{rate_onset}, \code{rate_decline}.
#' Partial name matching is currently not supported so please specify the metric
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
#' @return The function will create ggplot2 geom indicating the climatology,
#' threshold and temperature, with the hot or cold events that meet the
#' specifications of Hobday et al. (2016) shaded in as appropriate. The plotting
#' of hot or cold events depends on which option is specified in \code{\link{detect}}.
#' The top event detect during the selected time period will be visible in a
#' brighter colour. This function differs in use from \code{\link{event_line}}
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
#'ggplot(mhw, aes(x = date, y = temp, thresh = thresh_clim_year, seas = seas_clim_year, event = event_no)) +
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
  layer(
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

GeomFlame <- ggproto("GeomFlame", Geom,
                     
                     required_aes = c("x", "y", "thresh"),
                     
                     default_aes = aes(colour = NA, fill = NA,
                                       size = 0.5, linetype = 1, alpha = NA),
                     
                     draw_key = draw_key_polygon,
                     
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

GeomFlameOn <- ggproto("GeomFlameOn", Geom,
                       required_aes = c("x", "y", "thresh", "seas"), # "event" intentionally not included here as a required aes, but it does require it...
                       
                       default_aes = aes(colour = NA, fill.MHW = "salmon", fill.MHW.top = "red", 
                                         fill.MCS = "steelblue3", fill.MCS.top = "navy", 
                                         size = 0.5, linetype = 1, alpha = NA,
                                         temp.line = "black", seas.line = "grey60", thresh.line = "forestgreen"),
                       
                       draw_key = draw_key_polygon,
                       
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

#' Create a Timeline geom of Selected Event Metrics.
#'
#' Visualise a timeline of several event metrics as 'lollipops'.
#'
#' @importFrom magrittr %<>%
#' @importFrom ggplot2 aes_string geom_segment geom_point scale_x_continuous
#' element_rect element_line
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
#' The number of top events as per \code{event_count} will be highlighted
#' in a brighter colour. This function differs in use from \code{\link{lolli_plot}
#' in that it must be created as a ggplot() object. The benefit of this being
#' that one may add additional information to the figure as may be necessary.
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
#' ggplot() + geom_lolli_plot(res, metric = "int_cum", event_count = 3, xaxis = "date_peak") +
#' geom_text(aes(x = as.Date("2007-01-01"), y = 375, 
#' label = "One may clearly see\njust how dramatic\n these heatwaves are."))
#' }
geom_lolli_plot <- function(data,
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
    lolli_col <- c("salmon", "red")
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
  
  # Create the geom list for ggplot()
  lolli_stems <- geom_segment(data = event, aes_string(x = xaxis, y = metric, xend = xaxis, yend = 0, colour = "col"),
                              size = 0.6, lineend = "butt", show.legend = F)
  lolli_pops <- geom_point(data = event, aes_string(x = xaxis, y = metric, colour = "col", fill = "col"), 
                           shape = 21, size = 2.2)
  lolli_colour <- scale_colour_manual(name = NULL, values = lolli_col, guide = FALSE) 
  lolli_fill <- scale_fill_manual(name = NULL, values = c("ivory1", "grey40"), guide = FALSE)
  lolli_x <- xlab(xlabel)
  lolli_y <- ylab(ylabel)
  lolli_theme <- theme(plot.background = element_blank(),
                       panel.background = element_rect(fill = "white"),
                       panel.border = element_rect(colour = "black", fill = NA, size = 0.75),
                       panel.grid.minor = element_line(colour = NA),
                       panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dotted"),
                       axis.text = element_text(colour = "black"),
                       axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                       axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                       axis.ticks.length = unit(-0.25, "cm"))
  if(event[1, 4] < 0 & metric != "duration"){
    lolli_theme <- theme(plot.background = element_blank(),
                         panel.background = element_rect(fill = "white"),
                         panel.border = element_rect(colour = "black", fill = NA, size = 0.75),
                         panel.grid.minor = element_line(colour = NA),
                         panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dotted"),
                         axis.text = element_text(colour = "black"),
                         axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                         axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                         axis.ticks.length = unit(-0.25, "cm"),
                         legend.justification = c(0, 4.85))
  }
  res <- list(lolli_stems, lolli_pops, lolli_colour, lolli_fill, lolli_x, lolli_y, lolli_theme)
  if(xaxis == "event_no"){
    lolli_x <- scale_x_continuous(breaks = seq(from = 0, to = nrow(data$event), by = 5))
    res <- c(res, lolli_x)
  }
  return(res)
}
