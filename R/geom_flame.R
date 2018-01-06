#' Create 'Flame' Ploygons.
#'
#' This function will create polygons between two lines. If given a
#' temperature and theshold time series, like that produced by \code{\link{detect}},
#' the output will meet the specifications of Hobday et al. (2016) shown as
#' 'flame polygons.' If one wishes to plot polygons below a given threshold, and not
#' above, switch the values being fed to the \code{y} and \code{y2}
#' aesthetics. This function differs in use from \code{\link{event_line}}
#' in that it must be created as a \code{ggplot} 'geom' object. The benefit
#' of this being that one may add additional information to the figure as geom
#' layers to ggplot2 graphs as may be necessary.
#'
#' @seealso \code{\link{event_line}} for a non-ggplot2 based flame function.
#'
#' @section Aesthetics:
#' \code{geom_flame} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{y2}}
#'   \item \code{colour}
#'   \item \code{fill}
#'   \item \code{size}
#'   \item \code{alpha}
#'   \item \code{linetype}
#' }
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()} or \code{aes_()}. If
#' specified and inherit.aes = TRUE (the default), it is combined with the
#' default mapping at the top level of the plot. You must supply mapping if
#' there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'
#' If NULL, the default, the data is inherited from the plot data as specified
#' in the call to \code{ggplot()}.
#'
#' A data.frame, or other object, will override the plot data. All objects will
#' be fortified to produce a data frame. See \code{fortify()} for which variables will
#' be created.
#'
#' A function will be called with a single argument, the plot data. The return
#' value must be a \code{data.frame}, and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer,
#' as a string.
#' @param position Position adjustment, either as a string, or the result of a call
#' to a position adjustment function.
#' @param show.legend Logical. Should this layer be included in the legends? \code{NA},
#' the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and
#' \code{TRUE} always includes. It can also be a named logical vector to finely select
#' the aesthetics to display.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#' than combining with them. This is most useful for helper functions that define
#' both data and aesthetics and shouldn't inherit behaviour from the default plot
#' specification, e.g. \code{borders()}.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning. If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
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
#' ts_dat <- make_whole(sst_WA)
#' res <- detect(ts_dat, climatology_start = 1983, climatology_end = 2012)
#' mhw <- res$clim
#' mhw <- mhw[10580:10690,]
#'
#' \dontrun{
#' require(ggplot2)
#' ggplot(mhw, aes(x = t, y = temp)) +
#'   geom_flame(aes(y2 = thresh_clim_year)) +
#'   geom_text(aes(x = as.Date("2011-02-01"), y = 28,
#'   label = "That's not a heatwave.\nThis, is a heatwave.")) +
#'   xlab("Date") + ylab(expression(paste("Temperature [", degree, "C]")))
#' }

geom_flame <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlame,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomFlame <- ggplot2::ggproto("GeomFlame", ggplot2::Geom,

                              required_aes = c("x", "y", "y2"),

                              default_aes = ggplot2::aes(colour = NA, fill = "salmon",
                                                size = 0.5, linetype = 1, alpha = NA),

                              draw_key = ggplot2::draw_key_polygon,

                              draw_group = function(data, panel_scales, coord, na.rm = FALSE) {
                                if (na.rm) data <- data[stats::complete.cases(data[c("x", "y", "y2")]), ]

                                # Check that aesthetics are constant
                                aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
                                if (nrow(aes) > 1) {
                                  stop("Aesthetics must be consistent")
                                }
                                aes <- as.list(aes)

                                # Find the ploygon corners
                                x1 <- data$y
                                x2 <- data$y2
                                # Find points where x1 is above x2.
                                above <- x1 > x2
                                above[above == TRUE] <- 1
                                above[is.na(above)] <- 0

                                # Points always intersect when above=TRUE, then FALSE or reverse
                                intersect.points <- which(diff(above) != 0)

                                # Find the slopes for each line segment.
                                x1.slopes <- x1[intersect.points + 1] - x1[intersect.points]
                                x2.slopes <- x2[intersect.points + 1] - x2[intersect.points]

                                # Find the intersection for each segment.
                                x.points <- intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes - x2.slopes))
                                y.points <- x1[intersect.points] + (x1.slopes * (x.points - intersect.points))

                                # Coerece x.points to the same scale as x
                                x_gap <- data$x[2] - data$x[1]
                                x.points <- data$x[intersect.points] + (x_gap*(x.points - intersect.points))

                                # Create new data frame and merge to introduce new rows of data
                                data2 <- data.frame(y = c(data$y, y.points), x = c(data$x, x.points))
                                data2 <- data2[order(data2$x),]
                                data <- base::merge(data, data2, by = c("x","y"), all.y = T)
                                data$y2[is.na(data$y2)] <- data$y[is.na(data$y2)]

                                # Remove missing values for better plotting
                                data$y[data$y < data$y2] <- NA
                                missing_pos <- !stats::complete.cases(data[c("x", "y", "y2")])
                                ids <- cumsum(missing_pos) + 1
                                ids[missing_pos] <- NA

                                positions <- plyr::summarise(data,
                                                             x = c(x, rev(x)), y = c(y, rev(y2)), id = c(ids, rev(ids)))
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
