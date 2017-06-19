#' RmarineHeatWaves.
#'
#' This package is an R implementation of the python script \code{marineHeatWaves}
#' (\url{https://github.com/ecjoliver/marineHeatWaves}) written by Eric C. J.
#' Oliver as part of the marine heat waves definition by Hobday et al. (2016).
#'
#' @details Although the title of the package refers to marine heat waves (MHW),
#' it is equally capable of detecting marine cold spells (MCS). This
#' functionality is also present in the python package, where it was
#' implemented as a result of the publication Schlegel et al. (2017)
#' that discusses the quantification and detection of anomalously cold events.
#'
#' The main function is the detection function \code{\link{detect}} which takes as
#' input a time series of temperature (and a corresponding series of dates)
#' and outputs a set of detected MHWs or MCS, as well as the climatological
#' (varying by day-of-year) seasonal cycle and extremes threshold. There are
#' various helper functions to fascilitate developing an uninterrupted time
#' series of temperatures (e.g. \code{\link{make_whole}}) and some options to produce
#' graphical summaries and representations of the detected events such as
#' \code{\link{event_line}} and \code{\link{lolli_plot}}, or the ggplot2
#' equivalents, \code{\link{geom_flame}} and \code{\link{geom_lolli}}.
#'
#' This package is demonstrated by applying the MHW definition to observed SST
#' records and showing how it identifies three historical MHWs: the 2011
#' Western Australia event, the 2012 Northwest Atlantic event and the 2003
#' Mediterranean event. These data are included herewith.
#'
#' One may also use the \code{\link{exceedance}} function to calculate consecutive
#' days above or below a given static threshold. The output of this function is
#' similar to \code{\link{detect}}.
#'
#' @author Albertus J. Smit <\email{albertus.smit@gmail.com}>, Robert W. Schlegel,
#' Eric C. J. Oliver
#'
#' @references Hobday, A. J. et al. (2016), A hierarchical approach to defining
#' marine heatwaves. Progress in Oceanography, 141, pp. 227-238,
#' <DOI:10.1016/j.pocean.2015.12.014> (official citation for this package).
#'
#' Schlegel, R. W., Oliver, E. C. J., Wernberg, T. W., Smit, A. J. (2017)
#' Coastal and offshore co-occurrences of marine heatwaves and cold-spells.
#' Progress in Oceanography, 151, pp. 189-205, <DOI:10.1016/j.pocean.2017.01.004>
#'
#' @name RmarineHeatWaves
#' @docType package
NULL
