---
output: pdf_document
---

# VERSION CHANGES
## Changes in version 0.15.4 (5 March 2017)
* The detect() function now also produces a 'climatological' variance.
* Improve documentation in a help file.

## Changes in version 0.15.3 (28 February 2017)
* The detect() function may now be told to calculate climatologies only.

## Changes in version 0.15.2 (18 February 2017)
* Updating README.md, README.Rmd files.
* Updating the vignette.

## Changes in version 0.15.1 (15 February 2017)
* 'exceedence' changed to 'exceedance' throughout.

## Changes in version 0.15.0 (15 February 2017)
* Final edits to 'geom_flame' and 'geom_lolli';
  - specifically in geom_flame, small gaps in the corners of polygons are now 
  being filled in correctly.
* Updated examples.

## Changes in version 0.14.6 (15 February 2017)
* Edits to 'geom_flame' and 'geom_lolli'.

## Changes in version 0.14.5 (14 February 2017)
* Edits to 'geom_flame' and 'geom_lolli'.

## Changes in version 0.14.4 (11 February 2017)
* Changed 'geom_event_line' and 'geom_lolli_plot' to 'geom_flame' and 'geom_lolli'
* These new functions are now proper ggplot geoms.

## Changes in version 0.14.3 (31 January 2017)
* Corrected plotting error in event polygons in 'event_line' function
* Added two new functions: 'geom_event_line' and 'geom_lolli_plot' which may be 
used with ggplot2 directly as geoms.

## Changes in version 0.14.2 (26 January 2017)
* Fix bug which was that was brought to my attention by Mahmoud Haouari: enable 
the pctile option in the detect() function, which was accidentally disabled.

## Changes in version 0.14.1 (10 January 2017)
* Improved error messages in exceedance().
* Bug fix.

## Changes in version 0.14.0 (11 December 2016)
* All changes since version 0.13.1.

## Changes in version 0.13.9.9200 (11 December 2016)
* Minor edits to documentation.
* Corrected issue where the event_line graph's y-axis did not reflect the correct 
units for the metric selected for plotting.
* Updated the theme, i.e. removed the default ggplot2 theme in favour
of a more conventional white background with nice black axes.
* Removed the legend that indicated the peak and secondary events from the 
event_line plot. 
* Repositioned the legend in the event_line plot: it is now int he bottom right 
corner as here it has less chance of plotting over the peak events when the 
graph is rescaled.

## Changes in version 0.13.8.9200 (23 November 2016)
* Minor update to exceedance() function to improve usability.
* Usage of exceedance() shown in README file

## Changes in version 0.13.7.9100 (22 October 2016)
* The additions to the detect() function madde in the previous update have been 
rolled back in favour of creating an independent function to calculate exceedances.

## Changes in version 0.13.6.9000 (22 October 2016)
* Added 'threshold' variable to detect() function.
* This now allows the function to detect when temperatures are over (under) a 
static threshold supplied by the user.
* An example is given in the README.md file

## Changes in version 0.13.5.9000 (8 October 2016)
* Expanded example included as package vignette.

## Changes in version 0.13.4.9000 (8 October 2016)
* Added a marine cold spell example.

## Changes in version 0.13.3.9000 (9 September 2016)
* Correcting a few typos.

## Changes in version 0.13.1.9000 (9 June 2016)
* The new development version.

## Changes in version 0.13.1 (9 June 2016)
* Edit to README.md file.

## Changes in version 0.13.0 (8 June 2016)
* Significant changes to detection algorithm:
  - ... more extensive use of magrittr pipe operators %>% and %<>%
  - ... simplification of protoFunc helper function
  - ... removal of automatic start/end day calculation if these are not provided
  - ... climatology start/end dates must now always be explicitely set
  - ... finally enabled join_across_gaps option
  - ... fixed a bug that caused the function to fail when none of the gaps between
  events (i.e. proto_gaps) were less than max_gap
* Update examples to accommodate these changes.

## Changes in version 0.12.2 (6 June 2016)
* Add README.md file.
* Various minor changes to documentation of several functions.

## Changes in version 0.12.1 (6 June 2016)
* Fix Windows build dependency warnings.

## Changes in version 0.12.0 (2 June 2016)
* Added lolliplot functionality -- lolli_plot().

## Changes in version 0.11.2 (2 June 2016)
* Simplify make_whole() -- it should accept dates as class POSIXct or Date 
without the need for unneccesary 'if' logic.
* All example data dates (t) changed to class Date.
* Some minor rewording to documentation.

## Changes in version 0.11.1 (1 June 2016)
* Minor edits to event_line() as per Robert Schlegel (allows broader selection of
metrics for plotting.)

## Changes in version 0.11.0 (1 June 2016)
* block_average() rewritten -- it is now based upon dplyr functions so it is 
faster and more stream-lined.
* Completely removed the use of reshape2 in favour of tidyr.

## Changes in version 0.10.3 (1 June 2016)
* All comments removed inside of functions.

## Changes in version 0.10.2 (1 June 2016)
* Minor changes to the version change info provided for v0.10.1. 

## Changes in version 0.10.1 (31 May 2016)
* Fixed a bug that caused detect() to fail whenever it encountered fewer than two 
non-NAs in the period doy 59 to doy 61 when it was asked to interpolate over 
the non-existent day-60 during non-leap years. This is specific to versions of
'zoo' (required for na.approx()) up to 1.7-12; from 1.7-13 it works fine. A few 
extra lines of code were added to fix this.

## Changes in version 0.10.0 (30 May 2016)
* Added the block_average() function.

## Changes in version 0.9.3 (29 May 2016)
* Expanded documentation.

## Changes in version 0.9.2 (29 May 2016)
* Changes to event_line() documentation.
* Renamed 'metric' options to function as 'mean', 'maximum', 'cumulative'.
* Added more TODOs at the bottom of this file.

## Changes in version 0.9.1 (28 May 2016)
* Minor refinements to the documentation (i.e. package description).

## Changes in version 0.9.0 (28 May 2016)
* Replace 'mhw' with 'event'.
* Add basic plotting functionality a-la Robert Schlegel in the form of the 
event_line() function (plus edits to make Rob's code produce a clean build 
process, thereby avoiding throwing 'notes' that might be frowned upon by the 
CRAN people.)
* Renamed some things: 'make_whole.R' -> 'makeWhole.R'; 'marineHeatWaves-package.r' 
-> 'RmarineHeatWaves-package.r'; 'marineHeatWaves.R' -> 'RmarineHeatWaves.R'.

## Changes in version 0.8.2 (22 May 2016)
* Include CITATION file.
* Update DESCRIPTION file.
* Update author, creator and contributor roles.

## Changes in version 0.8.1 (22 May 2016)
* Add dates to NEWS.md file.

## Changes in version 0.8.0 (22 May 2016)
* Marine cold spell option enabled.
* Hacky changes to suppress some notes during package compilation, needed for
acceptance to CRAN, apparently.

## Changes in version 0.7.0 (20 May 2016)
* Internal variable name changes (replace more camelCase).
* Less problematic handling of NA.
* Improved/expanded documentation.

## Changes in version 0.6.0 (19 May 2016)
* Internal variable name changes (replace camelCase).
* Split out make_whole() function.
* Add make_whole() example.

## Changes in version 0.5.0 (18 May 2016)
* Added data and a functional example.
* Slightly improved documentation.

## Changes in version 0.4.0 (18 May 2016)
* Robert Schlegel added as co-author for work on graphing functions.

## Changes in version 0.3.0 (18 May 2016)
* Changed 'eventNo' calculation to properly reflect the actual events, and not 
one of the proto-event types.
* Changed ID variable in 'mhw' output to 'eventNo', thereby following the same
naming convention.
* Removed ', .id = NULL' from the event metrics 'ldply' calculations, enabling
compatibility with plyr (version < 1.8.3).

## Changes in version 0.2.0 (17 May 2016)
* Add 'eventNo' to climatology output to identify each unique event and to
fascilitate plotting of filled polygons using 'geom_polygon' in ggplot2, as per
Robert Schlegel's suggestion.
