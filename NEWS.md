---
output: pdf_document
---
# RmarineHeatWaves 0.12.0

# VERSION CHANGES
## Changes in version 0.12.0 (2-JUNE-2016)
* Added lolliplot functionality.

## Changes in version 0.11.2 (2-JUNE-2016)
* Simplify make_whole() -- it should accept dates as class POSIXct or Date 
without the need for unneccesary if logic options.
* All example data dates (t) changed to class Date.
* Some minor rewording to documentation.

## Changes in version 0.11.1 (1-JUNE-2016)
* Minor edits to event_line as per Robert Schlegel (allows broader selection of
metrics for plotting.)

## Changes in version 0.11.0 (1-JUNE-2016)
* block_average() rewritten -- it is now based upon dplyr functions so it is 
faster and more stream-lined.
* Completely removed the use of reshape2 in favour of tidyr.

## Changes in version 0.10.3 (1-JUNE-2016)
* All comments removed inside of functions.

## Changes in version 0.10.2 (1-JUNE-2016)
* Minor changes to the version change info provided for v0.10.1. 

## Changes in version 0.10.1 (31-MAY-2016)
* Fixed a bug that caused detect() to fail whenever it encountered fewer than two 
non-NAs in the period doy 59 to doy 61 when it was asked to interpolate over 
the non-existent day-60 during non-leap years. This is specific to versions of
'zoo' (required for na.approx()) up to 1.7-12; from 1.7-13 it works fine. A few 
extra lines of code were added to fix this.

## Changes in version 0.10.0 (30-MAY-2016)
* Added the block_average() function.

## Changes in version 0.9.3 (29-MAY-2016)
* Expanded documentation.

## Changes in version 0.9.2 (29-MAY-2016)
* Changes to event_line() documentation.
* Renamed 'metric' options to function as 'mean', 'maximum', 'cumulative'.
* Added more TODOs at the bottom of this file.

## Changes in version 0.9.1 (28-MAY-2016)
* Minor refinements to the documentation (i.e. package description).

## Changes in version 0.9.0 (28-MAY-2016)
* Replace 'mhw' with 'event'.
* Add basic plotting functionality a-la Robert Schlegel in the form of the 
event_line() function (plus edits to make Rob's code produce a clean build 
process, thereby avoiding throwing 'notes' that might be frowned upon by the 
CRAN people.)
* Renamed some things: 'make_whole.R' -> 'makeWhole.R'; 'marineHeatWaves-package.r' 
-> 'RmarineHeatWaves-package.r'; 'marineHeatWaves.R' -> 'RmarineHeatWaves.R'.

## Changes in version 0.8.2 (22-MAY-2016)
* Include CITATION file.
* Update DESCRIPTION file.
* Update author, creator and contributor roles.

## Changes in version 0.8.1 (22-MAY-2016)
* Add dates to NEWS.md file.

## Changes in version 0.8.0 (22-MAY-2016)
* Marine cold spell option enabled.
* Hacky changes to suppress some notes during package compilation, needed for
acceptance to CRAN, apparently.

## Changes in version 0.7.0 (20-MAY-2016)
* Internal variable name changes (replace more camelCase).
* Less problematic handling of NA.
* Improved/expanded documentation.

## Changes in version 0.6.0 (19-MAY-2016)
* Internal variable name changes (replace camelCase).
* Split out make_whole() function.
* Add make_whole() example.

## Changes in version 0.5.0 (18-MAY-2016)
* Added data and a functional example.
* Slightly improved documentation.

## Changes in version 0.4.0 (18-MAY-2016)
* Robert Schlegel added as co-author for work on graphing functions.

## Changes in version 0.3.0 (18-MAY-2016)
* Changed 'eventNo' calculation to properly reflect the actual events, and not 
one of the proto-event types.
* Changed ID variable in 'mhw' output to 'eventNo', thereby following the same
naming convention.
* Removed ', .id = NULL' from the event metrics 'ldply' calculations, enabling
compatibility with plyr (version < 1.8.3).

## Changes in version 0.2.0 (17-MAY-2016)
* Add 'eventNo' to climatology output to identify each unique event and to
fascilitate plotting of filled polygons using 'geom_polygon' in ggplot2, as per
Robert Schlegel's suggestion.

# TODO:
* Allow make_whole() function to provide helpful output, such as % NAs per year,
the number of missing dates filled in, and the start and end dates of the time
series.
* Test if specified years in 'climatology_period' are full years.
* What happens when 'climatology_period' is given but dates outside of the time 
series are provided? It should fail with the appropriate error message 
retruned - possibly something helpful such as start and end dates of the first
and last full years.
* Enable 'smooth_percentile' = TRUE / FALSE.
* Enable 'join_across_gaps' = TRUE / FALSE.
* event_line() function: if 'start_date' and 'end_date' are not provided, make
the function automatically select the beginning and end dates of the whole time
series.
* event_line() function: Windows computers do not easily produce pdf files as 
output. Provide the option to output png and jpg files, and maybe have a jpg as
default.

# THOUGHTS:
* My intention with the output is that the objects are simple enough for users 
to interact with directly ...
* Implement diagnostics, assessment, and exploration of the input data.
* Implement diagnostics, assessment, and exploration of these data objects.
* Could the review of these data objects be streamlined using classes and methods?
* If so, how should these be structured?