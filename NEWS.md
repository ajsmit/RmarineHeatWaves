---
output: pdf_document
---
# RmarineHeatWaves 0.8.0.

# Version changes:
## Changes in version 0.8.0
* MCS option enabled.
* Hacky changes to suppress some notes during package compilation.

## Changes in version 0.7.0
* Internal variable name changes (remove more camelCase).
* Less problematic handling of NA.
* Improved/expanded documentation.

## Changes in version 0.6.0
* Internal variable name changes (replace camelCase).
* Split out 'make_whole' function.
* Add 'make_whole' example.

## Changes in version 0.5.0
* Added data and a functional example.
* Slightly improved documentation.

## Changes in version 0.4.0
* Robert Schlegel added as co-author for work on graphing functions.

## Changes in version 0.3.0
* Changed 'eventNo' calculation to properly reflect the actual events, and not 
one of the proto-event types.
* Changed ID variable in 'mhw' output to 'eventNo', thereby following the same
naming convention.
* Removed ', .id = NULL' from the event metrics 'ldply' calculations, enabling
compatibility with plyr (version < 1.8.3).

## Changes in version 0.2.0
* Add 'eventNo' to climatology output to identify each unique event and to
fascilitate plotting of filled polygons using 'geom_polygon' in ggplot2, as per
Robert Schlegel's suggestion.

# TODO:
* Allow 'make_whole' function to provide helpful output, such as % NAs per year,
the number of missing dates filled in, and the start and end dates of the time
series.
* Test if specified years in 'climatology_period' are full years.
* What happens when 'climatology_period' is given but dates outside of the time 
series are provided? It should fail with the appropriate error message 
retruned - possibly something helpful such as start and end dates of the first
and last full years.
* Enable 'smooth_percentile' = TRUE / FALSE.
* Enable 'join_across_gaps' = TRUE / FALSE.