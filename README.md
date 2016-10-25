<!-- README.md is generated from README.Rmd. Please edit that file -->
RmarineHeatWaves
================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/RmarineHeatWaves)](http://cran.r-project.org/package=RmarineHeatWaves) [![Travis-CI Build Status](https://travis-ci.org/ajsmit/RmarineHeatWaves.svg?branch=master)](https://travis-ci.org/ajsmit/RmarineHeatWaves) ![](http://cranlogs.r-pkg.org/badges/grand-total/RmarineHeatWaves)

The RmarineHeatWaves package is a translation of the original Python code written by Eric C. J. Oliver that can be found on [GitHub](https://github.com/ecjoliver/marineHeatWaves).

The RmarineHeatWaves R package contains a number of functions which calculate and display marine heat waves according to the definition of Hobday et al. (2016). The marine cold spell option was implemented in version 0.13 (21 Nov 2015) of the Python module as a result of the preparation of Schlegel et al. (submitted), wherein the cold events are introduced and briefly discussed.

This package may be found on [CRAN](https://cran.r-project.org/web/packages/RmarineHeatWaves/index.html). Alternatively, you may install it from GitHub by issuing the following command:

`devtools::install_github("ajsmit/RmarineHeatWaves")`

The functions
=============

<table style="width:47%;">
<colgroup>
<col width="30%" />
<col width="16%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Function</th>
<th align="left">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><code>detect()</code></td>
<td align="left">The main function which detects the events as per the definition of Hobday et al. (2016).</td>
</tr>
<tr class="even">
<td align="left"><code>make_whole()</code></td>
<td align="left">Constructs a continuous, uninterrupted time series of temperatures.</td>
</tr>
<tr class="odd">
<td align="left"><code>block_average()</code></td>
<td align="left">Calculates annual means for event metrics.</td>
</tr>
<tr class="even">
<td align="left"><code>event_line()</code></td>
<td align="left">Creates a line plot of marine heat waves or cold spells.</td>
</tr>
<tr class="odd">
<td align="left"><code>lolli_plot()</code></td>
<td align="left">Creates a timeline of selected event metrics.</td>
</tr>
</tbody>
</table>

The package also provides data of observed SST records for three historical MHWs: the 2011 Western Australia event, the 2012 Northwest Atlantic event and the 2003 Mediterranean event.

For example, here is the `detect()` function applied to the Western Australian test data, which are also discussed by Hobday et al. (2016):

``` r
library(RmarineHeatWaves); library(dplyr)
ts <- make_whole(sst_WA)
mhw <- detect(ts, climatology_start = 1983, climatology_end = 2012)
mhw$event %>% 
  ungroup() %>%
  select(event_no, duration, date_start, date_peak, int_mean, int_max, int_cum) %>% 
  dplyr::arrange(-int_cum)
#> # A tibble: 60 x 7
#>    event_no duration date_start  date_peak int_mean  int_max   int_cum
#>       <int>    <dbl>     <date>     <date>    <dbl>    <dbl>     <dbl>
#> 1        22       95 1999-05-13 1999-05-22 2.498305 3.601700 237.33900
#> 2        42       60 2011-02-06 2011-02-28 3.211903 6.505969 192.71420
#> 3        49       47 2012-01-11 2012-01-27 2.225734 3.300112 104.60948
#> 4        50       46 2012-03-01 2012-04-10 1.993709 2.957609  91.71061
#> 5        41       40 2010-12-24 2011-01-28 2.157016 3.274803  86.28064
#> 6        31       34 2008-03-26 2008-04-14 2.236577 3.769274  76.04363
#> 7        18       38 1996-11-17 1996-12-21 1.820192 2.467469  69.16728
#> 8        23       29 2000-04-21 2000-05-04 1.949836 2.741701  56.54523
#> 9        48       29 2011-11-30 2011-12-19 1.760314 2.281731  51.04911
#> 10       39       24 2010-10-30 2010-11-03 1.623114 2.585006  38.95475
#> # ... with 50 more rows
```

The corresponding `event_line()` and `lolli_plot()`, which represent the massive Western Australian heatwave of 2011, look like this:

``` r
event_line(mhw, spread = 200, metric = "int_cum",
           start_date = "2010-10-01", end_date = "2011-08-30")
```

![](README-fig-example1-1.png)

``` r
lolli_plot(mhw)
```

![](README-fig-example1-2.png)

Marine cold spells are also accommodated. Here is a cold spell detected in the Western Australian OISST data:

``` r
mcs <- detect(ts, climatology_start = 1983, climatology_end = 2012, cold_spells = TRUE)
mcs$event %>% 
  ungroup() %>%
  select(event_no, duration, date_start, date_peak, int_mean, int_max, int_cum) %>% 
  dplyr::arrange(int_cum)
#> # A tibble: 71 x 7
#>    event_no duration date_start  date_peak  int_mean   int_max    int_cum
#>       <int>    <dbl>     <date>     <date>     <dbl>     <dbl>      <dbl>
#> 1        16       76 1990-04-13 1990-05-11 -2.538017 -3.218054 -192.88929
#> 2        54       58 2003-12-19 2004-01-23 -1.798455 -2.662320 -104.31038
#> 3        71       52 2014-04-14 2014-05-05 -1.818984 -2.565533  -94.58715
#> 4         8       38 1986-06-24 1986-07-17 -2.009802 -2.950536  -76.37248
#> 5        51       32 2003-09-08 2003-09-16 -1.560817 -2.116583  -49.94613
#> 6        31       28 1993-03-18 1993-04-11 -1.710159 -2.494210  -47.88444
#> 7        47       27 2002-09-11 2002-09-25 -1.714935 -2.618133  -46.30325
#> 8        40       22 1997-08-09 1997-08-22 -1.688167 -2.290205  -37.13966
#> 9        29       18 1992-06-20 1992-07-05 -1.934667 -2.329971  -34.82400
#> 10       41       18 1997-09-08 1997-09-15 -1.735893 -2.273214  -31.24607
#> # ... with 61 more rows
```

The plots showing the marine cold spells look like this:

``` r
event_line(mcs, spread = 200, metric = "int_cum",
           start_date = "1990-01-01", end_date = "1990-08-30")
```

![](README-fig-example2-1.png)

``` r
lolli_plot(mcs)
```

![](README-fig-example2-2.png)

In the development version of ggplot2 (2.1.0.9001), Hadley has *again* fiddled with some of the plot defaults and this has resulted in the legend position in the above plots being affected. I will make sure that the next release of RmarineHeatWaves fixes this problem. The next version of this package will also include some major updates to the plotting functions. An update will be released in the next month or two.

We can also load the gridded 0.25 degree Reynolds [OISST data](http://www.ncdc.noaa.gov/thredds/oisst-catalog.html) and apply the function pixel by pixel over all of the days of data. The example data used here have 93 longitude steps, 43 latitude steps, and cover 12797 days (1981 to 2016). We apply the `detect()` function to these data, fit a generalised linear model (GLM), and then plot the trend per decade of the marine heatwave count. In other words, have marine heatwaves become more or less frequent in recent years? Under climate change we can expect that extreme events would tend to occur more frequently and be of greater intensity. Indeed, we can clearly see in the figure below of the result of the GLM, how the Agulhas Current has been experiencing marine heat waves more frequently in recent decades. But there are two smaller areas, one along the western side of the Cape Peninsula in the Benguela Upwelling system and another around the Eastern Cape Province near Algoa Bay, where the frequency of marine heat waves seems to have actually been decreasing -- although the P-value of the decreasing trend is &gt; 0.05, and therefore not significant.

![](README-fig-example3.png) ![](README-fig-example4.png)

Please read the package [vignette](https://github.com/ajsmit/RmarineHeatWaves/blob/master/vignettes/gridded-event-detection.Rmd) to see how to load a netCDF file with the OISST data, apply the RmarineHeatWaves function to the whole 3D array of data, and then fit the GLM and plot the data.

References
==========

Hobday, A.J. et al. (2016). A hierarchical approach to defining marine heatwaves, Progress in Oceanography, 141, pp. 227-238. <DOI:10.1016/j.pocean.2015.12.014>. [PDF.](http://passage.phys.ocean.dal.ca/~olivere/docs/Hobdayetal_2016_PO_HierarchMHWDefn.pdf)

Schlegel, R. W., Oliver, E. C. J., Wernberg, T. W., Smit, A. J. (in review). Coastal and offshore co-occurrences of marine heatwaves and cold-spells. Progress in Oceanography.

Acknowledgements
================

The Python code was written by Eric C. J. Oliver.

Contributors to the Marine Heatwaves definition and its numerical implementation include Alistair J. Hobday, Lisa V. Alexander, Sarah E. Perkins, Dan A. Smale, Sandra C. Straub, Jessica Benthuysen, Michael T. Burrows, Markus G. Donat, Ming Feng, Neil J. Holbrook, Pippa J. Moore, Hillary A. Scannell, Alex Sen Gupta, and Thomas Wernberg.

The translation from Python to R was done by A. J. Smit and the graphing functions were contributed to by Robert. W. Schlegel.

Contact
=======

A. J. Smit Department for Biodiversity & Conservation Biology, University of the Western Cape, Private Bag X17, Bellville 7535, South Africa, E-mail: <ajsmit@uwc.ac.za>, Work tel.: +27 (0)21 959 3783
