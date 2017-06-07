RmarineHeatWaves
================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/RmarineHeatWaves)](https://cran.r-project.org/package=RmarineHeatWaves) [![Travis-CI Build Status](https://travis-ci.org/ajsmit/RmarineHeatWaves.svg?branch=master)](https://travis-ci.org/ajsmit/RmarineHeatWaves) ![](https://cranlogs.r-pkg.org/badges/grand-total/RmarineHeatWaves)

The **RmarineHeatWaves** package is a translation of the original Python code written by Eric C. J. Oliver that can be found on [GitHub](https://github.com/ecjoliver/marineHeatWaves).

The **RmarineHeatWaves** R package contains a number of functions which calculate and display marine heat waves according to the definition of Hobday et al. (2016). The marine cold spell option was implemented in version 0.13 (21 Nov 2015) of the Python module as a result of the preparation of Schlegel et al. (2017), wherein the cold events are introduced and briefly discussed.

This package may be found on [CRAN](https://cran.r-project.org/package=RmarineHeatWaves). Alternatively, you can install it from GitHub by issuing the following command:

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
<th>Function</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>detect()</code></td>
<td>The main function which detects the events as per the definition of Hobday et al. (2016).</td>
</tr>
<tr class="even">
<td><code>make_whole()</code></td>
<td>Constructs a continuous, uninterrupted time series of temperatures.</td>
</tr>
<tr class="odd">
<td><code>block_average()</code></td>
<td>Calculates annual means for event metrics.</td>
</tr>
<tr class="even">
<td><code>event_line()</code></td>
<td>Creates a line plot of marine heat waves or cold spells.</td>
</tr>
<tr class="odd">
<td><code>lolli_plot()</code></td>
<td>Creates a timeline of selected event metrics.</td>
</tr>
<tr class="even">
<td><code>exceedance()</code></td>
<td>A function similar to <code>detect()</code> but that detects consecutive days above/ below a given threshold.</td>
</tr>
<tr class="odd">
<td><code>geom_flame()</code></td>
<td>Creates flame polygons of marine heat waves or cold spells.</td>
</tr>
<tr class="even">
<td><code>geom_lolli()</code></td>
<td>Creates a lolliplot timeline of selected event metric.</td>
</tr>
</tbody>
</table>

The package also provides data of observed SST records for three historical MHWs: the 2011 Western Australia event, the 2012 Northwest Atlantic event and the 2003 Mediterranean event.

The detection and graphing functions
------------------------------------

The `detect()` function is the package's core function. Here is the `detect()` function applied to the Western Australian test data, which are also discussed by Hobday et al. (2016):

``` r
library(RmarineHeatWaves); library(plyr); library(dplyr); library(ggplot2)
ts <- make_whole(sst_WA)
mhw <- detect(ts, climatology_start = 1983, climatology_end = 2012)
mhw$event %>% 
  ungroup() %>%
  select(event_no, duration, date_start, date_peak, int_mean, int_max, int_cum) %>% 
  dplyr::arrange(-int_cum)
#> # A tibble: 60 x 7
#>    event_no duration date_start  date_peak int_mean  int_max   int_cum
#>       <int>    <dbl>     <date>     <date>    <dbl>    <dbl>     <dbl>
#>  1       22       95 1999-05-13 1999-05-22 2.498305 3.601700 237.33900
#>  2       42       60 2011-02-06 2011-02-28 3.211903 6.505969 192.71420
#>  3       49       47 2012-01-11 2012-01-27 2.225734 3.300112 104.60948
#>  4       50       46 2012-03-01 2012-04-10 1.993709 2.957609  91.71061
#>  5       41       40 2010-12-24 2011-01-28 2.157016 3.274803  86.28064
#>  6       31       34 2008-03-26 2008-04-14 2.236577 3.769274  76.04363
#>  7       18       38 1996-11-17 1996-12-21 1.820192 2.467469  69.16728
#>  8       23       29 2000-04-21 2000-05-04 1.949836 2.741701  56.54523
#>  9       48       29 2011-11-30 2011-12-19 1.760314 2.281731  51.04911
#> 10       39       24 2010-10-30 2010-11-03 1.623114 2.585006  38.95475
#> # ... with 50 more rows
```

The corresponding `event_line()` and `lolli_plot()`, which represent the massive Western Australian heatwave of 2011, look like this:

``` r
event_line(mhw, spread = 200, metric = "int_cum",
           start_date = "2010-10-01", end_date = "2011-08-30")
```

![](tools/fig-example1-1.png)

``` r
lolli_plot(mhw)
```

![](tools/fig-example2-1.png)

The `event_line()` and `lolli_plot()` functions were designed to work directly on one of the list returned by `detect()`. If more control over the figures is required, it may be useful to create them in **ggplot2** by stacking 'geoms'. We specifically created two new **ggplot2** geoms to reproduce the functionality of `event_line()` and `lolli_plot()`. These functions are more general in their functionality and can be used outside of the **RmarineHeatWave** package too. To apply them to MHWs and MCSs, they require that we access the `clim` or `event` data frames within the list that is produced by `detect()`. Here is how:

``` r
mhw2 <- mhw$clim # find the climatology dataframe
mhw2 <- mhw2[10580:10690,] # identify the region of the time series of interest

# ggplot(mhw2, aes(x = date, y = temp, y2 = thresh_clim_year)) +
#   geom_flame() +
#   geom_text(aes(x = as.Date("2011-02-01"), y = 28, label = "The MHW that launched\na thousand papers."))
# 
# ggplot(mhw$event, aes(x = date_start, y = int_max)) +
#   geom_lolli(colour = "salmon", colour.n = "red", n = 3) +
#   geom_text(aes(x = as.Date("2006-10-01"), y = 5, 
#                 label = "The distribution of events\nis skewed towards the\nend of the time series."),
#             colour = "black")
```

The default output of these function may not be to your liking. If so, not to worry. As **ggplot2** geoms, they are highly maleable. For example, if we were to choose to reproduce the format of the MHWs as seen in Hobday et al. (2016), the code would look something like this:

``` r
# It is necessary to give geom_flame() at least one row on either side of the event in order to calculate the polygon corners smoothly
mhw_top <- mhw2[49:110,]

ggplot(data = mhw2, aes(x = date)) +
  geom_flame(aes(y = temp, y2 = thresh_clim_year, fill = "all"), show.legend = T) +
  geom_flame(data = mhw_top, aes(y = temp, y2 = thresh_clim_year, fill = "top"), show.legend = T) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh_clim_year, colour = "thresh"), size = 1.0) +
  geom_line(aes(y = seas_clim_year, colour = "seas"), size = 1.2) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black", "thresh" =  "forestgreen", "seas" = "grey80")) +
  scale_fill_manual(name = "Event Colour", values = c("all" = "salmon", "top" = "red")) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  xlab("Date") +
  ylab(expression(paste("Temperature [", degree, "C]")))
```

![](tools/fig-example4-1.png)

Conversely, should we not wish to highlight any events with `geom_lolli()`, it would look like this:

``` r
# Note that this is accomplished by setting 'colour.n = NA', not by setting 'n = 0'.
ggplot(mhw$event, aes(x = date_start, y = int_cum)) +
  geom_lolli(colour = "salmon", n = 3, colour.n = NA)
```

![](tools/fig-example5-1.png)

The calculation and visualisation of marine cold spells is also accommodated within this package. Here is a cold spell detected in the OISST data for Western Australia:

``` r
mcs <- detect(ts, climatology_start = 1983, climatology_end = 2012, cold_spells = TRUE)
mcs$event %>% 
  ungroup() %>%
  select(event_no, duration, date_start, date_peak, int_mean, int_max, int_cum) %>% 
  dplyr::arrange(int_cum)
#> # A tibble: 71 x 7
#>    event_no duration date_start  date_peak  int_mean   int_max    int_cum
#>       <int>    <dbl>     <date>     <date>     <dbl>     <dbl>      <dbl>
#>  1       16       76 1990-04-13 1990-05-11 -2.538017 -3.218054 -192.88929
#>  2       54       58 2003-12-19 2004-01-23 -1.798455 -2.662320 -104.31038
#>  3       71       52 2014-04-14 2014-05-05 -1.818984 -2.565533  -94.58715
#>  4        8       38 1986-06-24 1986-07-17 -2.009802 -2.950536  -76.37248
#>  5       51       32 2003-09-08 2003-09-16 -1.560817 -2.116583  -49.94613
#>  6       31       28 1993-03-18 1993-04-11 -1.710159 -2.494210  -47.88444
#>  7       47       27 2002-09-11 2002-09-25 -1.714935 -2.618133  -46.30325
#>  8       40       22 1997-08-09 1997-08-22 -1.688167 -2.290205  -37.13966
#>  9       29       18 1992-06-20 1992-07-05 -1.934667 -2.329971  -34.82400
#> 10       41       18 1997-09-08 1997-09-15 -1.735893 -2.273214  -31.24607
#> # ... with 61 more rows
```

The plots showing the marine cold spells look like this:

``` r
event_line(mcs, spread = 200, metric = "int_cum",
           start_date = "1990-01-01", end_date = "1990-08-30")

lolli_plot(mcs)
```

![](tools/fig-example6-1.png) ![](tools/fig-example6-2.png)

Cold spell figures may be created as geoms in **ggplot2**, too:

``` r
mcs2 <- mcs$clim
mcs2 <- mcs2[2990:3190,]

# # Note that the plot centres on the polygons, so it may be necessary to manually zoom out a bit
ggplot(data = mcs2, aes(x = date)) +
  geom_flame(aes(y = thresh_clim_year, y2 = temp), fill = "steelblue3", show.legend = F) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh_clim_year, colour = "thresh"), size = 1.0) +
  geom_line(aes(y = seas_clim_year, colour = "seas"), size = 1.2) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black", "thresh" =  "forestgreen", "seas" = "grey80")) +
  scale_y_continuous(limits = c(18, 23.5)) +
  xlab("Date") +
  ylab(expression(paste("Temperature [", degree, "C]")))

ggplot(mcs$event, aes(x = date_start, y = int_cum)) +
  geom_lolli(colour = "steelblue3", colour.n = "navy", n = 7) +
  xlab("Date") +
  ylab(expression(paste("Cumulative intensity [days x ", degree, "C]")))
```

![](tools/fig-example7-1.png) ![](tools/fig-example7-2.png)

The exceedance function
-----------------------

In addition to the calculation of extreme events, consecutive days over a given static threshold may be calculated with the `exceedance()` function.

``` r
exc <- exceedance(ts, threshold = 25)
exc$exceedance %>% 
  ungroup() %>%
  select(exceedance_no, duration, date_start, date_peak, int_mean, int_max, int_cum) %>% 
  dplyr::arrange(-int_cum)
#> # A tibble: 11 x 7
#>    exceedance_no duration date_start  date_peak  int_mean   int_max
#>            <int>    <dbl>     <date>     <date>     <dbl>     <dbl>
#>  1             7       52 2011-02-08 2011-02-28 1.6740379 4.7399993
#>  2             6       25 2008-04-03 2008-04-14 0.9799994 2.1899994
#>  3            10       41 2012-03-03 2012-04-10 0.4385360 1.3699994
#>  4             2       17 1999-05-13 1999-05-22 0.8558818 1.3999994
#>  5             5       10 2000-05-03 2000-05-04 0.6969994 1.0099994
#>  6            11       10 2013-03-02 2013-03-09 0.3439994 0.8999994
#>  7             8        9 2011-04-20 2011-04-22 0.3555550 0.6899994
#>  8             9        6 2012-02-08 2012-02-09 0.5266661 0.8999994
#>  9             3        7 1999-06-02 1999-06-03 0.2071423 0.2699994
#> 10             1        5 1989-05-05 1989-05-06 0.2859994 0.3599994
#> 11             4        6 2000-04-21 2000-04-23 0.1549994 0.4099994
#> # ... with 1 more variables: int_cum <dbl>
```

The same function may be used to calculate consecutive days below a threshold, too.

``` r
exc <- exceedance(ts, threshold = 19, below = TRUE)
exc$exceedance %>%
  ungroup() %>%
  select(exceedance_no, duration, date_start, date_peak, int_mean, int_max, int_cum) %>%
  dplyr::arrange(int_cum)
#> # A tibble: 22 x 7
#>    exceedance_no duration date_start  date_peak   int_mean    int_max
#>            <int>    <dbl>     <date>     <date>      <dbl>      <dbl>
#>  1            17       46 2003-09-06 2003-09-16 -0.6008700 -1.3400004
#>  2            16       31 2002-09-08 2002-09-25 -0.8480649 -1.8800004
#>  3            13       24 1997-09-03 1997-09-15 -0.7691671 -1.4900004
#>  4            20       25 2005-09-26 2005-10-12 -0.5420004 -1.1000004
#>  5            12       18 1997-08-13 1997-08-22 -0.6944449 -1.2500004
#>  6             1       20 1982-09-15 1982-09-24 -0.4080004 -0.7600004
#>  7             2       17 1986-07-14 1986-07-17 -0.4605886 -0.9400004
#>  8            15       11 2000-08-06 2000-08-13 -0.6890913 -1.0800004
#>  9             5       26 1990-08-22 1990-09-10 -0.2288466 -0.5200004
#> 10            21        8 2006-09-05 2006-09-07 -0.6237504 -0.9000004
#> # ... with 12 more rows, and 1 more variables: int_cum <dbl>
```

Working with gridded SST data
=============================

We can also load the gridded 0.25 degree Reynolds [OISST data](https://www.ncei.noaa.gov/thredds/blended-global/oisst-catalog.html) and apply the function pixel by pixel over all of the days of data. The example data used here have 93 longitude steps, 43 latitude steps, and cover 12797 days (1981 to 2016). We apply the `detect()` function to these data, fit a generalised linear model (GLM), and then plot the trend per decade of the marine heatwave count. In other words, have marine heatwaves become more or less frequent in recent years? Under climate change we can expect that extreme events would tend to occur more frequently and be of greater intensity. Indeed, we can clearly see in the figure below of the result of the GLM, how the Agulhas Current has been experiencing marine heat waves more frequently in recent decades. But there are two smaller areas, one along the western side of the Cape Peninsula in the Benguela Upwelling system and another around the Eastern Cape Province near Algoa Bay, where the frequency of marine heat waves seems to have actually been decreasing -- although the *P*-value of the decreasing trend is &gt; 0.05, and therefore not significant.

![Count-trend](vignettes/README-grid-example1.png)

![P-of-trend](vignettes/README-grid-example2.png)

Please read the package [vignette](https://github.com/ajsmit/RmarineHeatWaves/blob/master/vignettes/gridded-event-detection.Rmd) to see how to load a netCDF file with the OISST data, apply the RmarineHeatWaves function to the whole 3D array of data, and then fit the GLM and plot the data.

References
==========

Hobday, A.J. et al. (2016). A hierarchical approach to defining marine heatwaves, Progress in Oceanography, 141, pp. 227-238.

Schlegel, R. W., Oliver, E. C. J., Wernberg, T. W., Smit, A. J. (2017). Coastal and offshore co-occurrences of marine heatwaves and cold-spells. Progress in Oceanography, 151, pp. 189-205.

Acknowledgements
================

The Python code was written by Eric C. J. Oliver.

Contributors to the Marine Heatwaves definition and its numerical implementation include Alistair J. Hobday, Lisa V. Alexander, Sarah E. Perkins, Dan A. Smale, Sandra C. Straub, Jessica Benthuysen, Michael T. Burrows, Markus G. Donat, Ming Feng, Neil J. Holbrook, Pippa J. Moore, Hillary A. Scannell, Alex Sen Gupta, and Thomas Wernberg.

The translation from Python to R was done by A. J. Smit and the graphing functions were contributed to by Robert. W. Schlegel.

Contact
=======

A. J. Smit Department for Biodiversity & Conservation Biology, University of the Western Cape, Private Bag X17, Bellville 7535, South Africa, E-mail: <ajsmit@uwc.ac.za>, Work tel.: +27 (0)21 959 3783
