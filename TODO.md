# THOUGHTS:
* My intention with the output is that the objects are simple enough for users 
to interact with directly ...
* Implement diagnostics, assessment, and exploration of the input data.
* Implement diagnostics, assessment, and exploration of these data objects.
* Could the review of these data objects be streamlined using classes and methods?
* If so, how should these be structured?

# TODO:
* Allow make_whole() function to provide helpful output, such as 
  - the start date,
  - the end date,
  - total number of NAs,
  - total % NAs,
  - mean % NAs per year
* Allow detect() to produce helpful output (verbose = TRUE), such as 
  - the start date of the time series,
  - its end date,
  - the climatological mean temperature,
  - the climatological coldest temperature, 
  - the climatological warmest temperature,
  - the threshold mean,
  - the threshold min,
  - the threshold max,
  - the number of events of <= 5 days in duration,
  - the number of events of 5 <= 10 days in duration,
  - the number of events of 10 <= 20 days in duration,
  - the metrics top three longest events, i.e.
    -- date_start,
    -- date_peak,
    -- date_stop,
    -- duration,
    -- int_mean,
    -- int_max,
    -- int_cum,
    -- int_var,
    -- rate_onset,
    -- rate_decline
  *** w.r.t. the above, another option might be to have a summary() function act 
  on the detect() functions' output; probably the first choice
