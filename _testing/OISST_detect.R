 OISST_detect <- function(dat, otherclim = FALSE, clim = 0) {

  if (otherclim) {
    test <- dat[,c(1 ,4)]
    colnames(test)[2] <- "temp"
    #test <- clim[,c(1 ,3)]
    diff_baseline = TRUE
    whole_clim <- make_whole(test)
  } else {
    diff_baseline = FALSE}

  datt <- dat[,c(1 ,3)]
  start = "1982-01-01"
  end = "2016-12-31"
  whole <- make_whole(datt)

  #whole

  mhw <- detect2(whole, climatology_start = start, climatology_end = end,
                 min_duration = 5, max_gap = 2, cold_spells = FALSE,
                 diff_baseline = diff_baseline, baseline_data = whole_clim)
  events <- mhw$event
  #clim <- mhw$clim
  return(events)
  #return(clim)
}
