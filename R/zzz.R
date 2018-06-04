.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
"
RmarineHeatWaves has been superceded by heatwaveR 0.2.7, which is available on
* CRAN: https://cran.r-project.org/package=heatwaveR
* GitHub: https://github.com/robwschlegel/heatwaveR

Only bug fixes will be implemented in RmarineHeatWaves, but active
development continues in heatwaveR.

See https://robwschlegel.github.io/heatwaveR for more information.
")
}
