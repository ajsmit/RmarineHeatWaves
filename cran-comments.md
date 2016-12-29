# Resubmission:

These errors were returned to me:

A.
Found the following (possibly) invalid URLs:
  URL: http://cran.r-project.org/package=RmarineHeatWaves
    From: README.md
    CRAN URL not in canonical form
  Canonical CRAN.R-project.org URLs use https.
  
--> fixed... changed to https://cran.r-project.org/package=RmarineHeatWaves
  
B.
  * checking files in ???vignettes??? ... WARNING
Files in the 'vignettes' directory newer than all files in 'inst/doc':
  ???gridded-event-detection.Rmd???
  
--> removed files in inst/doc as no pdf was present
