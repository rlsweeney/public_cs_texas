# Install necessary packages
packages <-
  c("tidyverse", "lubridate", "readxl", "sf", "fuzzyjoin", "raster", "lwgeom",
    "knitr", "broom", "kableExtra", "lmtest", "sandwich", "splines", "fixest",
    "Formula", "furrr", "grf", "grid", "gstat", "rgdal", "rgeos",
    "exactextractr", "RISCA", "boot")

check_install <- function(pkg){
  if(!(pkg %in% installed.packages())){
    install.packages(pkg, repos="http://cran.rstudio.com/", type = "binary")
    print(paste(pkg, " is now installed"))
  } else {
    print(paste(pkg, " was already installed"))
  }
}

print("checking if we need to install packages")

lapply(packages, check_install)
