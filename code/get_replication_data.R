library(here)
root <- here()
source(file.path(root, "code", "paths.R"))

replication_data <- "https://dataverse.harvard.edu/api/access/datafile/6392620"

options(timeout = max(36000, getOption("timeout")))

# check if raw and int exist
raw_missing <- is.na(file.info(raw)$size)
int_missing <- is.na(file.info(int)$size)
shp_missing <- is.na(file.info(raw_shape)$size)

# if any are missing, download replication file and expand contents in ddir
if((raw_missing | int_missing | shp_missing)) {
  print("Downloading and extracting replication data")
  temp <- tempfile()
  print(paste("tempfile location is: ", temp, sep = ""))
  download.file(replication_data, temp)
  unzip(temp, exdir = ddir)
  unlink(temp)
}