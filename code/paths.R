# Created by Yixin Sun on 7/26/2017
# File for storing all the file paths that data building code can refer to

# source txt file that everyone keeps locally to create the ddir paths
library(readr)
library(stringr)
ddir <- str_trim(read_file(file.path(root, "data.txt")))

raw <- file.path(ddir, "raw_data")
int <- file.path(ddir, "intermediate_data")
raw_shape <- file.path(ddir, "shape_files")

gen <- file.path(ddir, "generated_data")
shape <- file.path(ddir, "generated_shape_files")

raw_lease <- file.path(raw, "leases")
raw_coversheets <- file.path(raw, "coversheets")
raw_bids <- file.path(raw, "bids")
raw_notices <- file.path(raw, "notices")
raw_payments <- file.path(raw, "payments")
raw_assign <- file.path(raw, "assignments")

fdir <- file.path(root, "output", "figures")
tdir <- file.path(root, "output", "tables")
edir <- file.path(root, "output", "estimates")
pdir <- file.path(root, "output", "press")
