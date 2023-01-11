#===========================================================================
# DEFINE SOME CONSTANTS/FUNCTIONS WE MIGHT USE REPEATEDLY
#===========================================================================
discount <- .1 
modiscount <- 1-(1/(1 + discount))^(1/12)


# unit conversions --------------------------------
mcf_mmbtu <- 1.037 # https://www.eia.gov/tools/faqs/faq.php?id=45&t=8
mcf_to_boe <- 5.65853
m2_to_acre <- 0.000247105
m2_per_acre <- 4046.86
mi_to_m2 <- 1609.344

# projections -------------------------------------
main_crs <- 4269
utm_crs <- 32614

# dates -------------------------------------------
LastProductionDate <- ymd(20190301)
FirstProductionDate <- ymd(20000801)
SampleLeaseStart <- make_date(2004,1,1)
SampleLeaseEnd <- make_date(2016,12,31)
CensorDate <- make_date(2013,12,31)

# random forest seed: integer representation of the day the RAL was codified
# by the Texas Supreme Court
rfseed <- 19310821
set.seed(rfseed)

# dml replication count: ideally this is an odd number, but the dml routine
# will add 1 if you give it something even
# dml_n <- 11
dml_n <- 101

# common control variable lists
parcel_characteristics_1 <-
  c("ShapeQuality", "DistWaterbodies", "DistRiverStreams")

parcel_characteristics_2 <-
  c("DistRoads", "CoverShrub", "CoverForest",
    "CoverCultivated", "CoverDeveloped")

parcel_characteristics_names_1 <-
  c("Shape", "Water", "Rivers")

parcel_characteristics_names_2 <-
  c("Roads", "Shrub", "Forest", "Cultivated", "Developed")

  
