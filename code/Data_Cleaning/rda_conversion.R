#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
library(here)
root <- here()

library(tidyverse)
library(lubridate)
library(sf)
library(lwgeom)
library(gstat)
library(sp)


source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))

# ===========================================================================
# Convert roads shapefile to sf
# ===========================================================================
roads <-
  file.path(raw_shape, "txdot-roads_tx/txdot-2015-roadways_tx.shp") %>%
  st_read(stringsAsFactors = F) %>%
  st_transform(main_crs) %>%
  select(Route_Name = RTE_NM,
         Route_Prefix = RTE_PRFX_C,
         Route_Number = RTE_NBR,
         Roadbed_Type = RDBD_TYPE,
         Route_Class = RTE_CLASS,
         Street_Name = ST_NM,
         RouteID = RTE_ID,
         Route_Open = RTE_OPEN) %>%
  st_transform(utm_crs)

save(roads, file = file.path(shape, "roads.Rda"))

# =============================================================================
# Converting surface water shapefile to sf
# =============================================================================
waterbodies <-
  file.path(raw_shape, "usgs-rivers_tx/Waterbodies/Waterbodies.shp") %>%
  st_read(stringsAsFactors = F) %>%
  st_transform(utm_crs)

save(waterbodies,
  file = file.path(shape, "waterbodies.Rda"))

# =============================================================================
# Converting rivers and streams shapefile to sf
# =============================================================================
river_streams <-
  file.path(raw_shape, "usgs-rivers_tx/Rivers_Streams/Rivers_Streams.shp") %>%
  st_read(stringsAsFactors = F) %>%
  select(-Shape_Leng) %>%
  st_zm() %>%
  st_transform(utm_crs)

save(river_streams,
  file = file.path(shape, "river_streams.Rda"))

# ==========================================================================
# Select Shale Plays and Basins of interest
# ==========================================================================
texas <-
  file.path(raw_shape, "us_county/cb_2015_us_county_500k.shp") %>%
  st_read(stringsAsFactors = T) %>%
  mutate(NAME = toupper(NAME)) %>%
  filter(STATEFP == "48") %>%
  st_transform(utm_crs) %>%
  st_union()

shale_names <-
  c("Barnett", "Haynesville-Bossier",
    "Spraberry", "Delaware",
    "Eagle Ford")

basin_names <-
  c("ANADARKO", "TX-LA-MS SALT",
    "PERMIAN", "WESTERN GULF",
    "FORT WORTH")

shale_plays <-
  file.path(raw_shape,
            "TightOil_ShalePlays_US_EIA_Jan2019",
            "TightOil_ShalePlays_US_EIA_Jan2019.shp") %>%
  st_read(stringsAsFactors = F) %>%
  select(Basin, Shale_play) %>%
  st_transform(utm_crs) %>%
  filter(Shale_play %in% shale_names) %>%
  st_intersection(texas)

save(shale_plays, file = file.path(shape, "shale_plays.Rda"))

# ==========================================================================
# Load in shale isopach data
# ==========================================================================
isopach_permian <-
  file.path(raw_shape, "TightOil_ShaleGas_IndividualPlays_Lower48_EIA") %>%
  list.files(pattern = "*Wolfcamp_Isopach(.*)shp$", full.names = TRUE) %>%
  map(read_sf) %>%
  reduce(rbind) %>%
  filter(!st_is_empty(.)) %>%
  st_transform(utm_crs)

isopach_eagle_ford <-
  file.path(raw_shape,
            "TightOil_ShaleGas_IndividualPlays_Lower48_EIA",
            "ShalePlay_EagleFord_Isopach_EIA_Aug2015.shp") %>%
  read_sf %>%
  filter(!st_is_empty(.)) %>%
  st_transform(utm_crs)

# --------------
# spatially interpolate the line data
# --------------
isopach_permian_point <-
  isopach_permian %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT") %>%
  as("Spatial")

isopach_eagle_ford_point <-
  isopach_eagle_ford %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT") %>%
  as("Spatial")

# want to only interpolate within Eagleford, Spraberry and Delware
# note that the raw isopach files above cover Eagle Ford and "Wolfcamp", and
# the wolfcamp covers both the spraberry and delaware halves of the permian

isopach_permian_boundary <-
  shale_plays %>%
  filter(Shale_play %in% c("Spraberry", "Delaware")) %>%
  as("Spatial")

isopach_eagle_ford_boundary <-
  shale_plays %>%
  filter(Shale_play == "Eagle Ford") %>%
  as("Spatial")

# create empty grids to interpolate into
# note the hard coded proj4 string
p4string <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs"
proj4string(isopach_permian_point) <- p4string
proj4string(isopach_eagle_ford_point) <- p4string

isopach_permian_grid <-
  as.data.frame(sp::spsample(isopach_permian_point, "regular", n = 10000))
names(isopach_permian_grid) <- c("X", "Y")
coordinates(isopach_permian_grid) <- c("X", "Y")
gridded(isopach_permian_grid) <- TRUE
fullgrid(isopach_permian_grid) <- TRUE
proj4string(isopach_permian_grid) <- p4string

isopach_eagle_ford_grid <-
  as.data.frame(sp::spsample(isopach_eagle_ford_point, "regular", n = 10000))
names(isopach_eagle_ford_grid) <- c("X", "Y")
coordinates(isopach_eagle_ford_grid) <- c("X", "Y")
gridded(isopach_eagle_ford_grid) <- TRUE
fullgrid(isopach_eagle_ford_grid) <- TRUE
proj4string(isopach_eagle_ford_grid) <- p4string

# interpolate over the grids
isopach_permian_interpolate <-
  gstat::idw(Thick_ft ~ 1,
             isopach_permian_point,
             newdata = isopach_permian_grid) %>%
  raster::raster() %>%
  raster::mask(isopach_permian_boundary)

isopach_eagle_ford_interpolate <-
  gstat::idw(Thick_ft ~ 1,
             isopach_eagle_ford_point,
             newdata = isopach_eagle_ford_grid) %>%
  raster::raster() %>%
  raster::mask(isopach_eagle_ford_boundary)

save(isopach_permian_interpolate,
  file = file.path(shape, "isopach_permian_interpolate.Rda"))

save(isopach_eagle_ford_interpolate,
  file = file.path(shape, "isopach_eagle_ford_interpolate.Rda"))

# ==========================================================================
# construct grids
# ==========================================================================
# function to intersect leases with grids
st_texas_grid <- function(bb, cs){
  bb %>%
    st_make_grid(cellsize = c(cs, cs)) %>%
    st_sf(GridID = seq(1, length(.)), geometry = ., crs = utm_crs) %>%
    st_transform(main_crs)
}

# use bounding box of parcels to create grids
parcel_bb <-
  file.path(raw_shape,
            "texas_grids_bounding_box/texas_grids_bounding_box.shp") %>%
  read_sf %>%
  st_transform(utm_crs)

texas_grid5 <- st_texas_grid(parcel_bb, mi_to_m2 * 5)
texas_grid10 <- st_texas_grid(parcel_bb, mi_to_m2 * 10)
texas_grid20 <- st_texas_grid(parcel_bb, mi_to_m2 * 20)

save(texas_grid5, file = file.path(shape, "texas_grid5.Rda"))
save(texas_grid10, file = file.path(shape, "texas_grid10.Rda"))
save(texas_grid20, file = file.path(shape, "texas_grid20.Rda"))
