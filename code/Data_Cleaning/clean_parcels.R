# created by Yixin Sun May 2018
# Modified by Thom Covert February 2020 from the original clean_parcels.R
# Finalized by Thom Covert in June 2020

# load packages we need
library(sf)
library(raster)
library(exactextractr)
library(tidyverse)
library(lubridate)
library(readxl)
library(here)

root <- here()
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "texas_constants.R"))

# we don't want spherical geometry
sf_use_s2(FALSE)

# ==============================================================================
# load in PSF lands
# ==============================================================================
# load in psf shapefile.  note: this map sometimes has more than one polygon
# associated with a control number, usually when a public road or river/stream
# intersects an original PSF parcel, so in a later step we aggregate acreage up
# to the control number

load(file.path(shape, "psf.Rda"))
state_parcels_map <-
  psf %>%
  arrange(ControlNum) %>%
  group_by(ControlNum) %>%
  mutate(ParcelID = paste(ControlNum,
                          str_pad(as.character(row_number()),
                                  2,
                                  side = "left",
                                  pad = "0"),
                          sep = "-")) %>%
  ungroup %>%
  st_transform(main_crs) %>%
  filter(ControlNum != " ") %>% 
  st_make_valid

# =============================================================================
# add in grids to the original PSF map
# =============================================================================
load(file.path(shape, "texas_grid5.Rda"))
load(file.path(shape, "texas_grid10.Rda"))
load(file.path(shape, "texas_grid20.Rda"))

grid_overlap5 <-
  select(state_parcels_map, ParcelID) %>%
  st_intersection(texas_grid5) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(area = st_area(.)) %>%
  group_by(ParcelID) %>%
  arrange(-area) %>%
  filter(row_number() == 1) %>%
  as.data.frame() %>%
  select(ParcelID, Grid5 = GridID)

grid_overlap10 <-
  select(state_parcels_map, ParcelID) %>%
  st_intersection(texas_grid10) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(area = st_area(.)) %>%
  group_by(ParcelID) %>%
  arrange(-area) %>%
  filter(row_number() == 1) %>%
  as.data.frame() %>%
  select(ParcelID, Grid10 = GridID)

grid_overlap20 <-
  select(state_parcels_map, ParcelID) %>%
  st_intersection(texas_grid20) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(area = st_area(.)) %>%
  group_by(ParcelID) %>%
  arrange(-area) %>%
  filter(row_number() == 1) %>%
  as.data.frame() %>%
  select(ParcelID, Grid20 = GridID)

state_parcels_map <-
  state_parcels_map %>%
  left_join(grid_overlap5, by = "ParcelID") %>%
  left_join(grid_overlap10, by = "ParcelID") %>%
  left_join(grid_overlap20, by = "ParcelID") %>%
  mutate_if(is.character, toupper)

#==============================================================================
# merge in shale play info, isopach, distance to infrastructure, landcover
# then collapse to control number level
#==============================================================================

# compute closest shale and whether parcel overlies or is just near shale
# note: uses function from utils.R that finds closest shale
state_parcels_map <-
  closest_shale(state_parcels_map)

# compute distances to infrastructure
spm_slim <-
  state_parcels_map %>%
  st_transform(utm_crs) %>%
  select(ParcelID)

# gc() is because these eat a lot of ram
gc()
load(file.path(shape, "roads.Rda"))
# note: uses function from utils.R that finds distance to nearest infrastructure
roads <-
  infra_dist(spm_slim, roads, DistRoads, ParcelID)

gc()
load(file.path(shape, "waterbodies.Rda"))
waterbodies <-
  infra_dist(spm_slim, waterbodies, DistWaterbodies, ParcelID)

gc()
load(file.path(shape, "river_streams.Rda"))
river_streams <-
  infra_dist(spm_slim, river_streams, DistRiverStreams, ParcelID)
gc()

state_parcels_map <-
  state_parcels_map %>%
  left_join(roads, by = "ParcelID") %>%
  full_join(waterbodies, by = "ParcelID") %>%
  full_join(river_streams, by = "ParcelID")

# add in shale isopach values for eagle ford and permian shale parcels
load(file.path(shape, "isopach_permian_interpolate.Rda"))
load(file.path(shape, "isopach_eagle_ford_interpolate.Rda"))

parcels_isopach_permian <-
  raster::extract(isopach_permian_interpolate,
                  state_parcels_map,
                  fun = mean)

parcels_isopach_eagle_ford <-
  raster::extract(isopach_eagle_ford_interpolate,
                  state_parcels_map,
                  fun = mean)

state_parcels_map <-
  state_parcels_map %>%
  mutate(ShaleThicknessPermian = parcels_isopach_permian,
         ShaleThicknessEagleFord = parcels_isopach_eagle_ford) %>%
  mutate(ShaleThickness = case_when(
           !is.na(ShaleThicknessPermian) ~ ShaleThicknessPermian,
           !is.na(ShaleThicknessEagleFord) ~ ShaleThicknessEagleFord,
           TRUE ~ NA_real_)) %>%
  select(-ShaleThicknessPermian, -ShaleThicknessEagleFord)

# newer, faster landcover code, based on the package exactextractr
landcover <-
  file.path(raw_shape, "Land_Cover/landcover") %>%
  raster 
lc_proj <- projection(landcover)

parcels <- 
  state_parcels_map %>%
  select(ParcelID) %>%
  st_transform(lc_proj)

parcel_ids <-
  parcels %>%
  st_set_geometry(NULL) %>%
  select(ParcelID) %>%
  mutate(id = as.character(row_number()))

parcel_landcover <-
  exact_extract(landcover, parcels) %>%
  bind_rows(.id = "id") %>%
  as_tibble %>%
  left_join(parcel_ids, by = "id") %>%
  left_join(classification_dict, by = "value") %>%
  select(-value, -id) %>%
  group_by(ParcelID, Classification) %>%
  summarize(Total = sum(coverage_fraction)) %>%
  mutate(pct = Total / sum(Total)) %>%
  select(-Total) %>%
  ungroup %>%
  pivot_wider(ParcelID,
              names_from = Classification,
              values_from = pct,
              values_fill = 0) %>%
  mutate(CoverDeveloped =
           0.10 * Developed_OS +
           0.35 * Developed_LI +
           0.65 * Developed_MI +
           0.90 * Developed_HI,
         CoverWater = Open_Water, 
         CoverBarren = Barren, 
         CoverForest = rowSums(.[c("Deciduous_Forest",
                                   "Evergreen_Forest", 
                                   "Mixed_Forest")]),
         CoverShrub = Shrub_Scrub,
         CoverGrass = Grassland, 
         CoverCultivated = rowSums(.[c("Pasture",
                                       "Cultivated_Crops")]),
         CoverWetlands = rowSums(.[c("Woody_Wetlands",
                                     "Herbaceous_Wetlands")])) %>%
  select(ParcelID, starts_with("Cover"))

state_parcels_map <-
  state_parcels_map %>%
  left_join(parcel_landcover, by = "ParcelID")
  
# collapse down to ControlNum level
# helper functions
textmode <- function(v) {
  if (min(is.na(v)) == 1) {
    return(NA_character_)
  } else {
    vv <- v[!is.na(v)]
    uv <- unique(vv)
    return(uv[which.max(tabulate(match(vv, uv)))])
  }
}

takefirst <- function(v) {
  return(v[1])
}

# compute the area of the convex hull for each parcel
hull <-
  state_parcels_map %>%
  select(ControlNum) %>%
  st_convex_hull() %>%
  mutate(ParcelHullAcres = as.numeric(st_area(geometry) * m2_to_acre)) %>%
  st_set_geometry(NULL) %>%
  select(ControlNum, ParcelHullAcres) %>%
  group_by(ControlNum) %>%
  summarize(ParcelHullAcres = sum(ParcelHullAcres))

state_parcels_map <-
  state_parcels_map %>%
  left_join(hull, by = "ControlNum") %>%
  arrange(ControlNum, desc(Polygon_Acres)) %>%
  group_by(ControlNum, LandType) %>%
  mutate(across(c(Grid5, Grid10, Grid20, CoverDeveloped, CoverWater,
                  CoverBarren, CoverForest, CoverShrub, CoverGrass,
                  CoverCultivated, CoverWetlands, DistRoads, DistWaterbodies,
                  DistRiverStreams),
                takefirst)) %>%
  group_by(ControlNum, LandType, Grid5, Grid10, Grid20, CoverDeveloped,
           CoverWater, CoverBarren, CoverForest, CoverShrub, CoverGrass,
           CoverCultivated, CoverWetlands, DistRoads, DistWaterbodies,
           DistRiverStreams, ParcelHullAcres) %>%
  summarize(NPolygons = n(), ParcelAcres = sum(Polygon_Acres),
            OnShale = max(OnShale) == 1, Shale_play = textmode(Shale_play),
            County = textmode(County),
            ShaleThickness = mean(ShaleThickness),
            do_union = FALSE) %>%
  ungroup

# =============================================================================
# add GPS coordinates of centroid for each parcel shape
# =============================================================================
parcel_centroids <-
  state_parcels_map %>%
  select(ControlNum) %>%
  st_centroid %>%
  st_coordinates %>%
  as_tibble %>%
  rename(CentLong = X, CentLat = Y)

state_parcels_map <-
  state_parcels_map %>% 
  bind_cols(parcel_centroids)

# =============================================================================
# last step: generate the ParcelType variable and flag state leases with
# potentially private surface owners, and change units for acres (to thousands)
# thickness (to thousands of feet) and distance (to thousands of meters)
# =============================================================================
clean_parcels <-
  state_parcels_map %>%
  mutate(ParcelType = case_when(
           LandType %in% c("04", "15") ~ "STATE",
           LandType == "07" ~ "RAL",
           LandType == "08" ~ "FREE",
           TRUE ~ "OTHER"),
         PrivateSurface = LandType %in% c("07", "08", "15")) %>%
  mutate(ShaleThickness = ShaleThickness / 1000) %>%
  mutate(across(starts_with("Dist"), ~ .x / 1000)) %>%
  mutate(ShapeQuality = ParcelAcres / ParcelHullAcres) %>%
  rename(PSFLandNumber = LandType, ShalePlay = Shale_play) %>%
  mutate(ShalePlay = str_to_upper(ShalePlay))

# =============================================================================
# save to disk
# =============================================================================
save(clean_parcels, file = file.path(gen, "clean_parcels.Rda"))



