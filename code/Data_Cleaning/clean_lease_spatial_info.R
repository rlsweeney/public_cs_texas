# Created by Thom Covert in June 2021 based on prior work by Yixin Sun
# compute lease-level values of variables that are derived at the parcel level
# using clean parcels and the parcel-lease bridge

# load packages we need
library(sf)
library(tidyverse)
library(lubridate)
library(readxl)
library(fuzzyjoin)
library(here)

root <- here()
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "texas_constants.R"))

sf_use_s2(FALSE)

# =============================================================================
# load in the mineral file to control number match, and merge it with variables
# already computed in the psf data
# =============================================================================
load(file.path(gen, "parcel_lease_bridge.Rda"))
parcel_lease_bridge <-
  parcel_lease_bridge %>%
  filter(BridgeType != "REMOVED") %>%
  select(-BridgeType)

load(file.path(gen, "clean_parcels.Rda"))
parcel_lease_latlon_centroid_shapes <-
  clean_parcels %>%
  inner_join(parcel_lease_bridge) %>%
  group_by(Lease_Number) %>%
  summarize %>%
  st_cast("MULTIPOLYGON")

parcel_lease_latlon_centroid_values <-
  parcel_lease_latlon_centroid_shapes %>%
  st_centroid %>%
  st_coordinates %>%
  as_tibble %>%
  rename(CentLong = X, CentLat = Y)

parcel_lease_latlon_centroid <-
  parcel_lease_latlon_centroid_shapes %>%
  st_set_geometry(NULL) %>%
  bind_cols(parcel_lease_latlon_centroid_values)

clean_parcels <-
  clean_parcels %>%
  st_set_geometry(NULL)

parcel_lease_matching <-
  parcel_lease_bridge %>%
  left_join(clean_parcels) %>%
  group_by(Lease_Number) %>%
  summarize(NParcels = n(), NCleanParcels = sum(!is.na(ParcelAcres)))

parcel_lease_other <-
  parcel_lease_bridge %>%
  inner_join(clean_parcels) %>%
  group_by(Lease_Number) %>%
  summarize(TotalParcelAcresInLease = sum(ParcelAcres),
            ParcelShapeQuality = mean(ShapeQuality),
            PrivateSurface = max(PSFLandNumber %in% c("07", "15")) == 1)

parcel_lease_shale <-
  parcel_lease_bridge %>%
  inner_join(clean_parcels) %>%
  arrange(Lease_Number, desc(OnShale), desc(ParcelAcres)) %>%
  group_by(Lease_Number) %>%
  summarize(OnShale = max(OnShale) == 1,
            ShalePlay = first(ShalePlay),
            ShaleThickness = mean(ShaleThickness, na.rm = T))

parcel_lease_landcover <-
  parcel_lease_bridge %>%
  inner_join(clean_parcels) %>%
  group_by(Lease_Number) %>%
  summarize(across(starts_with("Cover"), ~ mean(.x, na.rm = T)))

parcel_lease_distances <-
  parcel_lease_bridge %>%
  inner_join(clean_parcels) %>%
  group_by(Lease_Number) %>%
  summarize(across(starts_with("Dist"), ~ mean(.x, na.rm = T)))

parcel_lease_grid5 <-
  parcel_lease_bridge %>%
  inner_join(clean_parcels) %>%
  group_by(Lease_Number, Grid5) %>%
  summarize(GridAcres = sum(ParcelAcres)) %>%
  group_by(Lease_Number) %>%
  arrange(Lease_Number, desc(GridAcres)) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  select(Lease_Number, Grid5)

parcel_lease_grid10 <-
  parcel_lease_bridge %>%
  inner_join(clean_parcels) %>%
  group_by(Lease_Number, Grid10) %>%
  summarize(GridAcres = sum(ParcelAcres)) %>%
  group_by(Lease_Number) %>%
  arrange(Lease_Number, desc(GridAcres)) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  select(Lease_Number, Grid10)

parcel_lease_grid20 <-
  parcel_lease_bridge %>%
  inner_join(clean_parcels) %>%
  group_by(Lease_Number, Grid20) %>%
  summarize(GridAcres = sum(ParcelAcres)) %>%
  group_by(Lease_Number) %>%
  arrange(Lease_Number, desc(GridAcres)) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  select(Lease_Number, Grid20)

parcel_lease_county <-
  parcel_lease_bridge %>%
  inner_join(clean_parcels) %>%
  group_by(Lease_Number, County) %>%
  summarize(GridAcres = sum(ParcelAcres)) %>%
  group_by(Lease_Number) %>%
  arrange(Lease_Number, desc(GridAcres)) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  select(Lease_Number, County)

# merge merge the spatial variables into leases
lease_spatial_info <-
  parcel_lease_matching %>%
  left_join(parcel_lease_other, by = "Lease_Number") %>%
  left_join(parcel_lease_shale, by = "Lease_Number") %>%
  left_join(parcel_lease_landcover, by = "Lease_Number") %>%
  left_join(parcel_lease_distances, by = "Lease_Number") %>%  
  left_join(parcel_lease_grid5, by = "Lease_Number") %>%
  left_join(parcel_lease_grid10, by = "Lease_Number") %>%
  left_join(parcel_lease_grid20, by = "Lease_Number") %>%
  left_join(parcel_lease_county, by = "Lease_Number") %>%
  left_join(parcel_lease_latlon_centroid, by = "Lease_Number")

# save to disk
save(lease_spatial_info, file = file.path(gen, "leases_spatial_info.Rda"))

