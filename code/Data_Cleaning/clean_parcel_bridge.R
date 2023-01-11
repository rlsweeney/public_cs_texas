# this reads in the two "parcel bridge" files we have
# - LeaseLandTypes.csv, which we received from GLO sometime in 2017
# - scraped_parcels.Rda, which we scraped from the GLO Scanned Lease File Search
# in July 2020
# by Thom Covert in July 2020

# load packages we need
library(sf)
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

# =============================================================================
# load the data
# 1) LandLeaseFile.csv
# 2) The scraped data (scraped_leases.Rda) 
# 3) the psf map
# =============================================================================
old_llf <-
  file.path(raw_lease, "LeaseLandFile.csv") %>%
  read_csv %>%
  rename(Lease_Number = StateLease, ControlNum = Control)

load(file.path(raw_lease, "lease_parcel_scraping", "scraped_parcels.Rda"))
new_llf <-
  scraped_parcels %>%
  mutate(Lease_Number = paste("MF", lease_number, sep = "")) %>%
  mutate(BasefileNum = str_replace(base_file, "SCH-", "")) %>%
  select(-lease_number, -base_file)

load(file.path(shape, "psf.Rda"))

# =============================================================================
# start by looking for exact BasefileNum matches between new_llf and psf
# try to backfill controlnum's from the PSF in new llf
# =============================================================================
psf_controlnum_basefile <-
  psf %>%
  st_set_geometry(NULL) %>%
  as_tibble %>%
  filter(ControlNum != "") %>%
  filter(ControlNum != " ") %>%
  filter(BasefileNum != " ") %>%
  select(ControlNum, BasefileNum) %>%
  distinct

psf_controlnum_basefile_single <-
  psf_controlnum_basefile %>%
  arrange(BasefileNum) %>%
  group_by(BasefileNum) %>%
  filter(n() == 1) %>%
  ungroup

psf_controlnum_basefile_multi <-
  psf_controlnum_basefile %>%
  arrange(BasefileNum) %>%
  group_by(BasefileNum) %>%
  filter(n() > 1) %>%
  ungroup

new_llf_basefile_match_single <-
  new_llf %>%
  inner_join(psf_controlnum_basefile_single, by = "BasefileNum")

new_llf_basefile_match_multi <-
  new_llf %>%
  inner_join(psf_controlnum_basefile_multi, by = "BasefileNum")

new_llf_no_basefile_match <-
  new_llf %>%
  anti_join(new_llf_basefile_match_single) %>%
  anti_join(new_llf_basefile_match_multi)

# figure out how many of the no matche leases-basefiles are on leases in one of
# the matched files
unmatched_single <-
  new_llf_no_basefile_match %>%
  select(Lease_Number) %>%
  distinct %>%
  inner_join(new_llf_basefile_match_single) %>%
  group_by(Lease_Number) %>%
  summarize(NParcelsSingle = length(unique(ControlNum)))

unmatched_multi <-
  new_llf_no_basefile_match %>%
  select(Lease_Number) %>%
  distinct %>%
  inner_join(new_llf_basefile_match_multi) %>%
  group_by(Lease_Number) %>%
  summarize(NParcelsMulti = length(unique(ControlNum)))

new_llf_no_basefile_match <-
  new_llf_no_basefile_match %>%
  left_join(unmatched_single, by = "Lease_Number") %>%
  left_join(unmatched_multi, by = "Lease_Number") %>%
  replace_na(list(NParcelsSingle = 0, NParcelsMulti = 0))

# most of these are just unmatched: ~700 out of ~18k leases have 1 or more
# matches in the single or multi matches

# =============================================================================
# collapse matches to Lease_Number-ControlNum level and flag the multi-matches
# =============================================================================
new_llf_matched0 <-
  new_llf_basefile_match_single %>%
  select(Lease_Number, ControlNum) %>%
  distinct %>%
  mutate(MultiMatch = FALSE)

new_llf_matched1 <-
  new_llf_basefile_match_multi %>%
  select(Lease_Number, ControlNum) %>%
  distinct %>%
  mutate(MultiMatch = TRUE)

new_llf_matched <-
  bind_rows(new_llf_matched0, new_llf_matched1)

# =============================================================================
# compare new_matched and old
# =============================================================================
compare_llfs <-
  new_llf_matched %>%
  full_join(mutate(select(old_llf, Lease_Number, ControlNum), InOld = TRUE)) %>%
  mutate(InNew = !is.na(MultiMatch),
         InOld = if_else(is.na(InOld), FALSE, TRUE))

# about 14k Leases
in_both_same <-
  compare_llfs %>%
  group_by(Lease_Number) %>%
  filter(sum(InOld) == sum(InNew))

# about 800 Leases
in_both_diff <-
  compare_llfs %>%
  group_by(Lease_Number) %>%
  filter(sum(InOld) > 0, sum(InNew) > 0, sum(InOld) != sum(InNew))

# about 1800 Leases
new_only <-
  compare_llfs %>%
  group_by(Lease_Number) %>%
  filter(sum(InOld) == 0)

# about 17k leases, but only 500 Leases with at least one control num in the
# 04/07/08/15 set
old_only <-
  compare_llfs %>%
  group_by(Lease_Number) %>%
  filter(sum(InNew) == 0)

# =============================================================================
# construct the parcel lease bridge
# this is the entries from the original lease land file for Lease_Numbers that
# it covers, and entries from the scraped data for Lease_Numbers that it does
# not cover
# =============================================================================
new_llf_no_old_leases <-
  new_llf_matched %>%
  anti_join(old_llf, by = "Lease_Number") %>%
  select(-MultiMatch)

parcel_lease_bridge <-
  old_llf %>%
  select(Lease_Number, ControlNum) %>%
  bind_rows(new_llf_no_old_leases)


# =============================================================================
# now bring in lease and parcel shapes, so that we can
# (a) flag parcel-lease bridge entries that seem spatially implausible
# (b) create new parcel-lease bridge entriees that seem spatially plausible
# =============================================================================
active <-
  file.path(raw_lease, "June2020/OAG_Leases_Active.shp") %>%
  read_sf %>%
  mutate(Lease_Source = "ACTIVE")

inactive <-
  file.path(raw_lease, "June2020/OAG_Leases_Inactive.shp") %>%
  read_sf %>%
  mutate(Lease_Source = "INACTIVE")

leases <-
  rbind(active, inactive) %>%
  st_transform(main_crs) %>%
  mutate(LeasePolygonAcres = as.numeric(st_area(.)) * m2_to_acre) %>%
  select(Lease_Number = LEASE_NUMB, Lease_Source, LeasePolygonAcres,
         Original_Gross = ORIGINAL_G, Lease_Status_Date = LEASE_ST_1) %>%
  filter(!is.na(Lease_Number)) %>%
  mutate(AcreageMatch = abs(Original_Gross/LeasePolygonAcres - 1)) %>%
  arrange(Lease_Number, Lease_Source, Lease_Status_Date, AcreageMatch) %>%
  group_by(Lease_Number) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  select(-AcreageMatch) %>%
  select(Lease_Number, LeasePolygonAcres) %>%
  st_make_valid

parcels <-
  psf %>%
  filter(ControlNum != "") %>%
  filter(ControlNum != " ") %>%
  group_by(ControlNum) %>%
  summarize(ParcelAcres = sum(Polygon_Acres), do_union = FALSE) %>%
  st_make_valid
         
lease_parcel_intersection <-
  st_intersection(leases, parcels) %>%
  mutate(IntersectedArea = as.numeric(st_area(.)) * m2_to_acre,
         IntersectedPct = pmax(IntersectedArea / LeasePolygonAcres,
                               IntersectedArea / ParcelAcres))

intersection_flags <-
  lease_parcel_intersection %>%
  select(Lease_Number, ControlNum, IntersectedPct) %>%
  st_set_geometry(NULL)

# flag bridge entries which have no intersection and are more than 1 mile
# apart
non_intersecting_bridge <-
  parcel_lease_bridge %>%
  left_join(intersection_flags) %>%
  filter(is.na(IntersectedPct)) %>%
  arrange(Lease_Number, ControlNum)

nib_leases <-
  left_join(non_intersecting_bridge, leases) %>%
  st_as_sf

nib_parcels <-
  left_join(non_intersecting_bridge, parcels) %>%
  st_as_sf

nib_distances <-
  st_distance(nib_leases, nib_parcels, by_element = TRUE)

non_intersecting_bridge <-
  non_intersecting_bridge %>%
  bind_cols(Distance = as.numeric(nib_distances) / mi_to_m2) %>%
  filter(Distance > 1) %>%
  mutate(BridgeType = "REMOVED") %>%
  select(Lease_Number, ControlNum, BridgeType)

# flag non-bridge intersections which could be added
non_bridge_intersections <-
  lease_parcel_intersection %>%
  filter(IntersectedPct >= 0.5) %>%
  anti_join(parcel_lease_bridge, by = c("Lease_Number", "ControlNum")) %>%
  mutate(BridgeType = "ADDED") %>%
  st_set_geometry(NULL) %>%
  select(Lease_Number, ControlNum, BridgeType)

# combine these, and drop "ADDED" bridge entries when ratio of
# LeasePolygonAcres to sum of "ORIGINAL" parcel acres is lower than 1.1
parcel_lease_bridge <-
  parcel_lease_bridge %>%
  left_join(non_intersecting_bridge) %>%
  bind_rows(non_bridge_intersections) %>%
  replace_na(list(BridgeType = "ORIGINAL")) %>%
  left_join(select(st_set_geometry(leases, NULL),
                   Lease_Number, LeasePolygonAcres)) %>%
  left_join(st_set_geometry(parcels, NULL)) %>%
  group_by(Lease_Number) %>%
  mutate(TotalParcelAcresInLease =
           sum(ParcelAcres * (BridgeType %in% c("ORIGINAL", "REMOVED")),
               na.rm = TRUE)) %>%
  ungroup %>%
  mutate(BridgeType = case_when(
           TotalParcelAcresInLease == 0 | is.na(LeasePolygonAcres) ~ BridgeType,
           BridgeType == "ADDED" &
           LeasePolygonAcres / TotalParcelAcresInLease <= 1.1 ~ "NotADDED",
           TRUE ~ BridgeType))

# drop the NotADDED types, and columns we don't need here
parcel_lease_bridge <-
  parcel_lease_bridge %>%
  filter(BridgeType != "NotADDED") %>%
  select(-LeasePolygonAcres, -ParcelAcres, -TotalParcelAcresInLease)

# =============================================================================
# save to disk
# =============================================================================
save(parcel_lease_bridge, file = file.path(gen, "parcel_lease_bridge.Rda"))
