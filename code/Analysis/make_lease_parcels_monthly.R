# make_lease_parcels_monthly.R
# - this file loads clean leases, parcels and monthly outcomes
# - it allocates leases to parcls using the parcel bridge

# SETUP ------------------------------------------------------------------------
# load packages we need
library(tidyverse)
library(lubridate)
library(sf)
library(here)

root <- here()
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "texas_constants.R"))

# LOAD DATA  
load(file.path(gen, "clean_parcels.Rda"))
load(file.path(gen, "clean_leases.Rda"))
load(file.path(gen, "clean_lease_outcomes_monthly.Rda"))

# drop geometry from parcels 
clean_parcels <-
  clean_parcels %>%
  st_set_geometry(NULL)

load(file.path(gen, "parcel_lease_bridge.Rda"))

parcel_lease_bridge <- 
  parcel_lease_bridge %>%
  filter(BridgeType != "REMOVED") 

## PREPARE PARCELS =============================================================
# get the share of each parcel in each lease 
lease_parcel_map <- 
  parcel_lease_bridge %>%
  inner_join(select(clean_leases, Lease_Number,
                    GrossAcres, TotalParcelAcresInLease)) %>%
  inner_join(select(clean_parcels, ControlNum, ParcelAcres)) %>%
  mutate(ParcelShare = ParcelAcres / TotalParcelAcresInLease,
         Prorated_ParcelShare = if_else(GrossAcres > TotalParcelAcresInLease,
                                        ParcelAcres / GrossAcres,
                                        ParcelShare)) %>%
  ungroup %>%
  select(-ParcelShare) %>%
  rename(ParcelShare = Prorated_ParcelShare)

# merge the monthly lease outcomes with parcels.  
## filter step is necessary because lease_outcomes_monthly may expand
## through past last production date
## for about 25 leases, we have missing effective date information, and so
## they currently reflect a sentinel value: 2099-12-31.  replace these dates
## with the earliest payment date
leases_monthly <-
  clean_lease_outcomes_monthly %>%
  rename(Date = PaymentDate) %>%
  filter(Date <= LastProductionDate) %>%
  mutate(LeaseActive = Date <= MaxActiveDate) %>%
  group_by(Lease_Number) %>%
  mutate(MinPaymentDate = min(Date)) %>%
  ungroup %>%
  mutate(Effective_Date = if_else(Effective_Date == make_date(2099, 12, 31),
                                  MinPaymentDate, Effective_Date)) %>%
  select(-MinPaymentDate)

lease_parcels_monthly <-
  lease_parcel_map %>%
  inner_join(leases_monthly, by = "Lease_Number")

## COMPUTE LEASE ORDER =========================================================
lease_generations <-
  lease_parcels_monthly %>%
  filter(LeaseActive) %>%
  group_by(Lease_Number) %>%
  summarize(MinActiveDate = min(Date))

lease_generations <-
  lease_parcels_monthly %>%
  left_join(lease_generations) %>%
  mutate(MinActiveDate = if_else(is.na(MinActiveDate),
                                 MinDate,
                                 MinActiveDate)) %>%
  mutate(ActiveAcres =
           (Date == MinActiveDate) * ParcelShare * GrossAcres) %>%
  arrange(ControlNum, Date) %>%
  group_by(ControlNum, ParcelAcres, Date) %>%
  summarize(ActiveAcres = sum(ActiveAcres)) %>%
  group_by(ControlNum, ParcelAcres) %>%
  mutate(CumulativeAcres = cumsum(ActiveAcres)) %>%
  ungroup %>%
  mutate(LeaseGen = ceiling(CumulativeAcres / ParcelAcres)) %>%
  select(ControlNum, Date, LeaseGen) %>%
  distinct

# rescale these values as consecutive integers from 1 to ... N
rescaled_lease_generations <-
  lease_generations %>%
  select(ControlNum, LeaseGen) %>%
  distinct %>%
  arrange(ControlNum, LeaseGen) %>%
  group_by(ControlNum) %>%
  mutate(RescaledLeaseGen = row_number()) %>%
  ungroup

lease_generations <-
  lease_generations %>%
  left_join(rescaled_lease_generations) %>%
  select(-LeaseGen) %>%
  rename(LeaseGen = RescaledLeaseGen)

take_largest <- function(x, y) {
  return(x[which.max(y)])
}

lease_orders <-
  lease_parcels_monthly %>%
  left_join(lease_generations) %>%
  group_by(ControlNum, Lease_Number) %>%
  mutate(LeaseOrderInParcel = min(LeaseGen)) %>%
  ungroup %>%
  select(Lease_Number, ControlNum, ParcelShare, LeaseOrderInParcel) %>%
  distinct %>%
  group_by(Lease_Number) %>%
  summarize(NParcels = length(unique(ControlNum)),
            MinOrder = min(LeaseOrderInParcel),
            MaxOrder = max(LeaseOrderInParcel),
            LargestParcelShareOrder = take_largest(LeaseOrderInParcel,
                                                   ParcelShare),
            AvgOrder = weighted.mean(LeaseOrderInParcel, ParcelShare))


## DEFINE LEASE PARCEL MONTHLY OUTCOMES BASED ON EACH OF THESE SHARES ==========
outcomes <-
  lease_parcels_monthly %>%
  select(Lease_Number, ControlNum, Date, ParcelShare, LeaseRevenue,
         contains("BOE"), contains("Pmt")) %>%
  mutate(across(c(LeaseRevenue, contains("BOE"),
                  contains("Pmt")), ~ .x * ParcelShare)) %>%
  select(-ParcelShare)

## SAVE OUTCOMES AT THE LEASE-PARCEL-MONTH LEVEL ===============================
lease_parcels_monthly_outcomes <-
  lease_parcels_monthly %>%
  select(-contains("BOE"), -contains("Pmt"), -LeaseRevenue) %>%
  left_join(outcomes, by = c("Lease_Number", "ControlNum", "Date")) %>%
  mutate(ParcelLeaseAcres = ParcelShare * GrossAcres)

save(lease_parcels_monthly_outcomes,
    file = file.path(gen, "lease_parcels_monthly_outcomes.Rda"))

# also save lease order calculation
save(lease_orders, file = file.path(gen, "lease_orders.Rda"))








