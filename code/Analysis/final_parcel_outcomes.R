#===============================================================================
# final_parcel_outcomes.R
# - this file loads and selects leases, loads and selects parcels, then
# constructs outcomes for those parcels on these restricted leases
# - collapses to the parcel month level, and discounts to the parcel level 
# - this file relies on running `make_lease_parcels_monthly.R` first
#===============================================================================

# TEXAS SETUP ------------------------------------------------------------------
# load packages we need
library(tidyverse)
library(lubridate)
library(sf)
library(here)

root <- here()
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "texas_constants.R"))

# LOAD DATA  --------------------------------------------------------------
load(file.path(gen, "clean_parcels.Rda"))
load(file.path(gen, "clean_leases.Rda"))
load(file.path(gen, "lease_parcels_monthly_outcomes.Rda"))
load(file.path(gen, "lease_orders.Rda"))

# drop geometry from parcels 
clean_parcels <-
  clean_parcels %>%
  st_set_geometry(NULL)

# merge in lease order variable, note the replace_na step there
clean_leases <-
  clean_leases %>%
  left_join(select(lease_orders, Lease_Number, AvgOrder)) %>%
  replace_na(list(AvgOrder = 1)) %>%
  mutate(AvgOrder = round(AvgOrder)) %>%
  mutate(AvgOrder = pmin(4, AvgOrder))

#===============================================================================
# DEFINE GET_PARCELOUTCOMES
# Single FUNCTION TO GET LEASES AND COMPARABLE PARCEL DISCOUNTED REVS
# this generates *LEASE* and *PARCEL* level discounted outcomes, for
# outcomes that occur between RevStartDatet and RevEndDate, on
# leases effective between LeaseStartDate and LeaseEndDate, on *ALL* parcels

# notes on inputs:
# RevStartDate: if for some reason you want to include old leases but start
# revs at some later date
# f_lease_info, f_parcel_info: here's where you would impose restrictions on
# which leases and or parcels are included
# NLeaseCutoff: if you wanted to restrict to, e.g., first lease
#===============================================================================
get_parceloutcomes <-
  function(LeaseStartDate = SampleLeaseStart,
           RevStartDate = LeaseStartDate, 
           LeaseEndDate = CensorDate,
           RevEndDate = LastProductionDate,
           discountRate = modiscount,
           discountDate = LeaseStartDate,
           f_lease_parcels_monthly = lease_parcels_monthly_outcomes,
           f_lease_info = filter(clean_leases, OnShale),
           f_parcel_info = filter(clean_parcels, OnShale),
           NLeaseCutoff = 1000) {
  
  # create a flag for if a parcel had active lease activity as of start date
  early_activity <-
    f_lease_parcels_monthly %>%
    filter(Date == RevStartDate, Effective_Date <= RevStartDate) %>%
    select(ControlNum) %>%
    distinct %>%
    mutate(ActiveRevStartDate = 1) %>%
    right_join(select(f_parcel_info, ControlNum), by = "ControlNum") %>%
    mutate(ActiveRevStartDate = if_else(is.na(ActiveRevStartDate), 0, 1)) 

  filtered_leases <-
    f_lease_info %>%
    filter(Effective_Date >= LeaseStartDate, Effective_Date <= LeaseEndDate)

  firstN <-
    f_lease_parcels_monthly %>%
    select(ControlNum, Lease_Number, ParcelLeaseAcres, Effective_Date) %>%
    distinct %>%
    semi_join(filtered_leases, by = "Lease_Number") %>%
    arrange(ControlNum, Effective_Date, -ParcelLeaseAcres) %>%
    group_by(ControlNum) %>%
    mutate(LeaseN = row_number()) %>%
    ungroup %>%
    filter(LeaseN <= NLeaseCutoff) %>%
    select(ControlNum, Lease_Number)

  filtered_lease_parcels_monthly <-
    f_lease_parcels_monthly %>%
    filter(Date >= RevStartDate, Date <= RevEndDate) %>%
    semi_join(firstN, by = c("ControlNum", "Lease_Number")) %>%
    left_join(select(filtered_leases, Lease_Number, AvgOrder, Auction),
              by = "Lease_Number") %>%
    mutate(LeaseWeight = ParcelShare * GrossAcres / ParcelAcres)

  all_dates <-
    filtered_lease_parcels_monthly %>%
    select(Date) %>%
    distinct %>%
    arrange(Date) %>%
    pluck("Date")
  
  all_parcels <-
    f_parcel_info %>%
    select(ControlNum) %>%
    distinct %>%
    arrange(ControlNum) %>%
    pluck("ControlNum")

  all_parcel_dates <- crossing(ControlNum = all_parcels, Date = all_dates)
  
  # get monthly parcel outcomes 
  parcel_month_outcomes <-
    filtered_lease_parcels_monthly %>%
    arrange(ControlNum, Date) %>%
    group_by(ControlNum, Date) %>%
    summarize(BOE_oil = sum(BOE_oil),
              BOE_gas = sum(BOE_gas),
              BOE_total = sum(BOE_total),
              PmtBonus = sum(PmtBonus),
              PmtRoyalty = sum(PmtRoyalty),
              PmtOther = sum(PmtOther),
              PmtFee = sum(PmtFee),
              PmtTotal = sum(PmtTotal),
              LeaseRevenue = sum(LeaseRevenue),
              AcresLeased = sum(ParcelLeaseAcres),
              NLeases = n_distinct(Lease_Number),
              TotalActive = sum(LeaseActive)) %>%
    ungroup %>%
    mutate(NLeases = as.numeric(NLeases),
           TotalActive = as.numeric(TotalActive)) %>%
    right_join(all_parcel_dates, by = c("ControlNum", "Date")) %>%
    mutate(across(c(contains("BOE"), contains("Pmt"), 
                    "AcresLeased", "NLeases", "TotalActive", "LeaseRevenue"),
                  ~ if_else(is.na(.x), 0, .x))) 
  
  # GET DISCOUNTED PARCEL AND LEASE OUTCOMES 
  discounted_lease_parcels_monthly <- 
    filtered_lease_parcels_monthly %>%
    mutate(delay = interval(discountDate, Date) %/% months(1),
           discount_factor = 1 / ((1 + discountRate) ^ delay)) %>%
    mutate(BOE_oil = discount_factor * BOE_oil,
           BOE_gas = discount_factor * BOE_gas,
           BOE_total = discount_factor * BOE_total,
           PmtBonus = discount_factor * PmtBonus,
           PmtRoyalty = discount_factor * PmtRoyalty,              
           PmtOther = discount_factor * PmtOther,
           PmtFee = discount_factor * PmtFee,
           PmtTotal = discount_factor * PmtTotal,
           LeaseRevenue = discount_factor * LeaseRevenue,
           AcresLeased = discount_factor*ParcelLeaseAcres) 
  
  f_months <- interval(RevStartDate, RevEndDate) %/% months(1)
  
  # get parcel_outcomes: first compute basic static facts about the parcel
  # like how many lesaes, how many are first vs second.  then also compute
  # when the parcel first saw leases or royalties
  n_parcel_leases <- 
    filtered_lease_parcels_monthly %>%
    select(ControlNum, Lease_Number, AvgOrder, Auction, LeaseWeight) %>%
    distinct %>%
    group_by(ControlNum) %>%
    summarise(NLeases = n_distinct(Lease_Number),
              TotalLeaseWeight = sum(LeaseWeight),
              NAuctions = sum(Auction),
              TotalAuctionWeight = sum(Auction * LeaseWeight),
              FirstLeases = sum(AvgOrder == 1),
              SecondLeases = sum(AvgOrder == 2),
              ThirdLeases = sum(AvgOrder == 3),
              FourthLeases = sum(AvgOrder == 4)) %>%
    ungroup

  parcel_leasing_royalty_dates <-
    filtered_lease_parcels_monthly %>%
    group_by(ControlNum) %>%
    summarize(minLeaseDate = min(Date),
              minRoyaltyDate = min(Date[PmtRoyalty > 0])) %>%
    ungroup
  
  parcel_outcomes <-
    discounted_lease_parcels_monthly %>%
    group_by(ControlNum) %>%
    summarize(BOE_oil = sum(BOE_oil),
              BOE_gas = sum(BOE_gas),
              BOE_total = sum(BOE_total),
              PmtBonus = sum(PmtBonus),
              PmtRoyalty = sum(PmtRoyalty),
              PmtOther = sum(PmtOther),
              PmtFee = sum(PmtFee),
              PmtTotal = sum(PmtTotal),
              LeaseRevenue = sum(LeaseRevenue),
              AcresLeased = sum(AcresLeased),
              MonthsActive = n_distinct(Date)) %>%
    ungroup %>% 
    right_join(f_parcel_info, by = "ControlNum") %>%
    left_join(n_parcel_leases, by = "ControlNum") %>%
    left_join(parcel_leasing_royalty_dates, by = "ControlNum") %>%    
    mutate(AvgAcresLeased = AcresLeased/f_months,
           MonthsActive = as.numeric(MonthsActive),
           NLeases = as.numeric(NLeases),
           TotalLeaseWeight = as.numeric(TotalLeaseWeight),           
           NAuctions = as.numeric(NAuctions),
           TotalAuctionWeight = as.numeric(TotalAuctionWeight),           
           across(c(FirstLeases, SecondLeases, ThirdLeases, FourthLeases),
                  as.numeric)) %>%
    select(-AcresLeased) %>%
    mutate(across(c(AvgAcresLeased, NLeases, TotalLeaseWeight, NAuctions,
                    TotalAuctionWeight, MonthsActive, 
                    FirstLeases, SecondLeases, ThirdLeases, FourthLeases),
                  ~ if_else(is.na(.x), 0, .x))) %>%
    mutate(across(c(contains("BOE"), contains("Pmt"),"LeaseRevenue"),
                  ~ if_else(is.na(.x), 0, .x))) %>% 
    # cap small outcomes at zero (this is surely measurement error)
    mutate(across(c(contains("BOE"), contains("Pmt"),"LeaseRevenue"),
                  ~ if_else(.x < 0, 0, .x))) %>% 
    mutate(Drilled = if_else(BOE_total > 0, 1, 0),
           MonthsActivePct = MonthsActive/f_months,
           EverLeased = NLeases > 0) %>%
    left_join(early_activity, by = "ControlNum")

  lease_outcomes <-
    discounted_lease_parcels_monthly %>%
    group_by(Lease_Number) %>%
    summarize(BOE_oil = sum(BOE_oil),
              BOE_gas = sum(BOE_gas),
              BOE_total = sum(BOE_total),
              PmtBonus = sum(PmtBonus),
              PmtRoyalty = sum(PmtRoyalty),              
              PmtOther = sum(PmtOther),
              PmtFee = sum(PmtFee),
              PmtTotal = sum(PmtTotal),
              LeaseRevenue = sum(LeaseRevenue),
              MonthsActive = n_distinct(Date),
              MonthsLeaseActive = sum(LeaseActive),
              ParcelLeaseAcres = sum(ParcelLeaseAcres)) %>% # placeholder 
    mutate(MonthsActive = as.numeric(MonthsActive),
           MonthsLeaseActive = as.numeric(MonthsLeaseActive)) %>%
    right_join(filtered_leases, by = "Lease_Number") %>%
    mutate(across(c(contains("BOE"), contains("Pmt"),"LeaseRevenue",
                    MonthsActive, MonthsLeaseActive),
                  ~ if_else(is.na(.x), 0, .x))) %>% 
    # cap small outcomes at zero (this is surely measurement error)
    mutate(across(c(contains("BOE"), contains("Pmt"),"LeaseRevenue"),
                  ~ if_else(.x < 0, 0, .x))) %>% 
    mutate(Drilled = if_else(BOE_total > 0, 1, 0),
           MonthsActivePct = MonthsActive/f_months)
  
  # finally, make a parcel-lease-outcomes file
  parcel_lease_outcomes <-
    discounted_lease_parcels_monthly %>%
    group_by(ControlNum, Lease_Number, ParcelAcres, GrossAcres, ParcelShare) %>%
    summarize(BOE_oil = sum(BOE_oil),
              BOE_gas = sum(BOE_gas),
              BOE_total = sum(BOE_total),
              PmtBonus = sum(PmtBonus),
              PmtRoyalty = sum(PmtRoyalty),              
              PmtOther = sum(PmtOther),
              PmtFee = sum(PmtFee),
              PmtTotal = sum(PmtTotal),
              LeaseRevenue = sum(LeaseRevenue),
              MonthsActive = n_distinct(Date)) %>% # placeholder 
    mutate(MonthsActive = as.numeric(MonthsActive)) %>%
    mutate(across(c(contains("BOE"), contains("Pmt"),"LeaseRevenue",
                    MonthsActive),
                  ~ if_else(is.na(.x), 0, .x))) %>% 
    # cap small outcomes at zero (this is surely measurement error)
    mutate(across(c(contains("BOE"), contains("Pmt"),"LeaseRevenue"),
                  ~ if_else(.x < 0, 0, .x))) %>%
    ungroup
  
  return(list(parcel_month_outcomes = parcel_month_outcomes,
              parcel_outcomes = parcel_outcomes,
              lease_outcomes = lease_outcomes,
              parcel_lease_outcomes = parcel_lease_outcomes))
}

#===============================================================================
# GET OUR PREFERRED PARCEL OUTCOMES
# - lease onshale status is defined as true if atleast one parcel is onshale
#     - so all leases associated with onshale parcels are onshale
#     - imposing that restriction here in both sets makes this much faster 
# - not imposing any land type restrictions here. 
#===============================================================================
parcel_outcomes_main <-
  get_parceloutcomes(LeaseStartDate = SampleLeaseStart,
                     RevStartDate = SampleLeaseStart, 
                     LeaseEndDate = CensorDate,
                     RevEndDate = LastProductionDate,
                     discountRate = modiscount,
                     discountDate = SampleLeaseStart,
                     f_lease_info = filter(clean_leases, OnShale),
                     f_parcel_info = filter(clean_parcels, OnShale),
                     NLeaseCutoff = 1000) 

save(parcel_outcomes_main,
     file = file.path(gen, "final_parcel_outcomes.Rda"))

#===============================================================================
# Include early leases 
# - useful for making the monthly plots
# - also checking if this left censoring matters
#===============================================================================
outcomes_earlyLeases <-
  get_parceloutcomes(LeaseStartDate = "1900-01-01",
                     RevStartDate = SampleLeaseStart, 
                     LeaseEndDate = SampleLeaseEnd,
                     RevEndDate = LastProductionDate,
                     discountRate = modiscount,
                     discountDate = SampleLeaseStart,
                     f_lease_info = filter(clean_leases, OnShale),
                     f_parcel_info = filter(clean_parcels, OnShale))

save(outcomes_earlyLeases ,
     file = file.path(gen, "outcomes_earlyLeases.Rda"))

#===============================================================================
# Include early leases AND compute going back to 1/1/2001
# - useful for making the monthly plots
# - also checking if this left censoring matters
#===============================================================================
outcomes_earlyLeases_earlydates <-
  get_parceloutcomes(LeaseStartDate = "1900-01-01",
                     RevStartDate = "2001-01-01", 
                     LeaseEndDate = SampleLeaseEnd,
                     RevEndDate = LastProductionDate,
                     discountRate = modiscount,
                     discountDate = SampleLeaseStart,
                     f_lease_info = filter(clean_leases, OnShale),
                     f_parcel_info = filter(clean_parcels, OnShale))

save(outcomes_earlyLeases_earlydates,
     file = file.path(gen, "outcomes_earlyLeases_earlydates.Rda"))
