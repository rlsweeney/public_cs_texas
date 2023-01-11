## load the final input data for analysis and save a definitive sample

#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
library(here)
root <- here()

library(tidyverse)
library(sf)
library(lubridate)

source(file.path(root, "code/paths.R"))
source(file.path(root, "code/functions/utils.R"))
source(file.path(root, "code/texas_constants.R"))

#===========================================================================
# PREPARE AUXILIARY INFORMATION AND OUTCOMES FIRST
# coversheet info
# RAL addenda
# lease multi-polygon flags
# discounted outcomes
#===========================================================================
# load coversheets so we can have a dummy variable for whether GLO improved
# on an RAL surface owner's negotiation
load(file.path(gen, "coversheets.Rda"))

ral_improved <-
  coversheets %>%
  filter(is.na(No_Review), is.na(No_Pdf)) %>%
  filter(BPA_Offered > 0, Royalty_Offered > 0, Terms_Offered > 0) %>%
  mutate(Royalty_Offered = if_else(Royalty_Offered > 1,
                                   Royalty_Offered / 100,
                                   Royalty_Offered)) %>%
  mutate(ImprovedBonus = BPA_Offered < BPA_Rec,
         ImprovedRoyalty = Royalty_Offered < Royalty_Rec,
         ImprovedTerms = Terms_Offered > Terms_Rec) %>%
  mutate(across(contains("Improved"), ~ if_else(is.na(.x), FALSE, .x))) %>%
  mutate(Improved = ImprovedBonus | ImprovedRoyalty | ImprovedTerms) %>%
  select(Lease_Number, contains("Improved")) %>%
  mutate(Lease_Number = paste0("MF", Lease_Number), HasImproved = TRUE)

# load RAL addenda
ral_addenda <-
  file.path(raw, "addenda.csv") %>%
  read_csv %>%
  mutate(Lease_Number = paste("MF", mf_n, sep = ""), HasAddenda = TRUE) %>%
  select(Lease_Number,
         c01:c16,
         AddendaPages = pages_rounded,
         AddendaClauses = clauses_rounded)

# load lease shape data and use it to flag presence of multi-polygon leases
load(file.path(gen, "clean_leases_shapes.Rda"))

leases_multipolygon <-
  clean_leases_shapes %>%
  st_cast("POLYGON") %>%
  st_set_geometry(NULL) %>%
  group_by(Lease_Number) %>%
  summarize(MultiPolygon = (n() > 1) * 1)

# lump sum the bonus payments up to effective date, do the same for
# the royalties and other payments, and flag leases that have payments which
# preceed effective date
load(file.path(gen, "clean_lease_outcomes_monthly.Rda"))
load(file.path(gen, "glo_royalties_lifo_flag.Rda"))

# merge in flags indicating whether the output information was subject to the
# LIFO algorithm for cleaning up unmatched reconcilliation paymeents
lifo_flags <-
  glo_royalties_lifo_flag %>%
  pivot_wider(Lease_Number,
              names_from = Commodity,
              values_from = LIFO,
              values_fill = FALSE) %>%
  rename(LIFOOil = Oil, LIFOGas = Gas)

pre_effective_lump_sum_outcomes <-
  clean_lease_outcomes_monthly %>%
  filter(PreEffectiveDate) %>%
  group_by(Lease_Number) %>%
  summarize(across(c(contains("BOE"), contains("Pmt"), LeaseRevenue), sum)) %>%
  mutate(HasEarlyBonus = PmtBonus != 0,
         HasEarlyRoyalty = (PmtGasRoyalty != 0) | (PmtOilRoyalty != 0),
         HasEarlyOther = PmtOther != 0,
         HasEarlyFee = PmtFee != 0) %>%
  select(Lease_Number, starts_with("Has"), PmtBonus, PmtRoyalty, PmtTotal,
         BOE_total, LeaseRevenue)

discounted_outcomes <-
  clean_lease_outcomes_monthly %>%
  filter(!PreEffectiveDate) %>%
  mutate(delay = interval(Effective_Date, PaymentDate) %/% months(1),
         discount_factor = 1 / ((1 + modiscount) ^ delay)) %>%
  mutate(across(c(contains("BOE"), contains("Pmt"), LeaseRevenue),
                ~ .x * discount_factor)) %>%
  group_by(Lease_Number) %>%
  summarize(across(c(PmtBonus, PmtRoyalty, PmtTotal,
                     BOE_total, LeaseRevenue), sum)) %>%
  left_join(select(pre_effective_lump_sum_outcomes, Lease_Number,
                   contains("HasEarly")),
            by = "Lease_Number") %>%
  replace_na(list(HasEarlyBonus = FALSE,
                  HasEarlyRoyalty = FALSE,
                  HasEarlyOther = FALSE,
                  HasEarlyFee = FALSE)) %>%
  bind_rows(pre_effective_lump_sum_outcomes) %>%
  mutate(across(c(PmtRoyalty, BOE_total, LeaseRevenue), ~ pmax(0, .x))) %>%
  group_by(Lease_Number, HasEarlyBonus, HasEarlyRoyalty,
           HasEarlyOther, HasEarlyFee) %>%
  summarize(across(c(PmtBonus, PmtRoyalty, PmtTotal,
                     BOE_total, LeaseRevenue), sum)) %>%
  ungroup %>%
  left_join(lifo_flags) %>%
  replace_na(list(LIFOOil = FALSE, LIFOGas = FALSE))

peak_time_to_drill <-
  clean_lease_outcomes_monthly %>%
  filter(!PreEffectiveDate) %>%
  filter(BOE_total > 0) %>%
  arrange(Lease_Number, PaymentDate) %>%
  group_by(Lease_Number) %>%
  summarize(FirstPaymentDate = min(PaymentDate),
            PeakBOE_total = max(BOE_total),
            PeakLeaseRevenue = max(LeaseRevenue))

#===========================================================================
# load in clean_leases
# merge this stuff in
# generate needed regression covariates
# define InSample
#===========================================================================
load(file.path(gen, "clean_leases.Rda"))

leases <-
  clean_leases %>%
  left_join(leases_multipolygon, by = "Lease_Number") %>%
  left_join(ral_improved, by = "Lease_Number") %>%
  left_join(ral_addenda, by = "Lease_Number") %>%
  left_join(discounted_outcomes, by = "Lease_Number") %>%
  left_join(peak_time_to_drill, by = "Lease_Number") %>%
  replace_na(list(MultiPolygon = 0,
                  ImprovedBonus = FALSE,
                  ImprovedRoyalty = FALSE,
                  ImprovedTerms = FALSE,
                  Improved = FALSE,
                  HasImproved = FALSE,
                  HasAddenda = FALSE,
                  c01 = 0,
                  c02 = 0,
                  c03 = 0,
                  c04 = 0,
                  c05 = 0,
                  c06 = 0,
                  c07 = 0,
                  c08 = 0,
                  c09 = 0,
                  c10 = 0,
                  c11 = 0,
                  c12 = 0,
                  c13 = 0,
                  c14 = 0,
                  c15 = 0,
                  c16 = 0,
                  AddendaPages = 0,
                  AddendaClauses = 0,
                  LIFOOil = FALSE,
                  LIFOGas = FALSE,
                  HasEarlyBonus = FALSE,
                  HasEarlyRoyalty = FALSE,
                  HasEarlyOther = FALSE,
                  HasEarlyFee = FALSE,
                  BOE_total = 0,
                  PmtBonus = 0,
                  PmtRoyalty = 0,
                  LeaseRevenue = 0,
                  FirstPaymentDate = make_date(2019, 3, 1),
                  PeakBOE_total = 0,
                  PeakLeaseRevenue = 0)) %>%
  mutate(FirstPaymentDate = pmax(Effective_Date, FirstPaymentDate)) %>%
  mutate(Year = year(Effective_Date),
         Quarter = quarter(Effective_Date),
         YearQtr = paste(Year, Quarter, sep = "-"),
         CountyYr = paste(County, Year, sep = "-"),
         Grid5Yr = paste(Grid5, Year, sep = "-"),
         Grid10Yr = paste(Grid10, Year, sep = "-"),
         Grid20Yr = paste(Grid20, Year, sep = "-"),
         CountyYrQtr = paste(County, Year, Quarter, sep = "-"),
         Grid5YrQtr = paste(Grid5, Year, Quarter, sep = "-"),
         Grid10YrQtr = paste(Grid10, Year, Quarter, sep = "-"),
         Grid20YrQtr = paste(Grid20, Year, Quarter, sep = "-"),
         EffDate = as.numeric(Effective_Date),
         ShapeQuality = LeasePolygonAcres / LeaseHullAcres,
         BonusPerAcre = PmtBonus / GrossAcres,
         RoyaltyRevenuePerAcre = PmtRoyalty / GrossAcres,
         LeaseRevenuePerAcre = LeaseRevenue / GrossAcres,
         DBOEPerAcre = BOE_total / GrossAcres,
         SellerRevenuePerAcre = PmtTotal / GrossAcres,
         Drilled = BOE_total > 0,
         PeakBOEPerAcre = PeakBOE_total / GrossAcres,
         PeakLeaseRevenuePerAcre = PeakLeaseRevenue / GrossAcres,
         TimeToDrill = as.numeric(FirstPaymentDate - Effective_Date)) %>%
  mutate(flag_early = Effective_Date < make_date(2004, 1, 1),
         flag_late = Effective_Date > make_date(2016, 12, 31),
         flag_small = GrossAcres < 10,
         flag_big = GrossAcres > 1000,
         flag_undivided = Undivided == "YES" | InferredUndivided == "YES",
         flag_netgross = NetAcres / GrossAcres < 0.99,
         flag_auction_ral = LeaseType == "RAL" & Auction,
         flag_negotiation_state = LeaseType == "STATE" & !Auction,
         flag_extra_type = HasExtraLeaseType,
         flag_ral_selfdeal = LeaseType == "RAL" & TexasLessor,
         flag_term = Term < 1) %>%
  mutate(InSample =
           ValidLease &
           LeaseType %in% c("RAL", "STATE") &
           OnShale &
           !flag_early &
           !flag_late &
           !flag_small &
           !flag_big &
           !flag_undivided &
           !flag_netgross &
           !flag_auction_ral &
           !flag_negotiation_state &
           !flag_extra_type &
           !flag_ral_selfdeal &
           !flag_term,
         drop_reason = case_when(
           !ValidLease ~ "invalid",
           !(LeaseType %in% c("RAL", "STATE")) ~ "wrongtype",
           !OnShale ~ "wrongplace",
           flag_early | flag_late ~ "timing",
           flag_small | flag_big ~ "size",
           flag_netgross ~ "netgross",
           flag_undivided ~ "undivided",
           flag_term ~ "term",
           flag_negotiation_state ~ "statenegotiate",
           flag_auction_ral ~ "ralauction",
           flag_extra_type | flag_ral_selfdeal ~ "nonstandard",
           TRUE ~ "notdropped"),
         Censored = Year > 2013) %>%
  rename(LesseeAlias = Lessee_alias, AssigneeAlias = Assignee_alias)
         

#===========================================================================
# flag lessee's who we think are brokers
#===========================================================================
# broker_regex and nonbroker_regex both defined in code/functions/utils.R
leases <-
  leases %>%
  mutate(broker = str_detect(LesseeAlias, broker_regex) &
           !str_detect(LesseeAlias, nonbroker_regex))

max2 <- function(v) {
  if(length(v) == 1) {
    v[1]
  } else {
    sort(v, decreasing=T)[2]
  }
}

# tabulate each broker's assignee frequencies with a variety of groupings
probfirms <- function(d, ..., TypeN = 1) {
  group_vars <- quos(...)
  assignees <-
    d %>%
    filter(broker, !is.na(AssigneeAlias)) %>%
    group_by(LesseeAlias, !!!group_vars, AssigneeAlias) %>%
    summarize(n=n()) %>%
    ungroup %>%
    arrange(LesseeAlias, !!! group_vars, desc(n)) %>%
    group_by(LesseeAlias, !!! group_vars) %>%
    mutate(hasmode = (max(n) > max2(n)) | n() == 1) %>%
    filter(row_number() == 1) %>%
    ungroup %>%
    filter(hasmode) %>%
    mutate(ProbFirmType = TypeN) %>%
    select(LesseeAlias, !!! group_vars,
           ProbFirm = AssigneeAlias, ProbFirmType)

  probfirms_d <-
    d %>% 
    inner_join(assignees) %>%
    select(Lease_Number, ProbFirm, ProbFirmType)
  
  return(list(probfirms_d, assignees))
}

probfirms_lease_data <-
  bind_rows(probfirms(leases, County, Year, Auction, TypeN = 1)[[1]],
            probfirms(leases, County, Auction, TypeN = 2)[[1]],
            probfirms(leases, County, Year, TypeN = 3)[[1]],
            probfirms(leases, County, TypeN = 4)[[1]],
            probfirms(leases, Auction, TypeN = 5)[[1]],
            probfirms(leases, TypeN = 6)[[1]]) %>%
  arrange(Lease_Number, ProbFirmType) %>%
  group_by(Lease_Number) %>%
  filter(row_number() == 1) %>%
  ungroup
  
leases <-
  leases %>%
  left_join(probfirms_lease_data) %>%
  mutate(NewFirm = case_when(
           !is.na(AssigneeAlias) ~ AssigneeAlias,
           !broker ~ LesseeAlias,
           !is.na(ProbFirm) ~ ProbFirm,
           TRUE ~ LesseeAlias)) %>%
  mutate(NewFirmType = case_when(
           !is.na(AssigneeAlias) ~ "Assignment",
           !broker ~ "Unassigned Non-Broker Lessee",
           !is.na(ProbFirm) ~ "Broker's Most Probable Assignee",
           TRUE ~ "Broker"))

# update all per acre figures to be in thousands, make auction 0/1 instead of
# TRUE/FALSE and save final_leases
final_leases <-
  leases %>%
  mutate(Acres = GrossAcres / 1000,
         Auction = if_else(Auction, 1, 0),
         ShapeQuality = if_else(is.na(ShapeQuality),
                                ParcelShapeQuality,
                                ShapeQuality))

save(final_leases, file = file.path(gen, "final_leases.Rda"))

# save the ProbFirms maps we compute above
probfirms_map <-
  bind_rows(probfirms(leases, County, Year, Auction, TypeN = 1)[[2]],
            probfirms(leases, County, Auction, TypeN = 2)[[2]],
            probfirms(leases, County, Year, TypeN = 3)[[2]],
            probfirms(leases, County, TypeN = 4)[[2]],
            probfirms(leases, Auction, TypeN = 5)[[2]],
            probfirms(leases, TypeN = 6)[[2]])

save(probfirms_map, file = file.path(gen, "probfirms_map.Rda"))
