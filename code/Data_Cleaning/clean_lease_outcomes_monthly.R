## compute lease outcomes by month
## By Rich Sweeney and Thom Covert in July/August 2020
## further modifications by Thom Covert in December 2020

# load packages we need
library(tidyverse)
library(lubridate)
library(here)

root <- here()
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "texas_constants.R"))
source(file.path(root, "code", "functions", "regtable.R"))

# Load in data =================================================================
load(file.path(gen, "clean_leases.Rda"))
load(file.path(gen, "prices.Rda"))

load(file.path(gen, "glo_royalties.Rda"))
load(file.path(gen, "glo_rentals_bonuses.Rda"))

# ==============================================================================
# helper function to convert a date to first of the month
# ==============================================================================
ym1 <- function(d) {make_date(year(d), month(d), 1)}

# ==============================================================================
# reshape royalty payments in the same format as the bonus/rental stuff
# ==============================================================================
royalties <-
  glo_royalties %>%
  mutate(Type = paste0(Commodity, "Royalty", sep = "")) %>%
  rename(Payment = NetPayment, PaymentDate = ProductionDate) %>%
  select(Lease_Number, PaymentDate, Type, Payment)

#===============================================================================
# clean up the payment type information in the rentals/bonus data, using
# information from clean_leases where appropriate
# to decide what to do about leases with no accounting payment, we consider 3
# cases:
# 1) sum of bonus payments here match the manual bonus in leases_state
# in this case, we'd conclude the missing payment is an "OTHER" payment
# 2) sum of bonus payments + sum of missing payments = manual bonus
# in this case, we'd conclude the missing payment is a "BONUS" payment
# there are also versions of the above using max/min missing payments
# as well as approximately similar payments
# 3) remaining missing payments we'll call "UNKNOWN"
#===============================================================================
manual_bonuses <-
  clean_leases %>%
  mutate(HasManual = !is.na(ManualBonus),
         HasBonus = !is.na(Bonus) & (Bonus > 0)) %>%
  replace_na(list(HasBonus = FALSE)) %>%
  filter(HasManual | HasBonus) %>%
  select(Lease_Number, ManualBonus, Bonus)

missing_info <-
  glo_rentals_bonuses %>%
  filter(Type == "Missing") %>%
  group_by(Lease_Number) %>%
  summarize(NMissing = n(),
            MaxPayment = max(PaymentAmount),
            MinPayment = min(PaymentAmount),
            TotalPayment = sum(PaymentAmount))

accounting_bonus_info <-
  glo_rentals_bonuses %>%
  filter(Type == "Bonus") %>%
  group_by(Lease_Number) %>%
  summarize(NBonus = n(),
            TotalBonus = sum(PaymentAmount))

combined_missing_info <-
  missing_info %>%
  left_join(manual_bonuses, by = "Lease_Number") %>%
  left_join(accounting_bonus_info, by = "Lease_Number") %>%
  replace_na(list(NBonus = 1, TotalBonus = 0)) %>%
  mutate(across(c(ManualBonus, Bonus, TotalBonus,
                  TotalPayment, MaxPayment, MinPayment),
                round)) %>%
  mutate(MissingType = case_when(
           !is.na(ManualBonus) & TotalBonus == ManualBonus ~ "OTHERa",
           !is.na(Bonus) & TotalBonus == Bonus ~ "OTHERb",
           !is.na(ManualBonus) &
           (TotalBonus + TotalPayment) == ManualBonus ~ "BONUS1a",
           !is.na(Bonus) &
           (TotalBonus + TotalPayment) == Bonus ~ "BONUS1b",
           !is.na(ManualBonus) &
           (TotalBonus + MaxPayment) == ManualBonus ~ "BONUS2a",
           !is.na(Bonus) &
           (TotalBonus + MaxPayment) == Bonus ~ "BONUS2b",
           !is.na(ManualBonus) &
           (TotalBonus + MinPayment) == ManualBonus ~ "BONUS3a",
           !is.na(Bonus) &
           (TotalBonus + MinPayment) == Bonus ~ "BONUS3b",
           !is.na(ManualBonus) & TotalBonus / ManualBonus >= 0.9 &
           TotalBonus / ManualBonus <= 1.1 ~ "OTHER2a",
           !is.na(Bonus) & TotalBonus / Bonus >= 0.9 &
           TotalBonus / Bonus <= 1.1 ~ "OTHER2b",
           !is.na(ManualBonus) & TotalBonus > 0 &
           (TotalBonus + TotalPayment) / ManualBonus >= 0.9 &
           (TotalBonus + TotalPayment) / ManualBonus <= 1.1 ~ "BONUS4a",
           !is.na(Bonus) & TotalBonus > 0 &
           (TotalBonus + TotalPayment) / Bonus >= 0.9 &
           (TotalBonus + TotalPayment) / Bonus <= 1.1 ~ "BONUS4b",
           !is.na(ManualBonus) & TotalBonus == 0 &
           TotalPayment / ManualBonus >= 0.9 &
           TotalPayment / ManualBonus <= 1.1 ~ "BONUS5a",
           !is.na(Bonus) & TotalBonus == 0 &
           TotalPayment / Bonus >= 0.9 &
           TotalPayment / Bonus <= 1.1 ~ "BONUS5b",
           TRUE ~ "UNKNOWN")) %>%
  select(Lease_Number, MissingType)

# after this categorization, switch MISSING to "Bonus" where appropriate,
# "Other" where appropriate, or keep as "Missing"
payments <-
  glo_rentals_bonuses %>%
  left_join(combined_missing_info, by = "Lease_Number") %>%
  arrange(Lease_Number, Type, PaymentAmount) %>%
  group_by(Lease_Number, Type) %>%
  mutate(IsMaxPmt = row_number() == n(), IsMinPmt = row_number() == 1) %>%
  mutate(Type = case_when(
           Type == "Missing" &
           MissingType %in% c("BONUS1a", "BONUS1b", "BONUS4a", "BONUS4b",
                              "BONUS5a", "BONUS5b") ~ "Bonus",
           Type == "Missing" &
           MissingType %in% c("BONUS2a", "BONUS2b") &
           IsMaxPmt ~ "Bonus",
           Type == "Missing" &
           MissingType %in% c("BONUS3a", "BONUS3b") &
           IsMinPmt ~ "Bonus",
           Type == "Missing" &
           MissingType %in% c("OTHERa", "OTHERb", "OTHER2a", "OTHER2b") ~
             "Other",
           TRUE ~ Type)) %>%
  ungroup

#===============================================================================
# aggregate payments up to the Lease_Number, Date, Type level
#===============================================================================
payments <-
  payments %>%
  group_by(Lease_Number, UndivInt, PaymentDate, Type) %>%
  summarize(Payment = sum(PaymentAmount),
            NPayments = n(),
            HasNegPayment = min(PaymentAmount) < 0) %>%
  ungroup %>%
  group_by(Lease_Number, PaymentDate, Type) %>%
  summarize(Payment = sum(Payment),
            NPayments = sum(NPayments),
            HasNegPayment = max(HasNegPayment) == 1) %>%
  ungroup

# finally stack this accounting based rental/bonus info onto the royalty
# payment data
accounting <-
  payments %>%
  mutate(PaymentDate = make_date(year(PaymentDate), month(PaymentDate), 1)) %>%
  select(Lease_Number, PaymentDate, Type, Payment)

revenues <-
  bind_rows(accounting, royalties)

# ==============================================================================
# find leases with no bonus payments in the accounting revenue data and backfill
# into the revenues file
# ==============================================================================
has_accounting_bonus <-
  revenues %>%
  filter(Type == "Bonus") %>%
  select(Lease_Number) %>%
  distinct

non_accounting_bonus_payments <-
  clean_leases %>%
  mutate(BonusType = if_else(!is.na(ManualBonus) & ManualBonus > 0,
                             "Manual",
                             "Shape"),
         Payment = if_else(BonusType == "Manual", ManualBonus, Bonus), 
         Type = "Bonus",
         PaymentDate = Effective_Date) %>%
  filter(!is.na(Payment), Payment > 0) %>% 
  select(Lease_Number, PaymentDate, Type, Payment, BonusType) %>%
  anti_join(has_accounting_bonus) %>%
  mutate(PaymentDate = ym1(PaymentDate))

# figure out how many bonus payments are from accounting data vs. the tabular
# and shape file data
all_bonus_payments <-
  revenues %>%
  filter(Type == "Bonus") %>%
  mutate(BonusType = "Accounting") %>%
  bind_rows(non_accounting_bonus_payments)

## > all_bonus_payments %>% group_by(BonusType) %>% tally
## # A tibble: 3 x 2
##   BonusType      n
##   <chr>      <int>
## 1 Accounting 15159
## 2 Manual       239
## 3 Shape      19978

# bind these Manual and Shape based bonus paymens onto revenues
revenues <-
  revenues %>%
  bind_rows(select(non_accounting_bonus_payments, -BonusType)) %>%
  mutate(AggType = if_else(Type %in% c("Bonus", "Fee",
                                       "OilRoyalty", "GasRoyalty"),
                           Type,
                           "Other"))


# ==============================================================================
# Reshape these payments into wide format and include months where no payments
# are made on a given lease
# ==============================================================================
# garbage collection step necessary here (at least my a laptop) beccause
# pivot_wider is a memory hog
gc()
revenues_wide <-
  revenues %>%
  filter(Payment != 0) %>%
  group_by(Lease_Number, PaymentDate, AggType) %>%
  summarise(Payment = sum(Payment)) %>% # multiple payments possible 
  ungroup %>% 
  pivot_wider(id_cols = c(Lease_Number, PaymentDate),
              names_from = AggType,
              values_from = Payment,
              values_fill = 0,
              names_prefix = "Pmt",
              names_sep = "")

# define a "realized" expiration date.
# if Lease_Status suggests it (Expired, Released, Terminated) then take the
# latest value of lease status date or lease status update that is both after 
# effective date and before the contractual expire date
# if neither of those satisfy, then set to expire date
realized_expire_dates <-
  clean_leases %>%
  mutate(Temp_Expire_Date = if_else(is.na(Expire_Date),
                                    Effective_Date +
                                    months(as.integer(Term * 12)),
                                    Expire_Date),
         Temp_Lease_Status_Date = if_else(is.na(Lease_Status_Date),
                                          Temp_Expire_Date,
                                          Lease_Status_Date),
         Temp_Lease_Status_Update = if_else(is.na(Lease_Status_Update),
                                            Temp_Lease_Status_Date,
                                            Lease_Status_Update)) %>%
  mutate(REStatus = Lease_Status %in% c("Expired", "Released", "Terminated"),
         Realized_Expire_Date = case_when(
           REStatus &
           (Effective_Date < Temp_Lease_Status_Date) &
           (Temp_Lease_Status_Date <= Temp_Lease_Status_Update) &
           (Temp_Lease_Status_Update <= Temp_Expire_Date) ~
             Temp_Lease_Status_Update,
           REStatus &
           (Effective_Date < Temp_Lease_Status_Update) &
           (Temp_Lease_Status_Update <= Temp_Lease_Status_Date) &
           (Temp_Lease_Status_Date <= Temp_Expire_Date) ~
             Temp_Lease_Status_Date,
           REStatus &
           (Effective_Date < Temp_Lease_Status_Date) &
           (Temp_Lease_Status_Date <= Temp_Expire_Date) ~
             Temp_Lease_Status_Date,
           REStatus &
           (Effective_Date < Temp_Lease_Status_Update) &
           (Temp_Lease_Status_Update <= Temp_Expire_Date) ~
             Temp_Lease_Status_Update,
           TRUE ~ Temp_Expire_Date)) %>%
  select(Lease_Number, Effective_Date, Expire_Date, Realized_Expire_Date,
         Status_Date = Temp_Lease_Status_Date, Lease_Status) %>%
  distinct

# over what dates should we track (potential) payments for a lease?
# Active/Expired: go through max of expire, max payment date, status date.  
# include status because leases can produce and then stop, or multiple
# undivided interests can have slightly different expiration dates and its
# unclear which one GLO is recording
# Held by production/producing/unitized: go through max sample date
# everything else: max of expire/max payment
lease_dates <- 
  revenues_wide %>%
  group_by(Lease_Number) %>%
  summarise(MinPaymentDate = min(PaymentDate),
            MaxPaymentDate = max(PaymentDate)) %>%
  left_join(realized_expire_dates, by = "Lease_Number") %>%
  mutate(across(c(Effective_Date, Expire_Date,
                  Realized_Expire_Date, Status_Date), ym1),
         MinDate = pmin(Effective_Date, MinPaymentDate),
         MaxDate = case_when(
           Lease_Status %in% c("Active", "Expired") ~
             pmax(Expire_Date, Status_Date, MaxPaymentDate),
           Lease_Status %in% c("Held by Production", "Producing", "Unitized") ~
             pmax(LastProductionDate, MaxPaymentDate),
           TRUE ~ pmax(Expire_Date, MaxPaymentDate))) %>%
  mutate(MinDate = if_else(is.na(MinDate), MinPaymentDate, MinDate),
         MaxDate = if_else(is.na(MaxDate), MaxPaymentDate, MaxDate))

# in the data, some leases will report payments after expire date, either
# because they are truly HBP, or because the realized expire date has
# measurement error.  To later define a MaxActiveDate, compute the last date
# that real oil and gas production royalty revenue arrives
last_royalty_date <-
  revenues_wide %>%
  filter(PmtOilRoyalty > 0 | PmtGasRoyalty > 0) %>%
  group_by(Lease_Number) %>%
  summarize(MaxRoyaltyDate = max(PaymentDate))

# Expand to a panel spanning the full term or payment history
# - drop leases with no activity post first production date
# - for leases with payments prior to the first production date, start the
# panel at that date
lease_revenues_monthly <- 
  lease_dates %>%
  left_join(last_royalty_date, by = "Lease_Number") %>%
  filter(MaxDate >= FirstProductionDate) %>%
  mutate(MinDate = pmax(MinDate, FirstProductionDate),
         mdiff = interval(MinDate, MaxDate) %/% months(1) + 1) %>%
  group_by(Lease_Number, MinDate, MaxDate, MaxPaymentDate, MaxRoyaltyDate,
           Effective_Date, Expire_Date, Realized_Expire_Date) %>%
  expand(it = seq(1:mdiff)) %>%
  ungroup %>%
  mutate(PaymentDate = MinDate + months(it-1),
         PreEffectiveDate = PaymentDate < Effective_Date,
         PrimaryTerm = (PaymentDate >= Effective_Date &
                        PaymentDate <= Expire_Date)) %>%
  select(-it) %>%
  ungroup %>%
  left_join(revenues_wide, by = c("Lease_Number", "PaymentDate")) %>%
  replace_na(list(PmtBonus = 0, PmtOther = 0, PmtOilRoyalty = 0,
                  PmtGasRoyalty = 0, PmtFee = 0)) %>%
  mutate(MaxRoyaltyDate = if_else(is.na(MaxRoyaltyDate),
                                  Effective_Date,
                                  MaxRoyaltyDate),
         MaxActiveDate = pmax(Realized_Expire_Date, MaxRoyaltyDate)) %>%
  select(-MaxRoyaltyDate)

# ==============================================================================
# Compute production from royalty payments
# ==============================================================================
prices_long <-
  prices %>%
  filter(Date >= FirstProductionDate, Date <= LastProductionDate) %>%
  pivot_longer(-Date) %>%
  rename(commodity = name, price = value)

royalty_rates <-
  clean_leases %>%
  select(Lease_Number, Effective_Date, Oil_Royalty, Gas_Royalty, LeaseType) %>%
  mutate(Oil_Royalty = if_else(is.na(Oil_Royalty),
                               Gas_Royalty,
                               Oil_Royalty),
         Gas_Royalty = if_else(is.na(Gas_Royalty),
                               Oil_Royalty,
                               Gas_Royalty)) %>%
  pivot_longer(c(Oil_Royalty, Gas_Royalty)) %>%
  rename(commodity = name, royalty_rate = value) %>%
  ## note: vast majority of these missing ones are pre-2004
  filter(!is.na(royalty_rate)) %>%
  filter(royalty_rate > 0) %>%  
  mutate(commodity = str_sub(commodity, 1, 3)) %>%
  mutate(commodity = str_to_lower(commodity))

# ## Check ROYALTY RATES
# te <- royalty_rates %>% filter(LeaseType == "RAL")
# summary(te)
# table(te$Oil_Royalty > .15)
# with(te %>% filter(year(Effective_Date) > 2000), table(Oil_Royalty > .15))
# with(te %>% filter(year(Effective_Date) > 2000), table(Oil_Royalty < .05))
# 
# te <- royalty_rates %>% filter(LeaseType == "STATE")
# summary(te)
# table(te$Oil_Royalty > .30)
# with(te %>% filter(year(Effective_Date) > 2000), table(Oil_Royalty > .30))
# with(te %>% filter(year(Effective_Date) > 2000), table(Oil_Royalty < .05))

# compute inferred production as payment / (price * royalty), and convert
# gas production into BOE terms
inferred_production <-
  lease_revenues_monthly %>%
  filter(PaymentDate >= FirstProductionDate,
         PaymentDate <= LastProductionDate) %>%
  select(Lease_Number, Date = PaymentDate,
         oil = PmtOilRoyalty, gas = PmtGasRoyalty) %>%
  pivot_longer(cols = c(oil, gas),
               names_to = "commodity",
               values_to = "Payment") %>%
  left_join(royalty_rates, by = c("Lease_Number", "commodity")) %>%
  left_join(prices_long, by = c("commodity", "Date")) %>%
  mutate(production = Payment / (price * royalty_rate)) %>%
  select(Lease_Number, PaymentDate = Date, commodity, production) %>%
  pivot_wider(names_from = commodity,
              values_from = production,
              values_fill = 0,
              names_prefix = "BOE_",
              names_sep = "") %>%
  mutate(BOE_gas = BOE_gas / mcf_to_boe)

# ==============================================================================
# Merge production into revenues, double the appropriate revenue types where
# needed for RAL leases, and compute some varibles that will be used in both
# lease and parcel analysis of this data
# ==============================================================================
royalty_rates_wide <-
  royalty_rates %>%
  select(-LeaseType, -Effective_Date) %>%
  pivot_wider(Lease_Number,
              names_from = commodity,
              values_from = royalty_rate) %>%
  rename(Oil_Royalty = oil, Gas_Royalty = gas)

# note: there are 8 leases with positive oil and or gas payments but missing
# royalty information.  to be conservative, we're going to assume that the
# RAL leases in this class have a royalty rate (to GLO) of 12.5%, the STATE
# and OTHER leases have a royalty rate of 25%, and the FREE leases have a rate
# of 6.25%.  these leases don't really survive to any of the main analyses.

# this also creates the LeaseRevenue variable, which is the production revenues
# to the entire piecce of land, not just the portion of those due to royalty
# holders

# second to last step: records all outcomes in appropriate units:
# payments (and LeaseRevenue) in thousands, BOE in hundreds
clean_lease_outcomes_monthly <-
  lease_revenues_monthly %>%
  left_join(inferred_production, by = c("Lease_Number", "PaymentDate")) %>%
  replace_na(list(BOE_oil = 0, BOE_gas = 0)) %>%
  left_join(select(clean_leases, Lease_Number, LeaseType, RALDouble),
            by = "Lease_Number") %>%
  left_join(royalty_rates_wide, by = "Lease_Number") %>%
  mutate(Oil_Royalty = case_when(
           is.na(Oil_Royalty) & LeaseType == "RAL" ~ 0.125,
           is.na(Oil_Royalty) &
           LeaseType %in% c("STATE", "OTHER", "UNKNOWN") ~ 0.25,
           is.na(Oil_Royalty) & LeaseType == "FREE" ~ 0.0625,
           TRUE ~ Oil_Royalty),
         Gas_Royalty = case_when(
           is.na(Gas_Royalty) & LeaseType == "RAL" ~ 0.125,
           is.na(Gas_Royalty) &
           LeaseType %in% c("STATE", "OTHER", "UNKNOWN") ~ 0.25,
           is.na(Gas_Royalty) & LeaseType == "FREE" ~ 0.0625,
           TRUE ~ Gas_Royalty)) %>%
  replace_na(list(RALDouble = FALSE)) %>%
  mutate(LeaseRevenue =
           PmtOilRoyalty / Oil_Royalty + PmtGasRoyalty / Gas_Royalty) %>%
  mutate(across(c(PmtBonus, PmtOilRoyalty, PmtGasRoyalty, PmtOther),
                ~ if_else(RALDouble, 2 * .x, .x))) %>%
  mutate(BOE_total = BOE_oil + BOE_gas,
         PmtRoyalty = PmtOilRoyalty + PmtGasRoyalty,
         PmtTotal = PmtBonus + PmtRoyalty + PmtOther + PmtFee) %>%
  select(-RALDouble, -LeaseType, -Oil_Royalty, -Gas_Royalty) %>%
  mutate(across(c(starts_with("Pmt"), LeaseRevenue), ~ .x / 1000)) %>%
  mutate(across(starts_with("BOE"), ~ .x / 100))  

# last step: re-allocate all output related monthly outcomes, both royalties
# and implied production, *within* each orderly development agreement (oda), by
# relative lease size
odas <-
  file.path(raw_lease, "odas.csv") %>%
  read_csv %>%
  distinct

# create lease shares within each ODA (acreage weighting)
oda_lease_shares <-
  odas %>%
  left_join(select(clean_leases, Lease_Number, GrossAcres)) %>%
  group_by(oda) %>%
  mutate(ODASize = sum(GrossAcres),
         LeaseODAShare = GrossAcres / ODASize) %>%
  ungroup %>%
  select(oda, Lease_Number, LeaseODAShare)

# reallocate ODA-month level production/royalties across leases and then
# recreate other relevant variables
clean_lease_outcomes_monthly_oda_only <-
  clean_lease_outcomes_monthly %>%
  inner_join(oda_lease_shares) %>%
  group_by(oda, PaymentDate) %>%
  mutate(across(c(PmtOilRoyalty, PmtGasRoyalty,
                  BOE_oil, BOE_gas,
                  LeaseRevenue, BOE_total),
                ~ sum(.x) * LeaseODAShare / sum(LeaseODAShare))) %>%
  ungroup %>%
  mutate(BOE_total = BOE_oil + BOE_gas,
         PmtRoyalty = PmtOilRoyalty + PmtGasRoyalty,
         PmtTotal = PmtBonus + PmtRoyalty + PmtOther + PmtFee) %>%
  select(-oda, -LeaseODAShare)

clean_lease_outcomes_monthly_not_oda <-
  clean_lease_outcomes_monthly %>%
  anti_join(oda_lease_shares)

clean_lease_outcomes_monthly <-
  bind_rows(clean_lease_outcomes_monthly_not_oda,
            clean_lease_outcomes_monthly_oda_only) %>%
  arrange(Lease_Number, PaymentDate)

# save to disk
save(clean_lease_outcomes_monthly,
     file = file.path(gen, "clean_lease_outcomes_monthly.Rda"))
