#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
library(here)
root <- here()

library(tidyverse)
library(lubridate)
library(readxl)

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))

options(scipen = 99999)

#=======================================================================
# Read in bonus/rental payment data
#=======================================================================
payments <- 
  file.path(raw_payments, "Rental_Payments_R2045515_PIR20-0640.xlsx") %>%
  read_excel(sheet = 1) %>%
  select(Lease_Number = MFN,
         PaymentDate = `Received Date`,
         PaymentAmount = `Total Payment`,
         Description = DESCRIPTION) %>%
  mutate(PaymentDate = as_date(PaymentDate),
         Lease_Number = str_replace(Lease_Number, "mf", "MF")) 

#=======================================================================
# split off A, B, C indicators on the Lease_Numbers in payments
#=======================================================================
payments <-
  payments %>%
  mutate(LN1 = str_sub(Lease_Number, 1, 8),
         LN2 = str_to_upper(str_sub(Lease_Number, 9, 11))) %>%
  select(-Lease_Number) %>%
  rename(Lease_Number = LN1, UndivInt = LN2)

#=======================================================================
# separate payments in bonus, rentals, fees, missing and other
#=======================================================================
payments <-
  payments %>%
  replace_na(list(Description = "Missing"))

descriptions <-
  payments %>%
  group_by(Description) %>%
  tally %>%
  mutate(Bonus = str_detect(Description, regex("bonus", ignore_case = T)),
         Rental = str_detect(Description, regex("rent", ignore_case = T)),
         Fee = str_detect(Description, regex("fee", ignore_case = T)),
         Royalty = str_detect(Description, regex("roy", ignore_case = T)),
         Depth = str_detect(Description, regex("dept", ignore_case = T)),
         Missing = is.na(Description)) %>%
  mutate(Type = case_when(
           Depth ~ "Depth",
           Fee ~ "Fee",
           Rental ~ "Rental",
           Bonus ~ "Bonus",
           Royalty ~ "ShutInRoyalty",
           TRUE ~ "Missing")) %>%
  select(Description, Type)

payments <-
  payments %>%
  left_join(descriptions, by = "Description")

# create a table of which leases have evidence for undivided interests
uiflags <-
  payments %>%
  select(Lease_Number, UndivInt) %>%
  distinct %>%
  group_by(Lease_Number) %>%
  summarize(NUndivInt = sum(UndivInt != "")) %>%
  ungroup

#===============================================================================
# save payments and these payment-informed undivided interest flags
#===============================================================================
glo_rentals_bonuses <- payments
save(glo_rentals_bonuses, file = file.path(gen, "glo_rentals_bonuses.Rda"))

save(uiflags, file = file.path(gen, "uiflags.Rda"))




