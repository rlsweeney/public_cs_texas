# Created by Yixin Sun May 2018
# Modified by Thom Covert February 2020 from the original clean_parcels.R
# revised again by Thom Covert June 2021 to separate proprietary parcel data
# dependencies

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
# load in raw state leases from the three sources we have, and clean up/combine
# =============================================================================
active <-
  file.path(raw_lease, "June2020/OAG_Leases_Active.shp") %>%
  read_sf %>%
  mutate(Lease_Source = "ACTIVE")

inactive <-
  file.path(raw_lease, "June2020/OAG_Leases_Inactive.shp") %>%
  read_sf %>%
  mutate(Lease_Source = "INACTIVE")

shapefile_lease <-
  rbind(active, inactive) %>%
  st_transform(main_crs) %>%
  mutate(LeasePolygonAcres = as.numeric(st_area(.)) * m2_to_acre) %>%
  select(Lease_Number = LEASE_NUMB, 
         Lease_Status = LEASE_STAT,
         Lease_Status_Date = LEASE_ST_1,
         Lease_Status_Update = LEASE_UPDA,
         GLOLeaseDescription = LEASE_TYPE,
         Effective_Date = EFFECTIVE_, 
         Expire_Date = PRIMARY_TE, 
         Term = PRIMARY__1, 
         Bonus = BONUS_AMOU, 
         Original_Gross = ORIGINAL_G, 
         Original_Net = ORIGINAL_N, 
         Current_Gross = CURRENT_GR, 
         Current_Net = CURRENT_NE, 
         Lessee = ORIGINAL_L, 
         Lessor = LESSOR, 
         Undivided = UNDIVIDED_,
         Unitized = UNITIZED,
         Oil_Royalty = LEASE_ROYA, 
         Gas_Royalty = LEASE_RO_1, 
         Condensate_Royalty = LEASE_RO_2,
         All_Depths = ALL_DEPTHS,
         Depth_From = DEPTH_FROM,
         Depth_To = DEPTH_TO,
         Depth_From_Formation = DEP_FROM_F,
         Depth_To_Formation = DEP_TO_FOR,
         Depth_From_Text = DEPTH_FR_1,
         Depth_To_Text = DEPTH_TO_T,
         PSFLandNumbers = LAND_TYPE,
         Lease_Source, LeasePolygonAcres) %>%
  mutate(AcreageMatch = abs(Original_Gross/LeasePolygonAcres - 1)) %>%
  arrange(Lease_Number, Lease_Source, Lease_Status_Date, AcreageMatch) %>%
  group_by(Lease_Number) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  select(-AcreageMatch) %>%
  filter(!is.na(Lease_Number)) %>%
  mutate(across(matches("Royalty"), as.numeric)) %>%
  mutate(Effective_Date = mdy(Effective_Date),
         Expire_Date = mdy(Expire_Date),
         Lease_Status_Date = mdy(Lease_Status_Date),
         Lease_Status_Update = as_date(mdy_hm(Lease_Status_Update)))

# compute area of convex hull containing each lease
hull <-
  shapefile_lease %>%
  st_convex_hull() %>%
  mutate(LeaseHullAcres = as.numeric(st_area(geometry) * m2_to_acre)) %>%
  st_set_geometry(NULL) %>%
  select(Lease_Number, LeaseHullAcres)

shapefile_lease <-
  shapefile_lease %>%
  left_join(hull, by = "Lease_Number")
  
# fix term in a separate step
shapefile_lease_terms <-
  shapefile_lease %>%
  select(Lease_Number, Term) %>%
  st_set_geometry(NULL) %>%
  mutate(Term = str_to_lower(Term)) %>%
  extract(Term,
          c("TermYears", "TermMonths", "TermDays"),
          "([0-9]+) years, ([0-9]+) months, ([0-9]+) days",
          convert = TRUE) %>%
  mutate(Term = TermYears + TermMonths / 12 + TermDays / 365) %>%
  select(-TermYears, -TermMonths, -TermDays)

shapefile_lease <-
  shapefile_lease %>%
  select(-Term) %>%
  left_join(shapefile_lease_terms, by = "Lease_Number")

shapefile_lease_shapes <-
  shapefile_lease %>%
  mutate(ShapeGrossAreaMatch = case_when(
           is.na(Original_Gross) ~ FALSE,
           !is.na(Original_Gross) & Original_Gross == 0 ~ FALSE,
           !is.na(Original_Gross) &
           Original_Gross > 0 &
           abs(log(Original_Gross) - log(LeasePolygonAcres)) <= 0.05 ~ TRUE,
           TRUE ~ FALSE)) %>%
  select(Lease_Number, ShapeGrossAreaMatch)

shapefile_lease <-
  shapefile_lease %>%
  st_set_geometry(NULL)

# then add in the tabular lease data we have
# effective date is stored with a 2 digit year
# pretty sure earliest lease in this data is 1914
# so if 2 digit year is 14-20, call it 1900 if lease number < 10000
# if its 14-20 and lease number >= 100000, call it 2000
# if its 0-13 call it 2000
# if its 21-99, call it 1900
tabular_lease <-
  file.path(raw_lease, "tblMineralLeaseSummaryAll_2020.csv") %>%
  read_csv(col_types = cols(.default = "c")) %>%
  select(Lease_Number = varLeaseNumber, 
         Lease_Status = varMineralLeaseStatus,
         Lease_Status_Date = datMineralLeaseStatusDate,
         Lease_Status_Update = datLeaseStatusUpdateDate,         
         GLOLeaseDescription = varMineralLeaseType,
         Effective_Date = datMineralLeaseEffectiveDate ,
         Term = intMineralLeasePrimaryTermYear, 
         Bonus = monMineralLeaseBonusAmount , 
         Original_Gross = decMineralLeaseOriginalGrossAcres, 
         Original_Net = decMineralLeaseOriginalNetAcres, 
         Current_Gross = decMineralLeaseCurrentGrossAcres, 
         Current_Net = decMineralLeaseCurrentNetAcres, 
         Lessee = varMineralLeaseLesseeLessorCompanyName, 
         Lessor = varMineralLeaseLessor , 
         Undivided = varMineralLeaseUndividedInterestIndicator,
         Unitized = varMineralLeaseUnitizedIndicator,
         Oil_Royalty = varOilMineralLeaseRoyaltyDecimal, 
         Gas_Royalty = varGasMineralLeaseRoyaltyDecimal, 
         Condensate_Royalty = varSulfurMineralLeaseRoyaltyDecimal,
         All_Depths = varMineralLeaseAllDepths,
         Depth_From = intMineralLeaseDepthFromAmount,
         Depth_To = intMineralLeaseDepthToAmount,
         Depth_From_Formation = varMineralLeaseDepthFromFormationName,
         Depth_To_Formation = varMineralLeaseDepthToFormationName,
         Depth_From_Text = varMineralLeaseDepthFromText,
         Depth_To_Text = varMineralLeaseDepthToText,
         PSFLandNumbers = varLandType) %>%
  unique %>%
  filter(str_detect(Lease_Number, "MF")) %>%
  mutate_all(~ if_else(. == "NULL", NA_character_, .)) %>%
  mutate_at(vars(matches("Gross|Net|Royalty"), Bonus), as.numeric) %>%
  mutate(Term = str_to_lower(Term)) %>%
  extract(Term,
          c("TermYears", "TermMonths", "TermDays"),
          "([0-9]+) years, ([0-9]+) months, ([0-9]+) days",
          convert = TRUE) %>%
  mutate(Term = TermYears + TermMonths / 12 + TermDays / 365) %>%
  mutate(Effective_Date = as_date(ymd_hm(Effective_Date)),
         Lease_Status_Date = as_date(ymd_hm(Lease_Status_Date)),
         Lease_Status_Update = as_date(mdy_hm(Lease_Status_Update))) %>%
  mutate(Effective_Date = case_when(
           year(Effective_Date) >= 2014 &
           year(Effective_Date) <= 2020 &
           Lease_Number < "MF100000" ~ Effective_Date - years(100),
           year(Effective_Date) >= 2014 &
           year(Effective_Date) <= 2020 &
           Lease_Number >= "MF100000" ~ Effective_Date,
           year(Effective_Date) < 2014 ~ Effective_Date,
           year(Effective_Date) > 2020 ~ Effective_Date - years(100))) %>%
  select(-TermYears, -TermMonths, -TermDays) %>%
  mutate(Lease_Source = "TABULAR")

# combine them into a single file and do some cleaning, including a best effort
# backfill of Expire_Date and dropping leases with missing Effective and Expire
tabular_lease_not_in_shapefile <-
  tabular_lease %>%
  anti_join(shapefile_lease, by = "Lease_Number")

leases <-
  shapefile_lease %>%
  bind_rows(tabular_lease_not_in_shapefile) %>%
  mutate(Expire_Date = if_else(is.na(Expire_Date),
                               as_date(Effective_Date + dyears(Term)),
                               Expire_Date)) %>%
  replace_na(list(GLOLeaseDescription = "Missing"))

# figure out depth restrictions
leases <-
  leases %>%
  mutate(All_Depths = case_when(
           All_Depths == "Allow All Depths" ~ "ALL",
           !is.na(All_Depths) ~ "SPECIFIED",
           TRUE ~ "MISSING")) %>%
  mutate(DepthRestrictionFormation =
           !is.na(Depth_From_Formation) | !is.na(Depth_To_Formation),
         DepthRestrictionFromText = case_when(
           str_detect(Depth_From_Text,
                      regex("surface", ignore_case = T)) ~ "SURFACE",
           !is.na(Depth_From_Text) ~ "OTHER",
           TRUE ~ "MISSING"),
         DepthRestrictionToText = case_when(
           str_detect(Depth_To_Text,
                      regex("100.*below.*depth", ignore_case = T)) ~ "HBP",
           str_detect(Depth_To_Text,
                      regex("100.*below.*td", ignore_case = T)) ~ "HBP",
           str_detect(Depth_To_Text,
                      regex("all depths", ignore_case = T)) ~ "ALL",
           !is.na(Depth_To_Text) ~ "OTHER",
           TRUE ~ "MISSING"),
         DepthRestrictionText = case_when(
           DepthRestrictionFromText == "OTHER" ~ TRUE,
           DepthRestrictionToText == "OTHER" ~ TRUE,
           TRUE ~ FALSE))
           
multiple_depth_from <-
  leases %>%
  select(Lease_Number, Depth_From) %>%
  filter(!is.na(Depth_From)) %>%
  filter(str_detect(Depth_From, ";")) %>%
  separate_rows(Depth_From) %>%
  mutate(Depth_From = as.numeric(Depth_From)) %>%  
  group_by(Lease_Number) %>%
  summarize(Depth_From_Min = min(Depth_From), Depth_From_Max = max(Depth_From))

multiple_depth_to <-
  leases %>%
  select(Lease_Number, Depth_To) %>%
  filter(!is.na(Depth_To)) %>%
  filter(str_detect(Depth_To, ";")) %>%
  separate_rows(Depth_To) %>%
  mutate(Depth_To = as.numeric(Depth_To)) %>%
  group_by(Lease_Number) %>%
  summarize(Depth_To_Min = min(Depth_To), Depth_To_Max = max(Depth_To))

leases <-
  leases %>%
  left_join(multiple_depth_from, by = "Lease_Number") %>%
  left_join(multiple_depth_to, by = "Lease_Number") %>%
  mutate(Depth_From = as.numeric(Depth_From),
         Depth_To = as.numeric(Depth_To)) %>%
  mutate(Depth_From2 = case_when(
           is.na(Depth_From) & !is.na(Depth_From_Min) ~ Depth_From_Min,
           !is.na(Depth_From) & !is.na(Depth_To) & Depth_From > Depth_To ~
             Depth_To,
           !is.na(Depth_From) ~ Depth_From,
           TRUE ~ NA_real_),
         Depth_To2 = case_when(
           is.na(Depth_To) & !is.na(Depth_To_Max) ~ Depth_To_Max,
           !is.na(Depth_To) & !is.na(Depth_From) & Depth_From > Depth_To ~
             Depth_From,
           !is.na(Depth_To) ~ Depth_To,
           TRUE ~ NA_real_)) %>%
  select(-Depth_From, -Depth_To, -Depth_From_Min, -Depth_From_Max,
         -Depth_To_Min, -Depth_To_Max) %>%
  rename(Depth_From = Depth_From2, Depth_To = Depth_To2) %>%
  mutate(DepthRestrictionFeet = case_when(
           !is.na(Depth_From) & Depth_From == 0 & is.na(Depth_To) ~ FALSE,
           !is.na(Depth_From) | !is.na(Depth_To) ~ TRUE,
           TRUE ~ FALSE)) %>%
  mutate(HasDepthRestriction = All_Depths == "SPECIFIED" &
           (DepthRestrictionFeet |
            DepthRestrictionFormation |
            DepthRestrictionText))

# =============================================================================
# load in the manually entered bonus information, which we'll use later on when
# we don't have reliable accounting information
# also load in auction results here
# =============================================================================

# "original" missing or zero'd bonus data.
glo_missing_bonus0 <- 
  file.path(int, "leases/missing_bonus.csv") %>%
  read_csv %>%
  filter(KeepBonus == 1) %>%
  select(Lease_Number, MissingBonus = Bonus) %>% 
  mutate(MissingBonus = if_else(MissingBonus == 0, NA_real_, MissingBonus)) %>%
  filter(!is.na(MissingBonus)) %>%
  distinct %>%
  mutate(source = 0)

# second pass
glo_missing_bonus1 <- 
  file.path(int, "leases/missing_bonus2.csv") %>%
  read_csv %>% 
  mutate(MissingBonus = if_else(MissingBonus == 0, NA_real_, MissingBonus)) %>%
  filter(!is.na(MissingBonus)) %>%
  mutate(source = 1)

# third pass
glo_missing_bonus2a <-
  file.path(int, "leases/missing_bonus3.xlsx") %>%
  read_excel(sheet = "older") %>%
  select(Lease_Number, MissingBonus = Bonus) %>%
  filter(!is.na(MissingBonus)) %>%
  mutate(source = 2)

# fourth pass!
glo_missing_bonus2b <-
  file.path(int, "leases/missing_bonus3.xlsx") %>%
  read_excel(sheet = "newer") %>%
  select(Lease_Number, MissingBonus = NewBonus) %>%
  filter(!is.na(MissingBonus)) %>%
  mutate(source = 3)

glo_missing_bonus <-
  bind_rows(glo_missing_bonus0,
            glo_missing_bonus1,
            glo_missing_bonus2a,
            glo_missing_bonus2b) %>%
  group_by(Lease_Number) %>%
  mutate(NSources = n()) %>%
  ungroup

glo_missing_bonus_1source <-
  glo_missing_bonus %>%
  filter(NSources == 1) %>%
  select(Lease_Number, MissingBonusSource = source, MissingBonus)

glo_missing_bonus_2source <-
  glo_missing_bonus %>%
  filter(NSources == 2) %>%
  filter(source > 1) %>%
  select(Lease_Number, MissingBonusSource = source, MissingBonus)

glo_missing_bonus <-
  bind_rows(glo_missing_bonus_1source, glo_missing_bonus_2source)

# backfilled "other" data
other_fixes <- 
  file.path(int, "leases/lease_check.csv") %>%
  read_csv %>%
  filter(!str_detect(Lease_Number, "-")) %>%
  select(-Effective_Date, -Notes, -True_Net, -Exclude)

# missing undivided flag
missing_undivided <- 
  file.path(int, "leases/missing_undivided.xlsx") %>%
  read_excel %>%
  rename(ManualUndivided = Undivided)

# undivided flags as informed by A vs B vs C leases in the payments data
load(file.path(gen, "uiflags.Rda"))

# RAL coversheets (recorded per acre, for the entire estate, so need to halve)
load(file.path(gen, "coversheets.Rda"))
coversheet_bonus_per_acre <-
  coversheets %>%
  filter(is.na(No_Pdf), is.na(No_Review)) %>%
  select(Lease_Number, BPA_Rec) %>%
  mutate(Lease_Number = paste0("MF", Lease_Number)) %>%
  rename(CoversheetBonusPerAcre = BPA_Rec) %>%
  mutate(CoversheetBonusPerAcre = CoversheetBonusPerAcre * 0.5)

# winning bids
load(file.path(gen, "clean_bids.Rda"))
bids_bonus <-
  clean_bids %>%
  filter(Winner) %>%
  select(Lease_Number, AuctionBonus = Bonus)

# final round of manual checks for bonuses with no other checks
final_manual_checks <-
  file.path(int, "leases/missing_bonus4.xlsx") %>%
  read_excel %>%
  select(Lease_Number, Bonus, CheckedBonus, FixedBonus, Notes)

fmc_correct <-
  final_manual_checks %>%
  filter(CheckedBonus == "yes") %>%
  select(Lease_Number, CheckedBonus = Bonus)

fmc_fixed <-
  final_manual_checks %>%
  filter(CheckedBonus == "no") %>%
  select(Lease_Number, CheckedBonus = FixedBonus)

fmc_haspdf <-
  bind_rows(fmc_correct, fmc_fixed)
  
fmc_nopdf <-
  final_manual_checks %>%
  filter(Notes == "missing pdf") %>%
  mutate(NoPDF = TRUE) %>%
  select(Lease_Number, NoPDF)

# combine the bonus data we've got
combined_bonus_data <-
  glo_missing_bonus %>%
  full_join(select(other_fixes, Lease_Number, OtherBonus = True_Bonus),
            by = "Lease_Number") %>%
  full_join(coversheet_bonus_per_acre, by = "Lease_Number") %>%
  full_join(bids_bonus, by = "Lease_Number") %>%
  full_join(missing_undivided, by = "Lease_Number") %>%
  left_join(uiflags, by = "Lease_Number")

# merge it into leases and do a few more minor tweaks
leases <-
  leases %>%
  left_join(combined_bonus_data, by = "Lease_Number") %>%
  mutate(ManualBonus = case_when(
           !is.na(MissingBonus) ~ MissingBonus,
           !is.na(OtherBonus) ~ OtherBonus,
           TRUE ~ NA_real_)) %>%
  mutate(Undivided = if_else(is.na(Undivided), ManualUndivided, Undivided)) %>%
  mutate(InferredUndivided = case_when(
           is.na(NUndivInt) ~ "Missing",
           NUndivInt == 0 ~ "NO",
           TRUE ~ "YES")) %>%
  select(-ManualUndivided, -NUndivInt)

# =============================================================================
# fix assumed royalty rate typos and implement manual fixes
# these most likely have a missing leading zero
# =============================================================================
# MF120032, MF117808, MF103073

leases <-
  leases %>%
  mutate(Oil_Royalty = if_else(Lease_Number == "MF105448", 0.115, Oil_Royalty),
         Gas_Royalty = if_else(Lease_Number == "MF105448", 0.115, Gas_Royalty),
         Condensate_Royalty = if_else(Lease_Number == "MF105448",
                                      0.115,
                                      Condensate_Royalty)) %>%
  mutate(Oil_Royalty = if_else(Lease_Number == "MF120032", 0.125, Oil_Royalty),
         Gas_Royalty = if_else(Lease_Number == "MF120032", 0.125, Gas_Royalty),
         Condensate_Royalty = if_else(Lease_Number == "MF120032",
                                      0.125,
                                      Condensate_Royalty)) %>%  
  mutate(temp_Oil_Royalty = if_else(is.na(Oil_Royalty), 0, Oil_Royalty),
         temp_Gas_Royalty = if_else(is.na(Gas_Royalty), 0, Gas_Royalty),
         temp_Condensate_Royalty = if_else(is.na(Condensate_Royalty),
                                           0,
                                           Condensate_Royalty)) %>%
  mutate(Oil_Royalty = if_else(temp_Oil_Royalty > 0.5,
                               Oil_Royalty / 10,
                               Oil_Royalty),
         Gas_Royalty = if_else(temp_Gas_Royalty > 0.5,
                               Gas_Royalty / 10,
                               Gas_Royalty),
         Condensate_Royalty = if_else(temp_Condensate_Royalty > 0.5,
                                      Condensate_Royalty / 10,
                                      Condensate_Royalty)) %>%
  # do a second pass of this for leases where the PERCENT was entered 
  mutate(temp_Oil_Royalty = if_else(is.na(Oil_Royalty), 0, Oil_Royalty),
         temp_Gas_Royalty = if_else(is.na(Gas_Royalty), 0, Gas_Royalty),
         temp_Condensate_Royalty = if_else(is.na(Condensate_Royalty),
                                           0,
                                           Condensate_Royalty)) %>%
  mutate(Oil_Royalty = if_else(temp_Oil_Royalty > 0.5,
                               Oil_Royalty / 10,
                               Oil_Royalty),
         Gas_Royalty = if_else(temp_Gas_Royalty > 0.5,
                               Gas_Royalty / 10,
                               Gas_Royalty),
         Condensate_Royalty = if_else(temp_Condensate_Royalty > 0.5,
                                      Condensate_Royalty / 10,
                                      Condensate_Royalty)) %>% 
  select(-temp_Oil_Royalty, -temp_Gas_Royalty, -temp_Condensate_Royalty) %>%
  mutate(Oil_Royalty = if_else(Lease_Number == "MF105448", 0.115, Oil_Royalty),
         Gas_Royalty = if_else(Lease_Number == "MF105448", 0.115, Gas_Royalty),
         Condensate_Royalty = if_else(Lease_Number == "MF105448",
                                      0.115,
                                      Condensate_Royalty))

# read in and apply some royalty rate manual checks
royalty_fixes <-
  file.path(int, "leases/royalty_fixes.xlsx") %>%
  read_excel

leases_nofix <-
  leases %>%
  anti_join(royalty_fixes, by = "Lease_Number")

leases_fix <-
  leases %>%
  inner_join(royalty_fixes, by = "Lease_Number") %>%
  mutate(Oil_Royalty = if_else(Halve, Oil_Royalty / 2, Oil_Royalty),
         Gas_Royalty = if_else(Halve, Gas_Royalty / 2, Gas_Royalty),
         Condensate_Royalty = if_else(Halve,
                                      Condensate_Royalty / 2,
                                      Condensate_Royalty)) %>%
  mutate(Oil_Royalty = if_else(FIX, NewRoyalty, Oil_Royalty),
         Gas_Royalty = if_else(FIX, NewRoyalty, Gas_Royalty),
         Condensate_Royalty = if_else(FIX, NewRoyalty, Condensate_Royalty)) %>%
  select(-Halve, -FIX, -NewRoyalty)

# stitch this back together and then backfill royalty rates where possible
leases <-
  bind_rows(leases_nofix, leases_fix) %>%
  mutate(Oil_Royalty = if_else(is.na(Oil_Royalty) & ! is.na(Gas_Royalty),
                               Gas_Royalty,
                               Oil_Royalty),
         Gas_Royalty = if_else(is.na(Gas_Royalty) & ! is.na(Oil_Royalty),
                               Oil_Royalty,
                               Gas_Royalty))

# =============================================================================
# small number of effective date fixes
# =============================================================================
effective_date_fixes <-
  file.path(int, "leases/effective_date_fixes.xlsx") %>%
  read_excel %>%
  mutate(New_Effective_Date = as_date(New_Effective_Date))

leases <-
  leases %>%
  left_join(effective_date_fixes, by = "Lease_Number") %>%
  mutate(Effective_Date = if_else(!is.na(New_Effective_Date),
                                  New_Effective_Date,
                                  Effective_Date)) %>%
  select(-New_Effective_Date)

# =============================================================================
# small number of expire date/term fixes
# =============================================================================
term_fixes <-
  file.path(int, "leases/term_fixes.xlsx") %>%
  read_excel

leases <-
  leases %>%
  left_join(term_fixes, by = "Lease_Number") %>%
  mutate(Term = if_else(!is.na(New_Term), New_Term, Term),
         Expire_Date = if_else(!is.na(New_Term),
                               as_date(Effective_Date + dyears(New_Term)),
                               Expire_Date)) %>%
  select(-New_Term)

# =============================================================================
# flag leases based on GLO's description as being outside of the standard RAL
# Free Royalty or State designation
# =============================================================================
leases <-
  leases %>%
  mutate(ExtraLeaseType = case_when(
           str_detect(GLOLeaseDescription, "32.207") ~ "TAC32",
           str_detect(GLOLeaseDescription, "52") ~ "TAC52",           
           str_detect(GLOLeaseDescription, "56") ~ "TAC56",
           str_detect(GLOLeaseDescription, "CRA") ~ "CRA",
           str_detect(GLOLeaseDescription, "MPAA") ~ "MPAA",
           str_detect(GLOLeaseDescription, "Unleased") ~ "Unleased",
           str_detect(GLOLeaseDescription, "HROW") ~ "HROW",
           TRUE ~ "NONE"),
         ExtraLeaseType = if_else(Lease_Number == "MF114830",
                                  "MPAA",
                                  ExtraLeaseType),
         ExtraLeaseType = if_else(Lease_Number == "MF117808",
                                  "MPAA",
                                  ExtraLeaseType),
         HasExtraLeaseType = if_else(ExtraLeaseType != "NONE", TRUE, FALSE))
           
# =============================================================================
# clean up the undivided flag
# =============================================================================
leases <-
  leases %>%
  mutate(Undivided = str_to_upper(Undivided)) %>%
  replace_na(list(Undivided = "Missing"))

# =============================================================================
# load in the mineral file to control number match
# =============================================================================
load(file.path(gen, "parcel_lease_bridge.Rda"))
parcel_lease_bridge <-
  parcel_lease_bridge %>%
  filter(BridgeType != "REMOVED") %>%
  select(-BridgeType)

# =============================================================================
# read-in the pre-computed lease spatial information from
# clean_lease_spatial_info.R.  this requires the parcel files to run
# =============================================================================
load(file.path(gen, "leases_spatial_info.Rda"))

# merge the spatial variables into leases
leases <-
  leases %>%
  left_join(lease_spatial_info, by = "Lease_Number") %>%
  replace_na(list(NParcels = 0, NCleanParcels = 0))

# =============================================================================
# acreage cleaning
# =============================================================================
# Organize Original Gross, Original Net, Current Gross, and Current Net
# into just Gross and Net acres (GrossAcres, NetAcres), with an intended bias
# towards using the original values if they are non-missing and non-zero
# to end up with non-missing values for GrossAcres and NetAcres, if GrossAcres
# is missing, replace it first with LeasePolygonAcres if its non-missing, then
# TotalParcelAcresInLease if that is missing.  Then, if NetAcres is missing,
# replace it with GrossAcres.

leases <-
  leases %>%
  mutate(GrossAcres = case_when(
           !is.na(Original_Gross) & Original_Gross > 0 ~ Original_Gross,
           TRUE ~ NA_real_),
         NetAcres = case_when(
           !is.na(Original_Net) & Original_Net > 0 ~ Original_Net,
           TRUE ~ NA_real_)) %>%
  mutate(GrossAcres = case_when(
           !is.na(GrossAcres) ~ GrossAcres,
           !is.na(LeasePolygonAcres) ~ LeasePolygonAcres,
           !is.na(TotalParcelAcresInLease) ~ TotalParcelAcresInLease,
           TRUE ~ NA_real_),
         NetAcres = if_else(!is.na(NetAcres), NetAcres, GrossAcres),
         NetAcres = pmin(NetAcres, GrossAcres))

# also, do 2 manual size fixes
leases <-
  leases %>%
  mutate(GrossAcres = if_else(Lease_Number == "MF105814", 31.47, GrossAcres),
         GrossAcres = if_else(Lease_Number == "MF106026", 8.468, GrossAcres),
         NetAcres = if_else(Lease_Number == "MF105814", 31.47, NetAcres),
         NetAcres = if_else(Lease_Number == "MF106026", 8.468, NetAcres))
         
           
# =============================================================================
# create link between LLF, parcels, and leases
# =============================================================================
lease_types <-
  parcel_lease_bridge %>%
  mutate(LandType = str_sub(ControlNum, 1, 2)) %>%
  group_by(Lease_Number) %>%
  mutate(LeaseType = case_when(
           min(LandType == "07") == 1 ~ "RAL",
           max(LandType == "07") == 1 & min(LandType == "07") == 0 ~ "RALMIX",
           min(LandType == "08") == 1 ~ "FREE",
           max(LandType == "08") == 1 & min(LandType == "08") == 0 ~ "FREEMIX",
           min(LandType %in% c("04", "15")) == 1  ~ "STATE",
           max(LandType %in% c("04", "15")) == 1 &
           min(LandType %in% c("04", "15")) == 0 ~ "STATEMIX",
           TRUE ~ "OTHER")) %>%
  ungroup %>%
  select(Lease_Number, LeaseType) %>%
  distinct

# merge this in, and also generate a version using PSFLandNumbers which maybe
# we'll use if a lease is not in the lease land file
glo_lease_types <-
  leases %>%
  select(Lease_Number, PSFLandNumbers) %>%
  rename(LandType = PSFLandNumbers) %>%
  separate_rows(LandType, sep = ";") %>%
  mutate(LandType = str_trim(LandType),
         LandType = str_pad(LandType, 2, "left", pad = "0")) %>%
  replace_na(list(LandType = "00")) %>%
  group_by(Lease_Number) %>%
  mutate(LeaseTypeGLO = case_when(
           min(LandType == "07") == 1 ~ "RAL",
           max(LandType == "07") == 1 & min(LandType == "07") == 0 ~ "RALMIX",
           min(LandType == "08") == 1 ~ "FREE",
           max(LandType == "08") == 1 & min(LandType == "08") == 0 ~ "FREEMIX",
           min(LandType %in% c("04", "15")) == 1  ~ "STATE",
           max(LandType %in% c("04", "15")) == 1 &
           min(LandType %in% c("04", "15")) == 0 ~ "STATEMIX",
           TRUE ~ "OTHER")) %>%
  ungroup %>%
  select(Lease_Number, LeaseTypeGLO) %>%
  distinct

leases <-
  leases %>%
  left_join(lease_types, by = "Lease_Number") %>%
  left_join(glo_lease_types, by = "Lease_Number") %>%
  replace_na(list(LeaseType = "UNKNOWN", LeaseTypeGLO = "UNKNOWN"))

# =============================================================================
# compare lease-based and lease-land-file-based measures of lease classification
# =============================================================================
lease_type_compare <-
  leases %>%
  group_by(LeaseType, LeaseTypeGLO) %>%
  tally

# ===========================================================================
# add lease assignment information where we have it
# ===========================================================================
# add in assignee information
load(file.path(gen, "assignments.Rda"))
assignments <-
  select(assignments, Lease_Number, Assignee, AssignDate)

leases <-
  leases %>%
  regex_left_join(assignments) %>%
  group_by(Lease_Number.x) %>%
  arrange(AssignDate) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  select(-Lease_Number.y) %>%
  rename(Lease_Number = Lease_Number.x)


# merge in alias names for lessee and asignee
company_names <-
  file.path(int, "manually_improved_names.xlsx") %>%
  read_excel %>%
  select(Name, alias = Alias) %>%
  mutate(Name = str_to_upper(Name)) %>%
  select(Name, alias) %>%
  unique

leases <-
  leases  %>%
  mutate(Lessee2 = str_to_upper(Lessee)) %>%
  left_join(company_names, by = c("Lessee2" = "Name")) %>%
  mutate(Lessee_alias = if_else(!is.na(alias), alias, Lessee2)) %>%
  select(-alias, -Lessee2) %>%
  mutate(Assignee2 = str_to_upper(Assignee)) %>%
  left_join(company_names, by = c("Assignee2" = "Name")) %>%
  rename(Assignee_alias = alias) %>%
  mutate(Assignee_alias = if_else(is.na(Assignee_alias),
                                  Assignee2, Assignee_alias)) %>%
  select(-Assignee2)

# =============================================================================
# flag leases whose recorded payments need to be doubled to capture full benefit
# to the mineral estate
# this includes all RAL's except those with an auction bonus, those that have
# an ExtraLeaseType, or those for which the lessor is the State of Texas
# Define a ValidLease as one which
# (a) actually happened (not cancelled/withdrawn)
# (b) has non-missing effective date and term
# (c) has non-missing royalty info
# (d) has non-missing spatial info from the lease-parcel bridge
# Also, define "Auction" leases as those with non-missing auction bonus payments
# =============================================================================
clean_leases <-
  leases %>%
  mutate(TempLessor = if_else(is.na(Lessor), "MISSING", Lessor)) %>%
  mutate(TexasLessor =
           str_detect(TempLessor, regex("State of Texas|General Land Office",
                                        ignore_case = TRUE)),
         RALDouble =
           (LeaseType == "RAL" |
            (LeaseType == "UNKNOWN" & LeaseTypeGLO == "RAL")) &
           is.na(AuctionBonus) &
           (HasExtraLeaseType == FALSE) &
           !TexasLessor) %>%
  select(-TempLessor) %>%
  replace_na(list(Effective_Date = make_date(2099, 12, 31))) %>%
  mutate(ValidLease =
           !(Lease_Status %in% c("Cancelled", "Forfeited",
                                 "Withdrawn", "Inactive")) &
           !is.na(Oil_Royalty) &
           NCleanParcels > 0,
         Auction = !is.na(AuctionBonus), 
         RoyaltyRate = if_else(RALDouble, 2 * Oil_Royalty, Oil_Royalty)) 

# ==============================================================================
# save to disk
# ==============================================================================
save(clean_leases, file = file.path(gen, "clean_leases.Rda"))

clean_leases_shapes <- shapefile_lease_shapes
save(clean_leases_shapes, file = file.path(gen, "clean_leases_shapes.Rda"))

  


