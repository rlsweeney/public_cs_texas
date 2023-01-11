# RICH CREATED THIS OFF OF THE OLD PARCEL_STATS.R
# HEAVY EDITS BY THOM IN FEBRUARY 2019

# BASIC TEXAS SETUP ==========================================================
library(here)
root <- here()
source(file.path(root, "code", "paths.R"))

library(tidyverse)
library(lubridate)
library(fixest)
library(splines)
library(lmtest)
library(sandwich)
library(sf)


source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "texas_constants.R"))
source(file.path(root, "code", "functions", "regtable.R"))

# LOAD AND FILTER PARCELS =====================================================
load(file.path(gen, "clean_parcels.Rda"))

parcels <-
  clean_parcels %>%
  st_set_geometry(NULL) %>%
  filter(ParcelType %in% c("FREE", "RAL", "STATE"), OnShale)  %>%
  mutate(ParcelType = if_else(ParcelType == "STATE", "AUCTION", ParcelType)) %>%
  mutate(ParcelType = factor(ParcelType),
         ParcelType = fct_relevel(ParcelType, "RAL"))

# BALANCE TEST REGS AT GRID LEVEL: is mechanism assignment as good as random?
ldep_vars_names <- c("Thickness", "Acres", parcel_characteristics_names_1)
ldep_vars <- c("ShaleThickness", "ParcelAcres", parcel_characteristics_1)

means <-
  parcels %>%
  select(ldep_vars) %>%
  summarize_all(~ mean(., na.rm = TRUE)) %>%
  as.numeric %>%
  round(3) %>%
  format(big.mark = ",")

setFixest_fml(..depvars = ~ c(ShaleThickness, ParcelAcres, ShapeQuality,
                              DistWaterbodies, DistRiverStreams))
femodels <- feols(..depvars ~ ParcelType | Grid10, parcels)

regtable(as.list(femodels),
         est = c("ParcelTypeAUCTION", "ParcelTypeFREE"),
         mnames = ldep_vars_names,
         extra_rows = list("Average" = means),           
         stats = "r.squared", stats_names= "$R^2$",
         est_names = c("Auction", "Free Royalty")) %>%
  writeLines(file.path(tdir, "parcel_balance.tex"))

# make parcel summary stats, similar to lease summary stats
load(file.path(gen, "final_parcel_outcomes.Rda"))
parcel_data <-
  parcel_outcomes_main$parcel_outcomes %>%
  mutate(Acres = ParcelAcres, Type = ParcelType) %>%
  rename(DBOE = BOE_total, Bonus = PmtBonus, SellerRevenue = PmtTotal) %>%
  mutate(across(c(Bonus, DBOE, SellerRevenue, LeaseRevenue),
                ~ . / Acres, .names = "{col}PerAcre")) %>%
  mutate(Auction = if_else(Type == "STATE", 1, 0),
         Acres = Acres / 1000) %>%
  filter(Type %in% c("RAL", "STATE"))

sumvars <-
  parcel_data %>%
  select(Bonus = BonusPerAcre,
         LeaseRevenue = LeaseRevenuePerAcre,
         SellerRevenue = SellerRevenuePerAcre,
         Output = DBOEPerAcre,
         Acres,
         Auction,
         ShapeQuality,
         ShaleThickness,
         EverLeased,
         NLeases)

sumvar_stats <-
  sumvars %>%
  group_by(Auction) %>%
  summarise_all(list(min = ~ min(., na.rm = TRUE), 
                     max = ~ max(., na.rm = TRUE),
                     mean = ~ mean(., na.rm = TRUE), 
                     sd = ~ sd(., na.rm = TRUE))) %>%
  gather(stat, val, -Auction) %>%
  separate(stat, into = c("variable", "stat"), sep = "_") %>%
  mutate(nstat = paste(stat, Auction, sep = "_")) %>% 
  select(-stat, -Auction) %>%
  spread(nstat, val) %>%
  select(variable,
         min_0,
         max_0,
         mean_0,
         sd_0,
         min_1,
         max_1,
         mean_1,
         sd_1)

# NOW GET TTEST OF MEANS
sumvar_tstats <-
  sumvars %>%
  gather(var, value, -Auction) %>%
  group_by(var) %>%
  do(tidy(t.test(value ~ Auction, data = .))) %>%
  ungroup %>%
  select(variable = var, diff = estimate, p_value = p.value)

# COMBINE AND MAKE IT INTO A TABLE
n_Negotiation <-
  parcel_data %>%
  filter(Auction == 0) %>%
  nrow %>%
  paste("Negotiation (N = ", ., ")", sep = "")

n_Auction <-
  parcel_data %>%
  filter(Auction == 1) %>%
  nrow %>%
  paste("Auction (N = ", ., ")", sep = "")

n_header <- c(1, 4, 4)
names(n_header) <- c(" ", n_Negotiation, n_Auction)

summary_stats_table <-
  inner_join(sumvar_stats, sumvar_tstats, by = "variable") %>%
  select(variable,
         mean_0, sd_0, min_0, max_0,
         mean_1, sd_1, min_1, max_1,
         diff, p_value) %>%
  mutate(temp = case_when(
           variable == "Acres" ~ 1,
           variable == "ShapeQuality" ~ 2,
           variable == "ShaleThickness" ~ 3,
           variable == "EverLeased" ~ 4,
           variable == "NLeases" ~ 5,
           variable == "Bonus" ~ 6,
           variable == "Output" ~ 7,
           variable == "LeaseRevenue" ~ 8,
           variable == "SellerRevenue" ~ 9,
           TRUE ~ NA_real_)) %>%
  mutate(variable = case_when(
           variable == "EverLeased" ~ "Ever Leased",
           variable == "NLeases" ~ "Number of Leases",
           variable == "ShapeQuality" ~ "Shape Quality",
           variable == "ShaleThickness" ~ "Shale Thickness",
           variable == "LeaseRevenue" ~ "Lease Production Revenue",
           variable == "SellerRevenue" ~ "Total Seller Revenue",
           TRUE ~ variable)) %>%
  arrange(temp) %>%
  select(-temp) %>%
  kable(digits = 2,
        format = "latex",
        booktabs = TRUE,
        linesep = "",
        col.names = c("Variable",
                      "mean", "sd", "min", "max", 
                      "mean", "sd", "min", "max", 
                      "Difference", "p-value")) %>%
  group_rows("Land Characteristics", 1, 3) %>%
  group_rows("Parcel Outcomes", 4, 9) %>%  
  add_header_above(n_header)

write(summary_stats_table,
      file = file.path(tdir, "summary_stats_parcel.tex"),
      append = FALSE,
      sep = " ")

