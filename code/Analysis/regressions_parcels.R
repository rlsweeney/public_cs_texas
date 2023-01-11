#==============================================================================
# code to estimate and format regression tables for parcel outcomes
# this is currently WIP, since it estimates way more specs than we ultimately
# want to report
# by thom/rich in december 2020 - march 2021
#==============================================================================
library(tidyverse)
library(lubridate)
library(splines)
library(lmtest)
library(sandwich)
library(fixest)
library(sf)

#==============================================================================
# BASIC TEXAS SETUP
#==============================================================================
library(here)
root <- here()

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))
source(file.path(root, "code", "functions", "regtable.R"))
source(file.path(root, "code", "functions", "latex_number.R"))
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "functions", "load_crossfit.R"))

#===============================================================================
# LOAD DATA CLEANED IN final_parcel_outcomes.R: parcel outcomes and lease
# outcomes underlying these parcel outcomes.
# ALSO LOAD final leases so that we can compare lease and parcel results
#===============================================================================
load(file.path(gen, "final_parcel_outcomes.Rda"))
regdata <-
  parcel_outcomes_main$parcel_outcomes %>%
  mutate(Acres = ParcelAcres, Type = ParcelType) %>%
  rename(DBOE = BOE_total, Bonus = PmtBonus, SellerRevenue = PmtTotal) %>%
  mutate(across(c(Bonus, DBOE, SellerRevenue, LeaseRevenue),
                ~ . / Acres, .names = "{col}PerAcre")) %>%
  mutate(Auction = if_else(Type == "STATE", 1, 0),
         Acres = Acres / 1000) %>%
  filter(Type %in% c("RAL", "STATE"))

load(file.path(gen, "final_leases.Rda"))
regdata_leases_fl_info <-
  final_leases %>%
  select(Lease_Number, InSample, FLAuction = Auction, Year)

regdata_leases <-
  parcel_outcomes_main$lease_outcomes %>%
  mutate(Acres = GrossAcres, Type = LeaseType) %>%
  select(-Bonus) %>%
  rename(DBOE = BOE_total, Bonus = PmtBonus, SellerRevenue = PmtTotal) %>%
  mutate(across(c(Bonus, DBOE, SellerRevenue, LeaseRevenue),
                ~ . / Acres, .names = "{col}PerAcre")) %>%
  filter(Type %in% c("RAL", "STATE")) %>%
  mutate(Acres = Acres / 1000) %>%  
  mutate(Auction = if_else(Type == "STATE", 1, 0)) %>%
  mutate(EffDate = as.numeric(Effective_Date)) %>%
  left_join(regdata_leases_fl_info) %>%
  replace_na(list(InSample = FALSE))

load(file.path(gen, "outcomes_earlyLeases.Rda"))
regdata_everleased <-
  outcomes_earlyLeases$parcel_outcomes %>%
  mutate(Acres = ParcelAcres, Type = ParcelType) %>%
  rename(DBOE = BOE_total, Bonus = PmtBonus, SellerRevenue = PmtTotal) %>%
  mutate(across(c(Bonus, DBOE, SellerRevenue, LeaseRevenue),
                ~ . / Acres, .names = "{col}PerAcre")) %>%
  mutate(Auction = if_else(Type == "STATE", 1, 0),
         Acres = Acres / 1000) %>%
  filter(Type %in% c("RAL", "STATE"))


#===============================================================================
# define specifications
#===============================================================================
# this reduces code complexity/duplication in the feols calls that follow
setFixest_fml(..depvarspa = ~ c(BonusPerAcre, DBOEPerAcre,
                                LeaseRevenuePerAcre, SellerRevenuePerAcre),
              ..depvars = ~ c(Bonus, DBOE, LeaseRevenue, SellerRevenue),
              ..fe = ~sw(Grid10, Grid20),
              ..surface = c(parcel_characteristics_1, parcel_characteristics_2))

dml_pa_outcomes <-
  c("BonusPerAcre", "DBOEPerAcre",
    "LeaseRevenuePerAcre", "SellerRevenuePerAcre")

dml_outcomes <-
  c("Bonus", "DBOE", "LeaseRevenue", "SellerRevenue")

dml_spec <- "Acres + CentLat + CentLong"

extra_controls <-
  paste(paste0(parcel_characteristics_1, collapse = " + "),
        paste0(parcel_characteristics_2, collapse = " + "),
        sep = " + ")

dml_fml <- function(...) {
  left_fml <- "Auction"
  right_fml <- paste(dml_spec, ..., sep = " + ")
  fml <- paste(left_fml, right_fml, sep = " | ")
  return(fml)
}

dml_models <- function(lhs, type = "linear") {
  m <-
    paste(lhs, dml_fml(), sep = " ~ ") %>%
    as.formula %>%
    dml(regdata, type, n = dml_n, ml = "rf", workers = 8)

  return(list(m))
}


#===============================================================================
# table helper function
#===============================================================================
## ms is a list of the 4 model run programs
make_table <- function(lhs, ms, output_format = "latex") {
  results <-
    c(ms[[1]][lhs = lhs, fixef = 1],
      ms[[1]][lhs = lhs, fixef = 2],
      list(ms[[2]][[lhs]][[1]]),
      ms[[3]][lhs = lhs, fixef = 1],
      ms[[3]][lhs = lhs, fixef = 2],
      list(ms[[4]][[lhs]][[1]]))

  tbl <-
    regtable(results,
             est = c("Auction"),
             est_names = c("Auction"),
             extra_rows = list("Estimate" = rep(c("G10", "G20", "DML"), 2),
                               "Estimator" = c(rep("Linear", 3),
                                               rep("Poisson", 3))),
             n_obs = TRUE,
             stats = NA,
             stats_names = NA,
             decimals = 2,
             output_format = output_format)
  return(tbl)
}

#===============================================================================
# estimate FE models in levels (fast)
#===============================================================================
parcel_models_fe <-
  feols(..depvarspa ~ Auction + bs(Acres, df = 4) | ..fe, regdata)

parcel_models_pois_fe <-
  fepois(..depvars ~ Auction + bs(Acres, df = 4) | ..fe, regdata)

#===============================================================================
# estimate dml models (slow)
#===============================================================================
parcel_models_dml <-
  dml_pa_outcomes %>%
  map(dml_models)

parcel_models_pois_dml <-
  dml_outcomes %>%
  map(dml_models, type = "poisson")

#===============================================================================
# make and save table
#===============================================================================
main_parcel_outcomes <-
  seq(1, 4) %>%
  map(~ make_table(., list(parcel_models_fe, parcel_models_dml,
                           parcel_models_pois_fe, parcel_models_pois_dml),
                   output_format = "df")) %>%
  regtable_stack(table_names = c("Bonus", "Output",
                                 "Lease Revenue", "Seller Revenue"),
                 n_bottom = TRUE,
                 output_format = "latex")

writeLines(main_parcel_outcomes, file.path(tdir, "parcel_regressions.tex"))

#===============================================================================
# make parcel lease comparison table
# this will have 5 columns and four rows
# columns are lease level in sample + year, lease level all + year, lease level
# all no time control, lease level no time weighted by nparcels, parcel reg
# for ever leased sample, and then finally parcel reg for the whole sample
# rows are the outcomes
# do one table for grid10, the other for grid20
#===============================================================================
setFixest_fml(..depvarspa = ~ c(BonusPerAcre, DBOEPerAcre,
                                LeaseRevenuePerAcre, SellerRevenuePerAcre),
              ..depvars = ~ c(Bonus, DBOE,
                              LeaseRevenue, SellerRevenue),
              ..fe = ~sw(Grid10, Grid20),
              reset = TRUE)

ols_leases <-
  feols(..depvarspa ~ Auction + bs(Acres, df = 4) | ..fe,
        regdata_leases)

ols_parcels <- 
  feols(..depvarspa ~ Auction + bs(Acres, df = 4) | ..fe,
        filter(regdata, EverLeased))

pois_leases <-
  fepois(..depvars ~ Auction + bs(Acres, df = 4) | ..fe,
         regdata_leases)

pois_parcels <- 
  fepois(..depvars ~ Auction + bs(Acres, df = 4) | ..fe,
         filter(regdata, EverLeased))

dml_pl_model <- function(lhs, rd, type = "linear") {
  m <-
    paste(lhs, dml_fml(), sep = " ~ ") %>%
    as.formula %>%
    dml(rd, type, n = dml_n, ml = "rf", workers = 8)

  return(m)
}

ols_lease_dml <-
  dml_pa_outcomes %>%
  map(~ dml_pl_model(., regdata_leases))

ols_parcel_dml <-
  dml_pa_outcomes %>%
  map(~ dml_pl_model(., filter(regdata, EverLeased)))

poisson_lease_dml <-
  dml_outcomes %>%
  map(~ dml_pl_model(., regdata_leases, type = "poisson"))

poisson_parcel_dml <-
  dml_outcomes %>%
  map(~ dml_pl_model(., filter(regdata, EverLeased), type = "poisson"))

get_est <- function(m, lhs, fixef) {
  m[lhs=lhs, fixef=fixef]
}

# row of a table is g10, g20, dml (spec 3), g10, g20, dml (spec 5)
comparetbl <- function(cols) {
  temp <-
    cols %>%
    regtable(est = c("Auction"),
             est_names = c("Auction"),
             extra_rows = list("Estimate" = rep(c("G10", "G20", "DML"), 2),
                               "Sample" = c(rep("All Leases", 3),
                                            rep("Leased Parcels", 3))),
             n_obs = TRUE,
             stats = NA,
             stats_names = NA,
             output_format = "df")
  return(temp)
}

appendix_compare_linear <-
  regtable_stack(
    list(comparetbl(c(ols_leases[fixef=1, lhs=1],
                         ols_leases[fixef=2, lhs=1],
                         list(ols_lease_dml[[1]]),
                         ols_parcels[fixef=1, lhs=1],
                         ols_parcels[fixef=2, lhs=1],
                         list(ols_parcel_dml[[1]]))),
         comparetbl(c(ols_leases[fixef=1, lhs=2],
                         ols_leases[fixef=2, lhs=2],
                         list(ols_lease_dml[[2]]),
                         ols_parcels[fixef=1, lhs=2],
                         ols_parcels[fixef=2, lhs=2],
                         list(ols_parcel_dml[[2]]))),
         comparetbl(c(ols_leases[fixef=1, lhs=3],
                         ols_leases[fixef=2, lhs=3],
                         list(ols_lease_dml[[3]]),
                         ols_parcels[fixef=1, lhs=3],
                         ols_parcels[fixef=2, lhs=3],
                         list(ols_parcel_dml[[3]]))),
         comparetbl(c(ols_leases[fixef=1, lhs=4],
                         ols_leases[fixef=2, lhs=4],
                         list(ols_lease_dml[[4]]),
                         ols_parcels[fixef=1, lhs=4],
                         ols_parcels[fixef=2, lhs=4],
                         list(ols_parcel_dml[[4]])))),
    table_names = c("Bonus", "Output", "Lease Revenue", "Seller Revenue"),
    output_format = "latex")

appendix_compare_poisson <-
  regtable_stack(
    list(comparetbl(c(pois_leases[fixef=1, lhs=1],
                         pois_leases[fixef=2, lhs=1],
                         list(poisson_lease_dml[[1]]),
                         pois_parcels[fixef=1, lhs=1],
                         pois_parcels[fixef=2, lhs=1],
                         list(poisson_parcel_dml[[1]]))),
         comparetbl(c(pois_leases[fixef=1, lhs=2],
                         pois_leases[fixef=2, lhs=2],
                         list(poisson_lease_dml[[2]]),
                         pois_parcels[fixef=1, lhs=2],
                         pois_parcels[fixef=2, lhs=2],
                         list(poisson_parcel_dml[[2]]))),
         comparetbl(c(pois_leases[fixef=1, lhs=3],
                         pois_leases[fixef=2, lhs=3],
                         list(poisson_lease_dml[[3]]),
                         pois_parcels[fixef=1, lhs=3],
                         pois_parcels[fixef=2, lhs=3],
                         list(poisson_parcel_dml[[3]]))),
         comparetbl(c(pois_leases[fixef=1, lhs=4],
                         pois_leases[fixef=2, lhs=4],
                         list(poisson_lease_dml[[4]]),
                         pois_parcels[fixef=1, lhs=4],
                         pois_parcels[fixef=2, lhs=4],
                         list(poisson_parcel_dml[[4]])))),
    table_names = c("Bonus", "Output", "Lease Revenue", "Seller Revenue"),
    output_format = "latex")

writeLines(appendix_compare_linear,
           file.path(tdir, "lease_parcel_comparisons_linear.tex"))

writeLines(appendix_compare_poisson,
           file.path(tdir, "lease_parcel_comparisons_poisson.tex"))


