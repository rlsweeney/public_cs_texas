#==============================================================================
# code to estimate and format regression tables for lease outcomes which
# condition on royalty and term
# by thom in march 2021
#==============================================================================
library(tidyverse)
library(lubridate)
library(splines)
library(lmtest)
library(sandwich)
library(fixest)

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
# load lease level data, and work with outcomes:
# bonus, output, lease revenue, seller revenue
# per acre in linear models
# levels in poisson models
# same layout as main results parcel table
#===============================================================================
load(file.path(gen, "final_leases.Rda"))
regdata <-
  final_leases %>%
  filter(InSample) %>%
  mutate(Bonus = BonusPerAcre * Acres,
         DBOE = DBOEPerAcre * Acres,
         LeaseRevenue = LeaseRevenuePerAcre * Acres,
         SellerRevenue = SellerRevenuePerAcre * Acres)

#===============================================================================
# define specifications
#===============================================================================
# this reduces code complexity/duplication in the feols calls that follow
setFixest_fml(..outputdepvarspa = ~ c(LeaseRevenuePerAcre,
                                      DBOEPerAcre,
                                      SellerRevenuePerAcre),
              ..outputdepvars = ~ c(LeaseRevenue, DBOE, SellerRevenue),
              ..bonusvars = ~ c(BonusPerAcre, log(BonusPerAcre)),
              ..fe = ~sw(Grid10Yr + YearQtr, Grid20Yr + YearQtr))

dml_pa_outcomes <-
  c("LeaseRevenuePerAcre", "DBOEPerAcre", "SellerRevenuePerAcre")

dml_outcomes <-
  c("Bonus", "LeaseRevenue", "DBOE", "SellerRevenue")

dml_spec <- "Acres + CentLat + CentLong + EffDate + RoyaltyRate + Term"

dml_fml <- function(...) {
  left_fml <- "Auction"
  right_fml <- paste(dml_spec, ..., sep = " + ")
  fml <- paste(left_fml, right_fml, sep = " | ")
  return(fml)
}

dml_models <- function(lhs, type = "linear", data = regdata) {
  m_base <-
    paste(lhs, dml_fml(), sep = " ~ ") %>%
    as.formula %>%
    dml(data, type, n = dml_n, ml = "rf", workers = 8)

  return(list(m_base))
}


#===============================================================================
# table helper function
#===============================================================================
## ms is a list of the 4 model run programs
make_output_table <- function(lhs, ms, output_format = "latex") {
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
             extra_rows = list("Estimate" = rep(c("G10Y", "G20Y", "DML"), 2),
                               "Estimator" = c(rep("Linear", 3),
                                               rep("Poisson", 3))),
             n_obs = TRUE,
             stats = NA,
             stats_names = NA,
             decimals = 2,           
             output_format = output_format)
  return(tbl)
}

make_bonus_table <- function(ms, output_format = "latex") {
  results <-
    c(ms[[1]][lhs = 1, fixef = 1],
         ms[[1]][lhs = 1, fixef = 2],
         list(ms[[2]][[1]][[1]]),
         ms[[1]][lhs = 2, fixef = 1],
         ms[[1]][lhs = 2, fixef = 2],
         list(ms[[2]][[2]][[1]]))

  tbl <-
    regtable(results,
             est = c("Auction"),
             est_names = c("Auction"),
             extra_rows = list("Estimate" = rep(c("G10Y", "G20Y", "DML"), 2),
                               "Outcome" = c(rep("Bonus per acre", 3),
                                             rep("log(Bonus)", 3))),
             n_obs = TRUE,
             stats = NA,
             stats_names = NA,
             decimals = 2,           
             output_format = output_format)
  return(tbl)
}

#===============================================================================
# estimate FE models (fast)
#===============================================================================
output_linear_models_fe <-
  feols(..outputdepvarspa ~ Auction + bs(Acres, df = 4) +
          RoyaltyRate + Term | ..fe,
        filter(regdata, !Censored))

output_poisson_models_fe <-
  fepois(..outputdepvars ~ Auction + bs(Acres, df = 4) +
           RoyaltyRate + Term | ..fe,
         filter(regdata, !Censored))

bonus_models_fe <-
  feols(..bonusvars ~ Auction + bs(Acres, df = 4) + RoyaltyRate + Term | ..fe,
        regdata)

#===============================================================================
# estimate dml models (slow)
#===============================================================================
output_linear_models_dml <-
  dml_pa_outcomes %>%
  map(dml_models, data = filter(regdata, !Censored))

output_poisson_models_dml <-
  dml_outcomes %>%
  map(dml_models, type = "poisson", data = filter(regdata, !Censored))

bonus_models_dml <-
  c("BonusPerAcre", "log(BonusPerAcre)") %>%
  map(dml_models)

#===============================================================================
# save tables
#===============================================================================
extra_controls_bonus_tbl <-
  make_bonus_table(list(bonus_models_fe, bonus_models_dml),
                   output_format = "latex")
  
extra_controls_output_tbl <-
  seq(1, 3) %>%
  map(~ make_output_table(., list(output_linear_models_fe,
                                  output_linear_models_dml,
                                  output_poisson_models_fe,
                                  output_poisson_models_dml),
                          output_format = "df")) %>%
  regtable_stack(table_names = c("Lease Revenue", "Output", "Seller Revenue"),
                 n_bottom = FALSE,
                 output_format = "latex")

#===============================================================================
# save table to disk
#===============================================================================
writeLines(extra_controls_bonus_tbl,
           file.path(tdir, "lease_regressions_extra_bonus.tex"))

writeLines(extra_controls_output_tbl,
           file.path(tdir, "lease_regressions_extra_output.tex"))

