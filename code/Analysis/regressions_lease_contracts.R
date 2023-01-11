#==============================================================================
# code to estimate and format regression tables for lease bonus, royalty and
# term models
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
# LOAD DATA CLEANED IN lease_selection.R
#===============================================================================
load(file.path(gen, "final_leases.Rda"))

reg_data <-
  final_leases %>%
  filter(InSample) %>%
  mutate(RoyaltyRate = RoyaltyRate * 100,
         LogBonus = log(BonusPerAcre * GrossAcres))

#===============================================================================
# define specifications
#===============================================================================
extra_controls <-
  paste(paste0(parcel_characteristics_1, collapse = " + "),
        paste0(parcel_characteristics_2, collapse = " + "),
        "MultiPolygon",
        sep = " + ")

vcov10 <- function(x) vcov(x, cluster = "Grid10", dof = dof(adj = FALSE))
vcov20 <- function(x) vcov(x, cluster = "Grid20", dof = dof(adj = FALSE))

# this reduces code complexity/duplication in the feols calls that follow
setFixest_fml(..depvars = ~ c(LogBonus, RoyaltyRate, Term, BonusPerAcre),
              ..basefe = ~sw(Grid10 + YearQtr,
                             Grid10Yr + YearQtr,
                             Grid10YrQtr,
                             Grid20Yr + YearQtr),
              ..Grid10Yr = ~ Grid10Yr + YearQtr,
              ..extra = c(parcel_characteristics_1,
                          parcel_characteristics_2,
                          "MultiPolygon"))

# this reduces code complexity/duplication in the dml calls that follow
dml_spec <- "CentLat + CentLong + EffDate"
dml_outcomes <-
  list("LogBonus", "RoyaltyRate", "Term", "BonusPerAcre")

# functions to do the robust dml specs with a bit less boilerplate
dml_fml <- function(...) {
  left_fml <- "Auction"
  right_fml <- paste(dml_spec, "Acres", ..., sep = " + ")
  fml <- paste(left_fml, right_fml, sep = " | ")
  return(fml)
}

robust_lease_contract_models_dml <- function(lhs) {
  m_extra <-
    paste(lhs, dml_fml(extra_controls), sep = " ~ ") %>%
    as.formula %>%
    dml(reg_data, "linear", n = dml_n, ml = "rf", workers = 8)
  
  m_thick <-
    paste(lhs, dml_fml(extra_controls, "ShaleThickness"), sep = " ~ ") %>%
    as.formula %>%
    dml(filter(reg_data, !is.na(ShaleThickness)),
        "linear", n = dml_n, ml = "rf", workers = 8)
  
  m_private <-
    paste(lhs, dml_fml(), sep = " ~ ") %>%
    as.formula %>%
    dml(filter(reg_data, PrivateSurface),
        "linear", n = dml_n, ml = "rf", workers = 8)
   return(list(m_extra, m_thick, m_private))
}

#===============================================================================
# table helper functions
#===============================================================================
make_rt_table <- function(l, output_format = "latex") {
    regtable(l, est = "Auction",
             extra_rows = list("Grid" =
                                 c("10", "10", "10", "20", "DML"),
                               "Time" =
                                 c("Q", "GY,Q", "GYQ", "GY,Q", "DML")),
             n_obs = TRUE,
             stats = c("r.squared"),
             stats_names = c("$R^2$"),
             se_fun = list(vcov10, vcov10, vcov10, vcov20, vcov),
             decimals = 2,
             output_format = output_format)
}

make_bonus_table <- function(ms, output_format = "latex") {
  glabels <- c("10", "10", "10", "20", "DML", "10", "DML", "10", "DML")
  tlabels <- c("Q", "GY,Q", "GYQ", "GY,Q", "DML", "GY,Q", "DML", "GY,Q", "DML")
  vclist <- 
    list(vcov10, vcov10, vcov10, vcov20, vcov, vcov10, vcov, vcov10, vcov)

  tbl <-
    regtable(ms,
             est = c("Auction"),
             est_names = c("Auction"),
             extra_rows = list("Grid" = glabels, "Time" = tlabels,
                               "Extra" =
                                 c(rep("No", 5), rep("Yes", 2), rep("No", 2)),
                               "Private Only" =
                                 c(rep("No", 7), rep("Yes", 2))),
             n_obs = TRUE,
             stats = c("r.squared"),
             stats_names = c("$R^2$"),
             se_fun = vclist,
             decimals = 2,           
             output_format = output_format)
  return(tbl)
}
#===============================================================================
# compute some numbers we refer to in the text
#===============================================================================
negotiation_avg_bonus <-
  with(filter(reg_data, Auction == 0), 1000 * mean(BonusPerAcre))
latex_number(negotiation_avg_bonus,
             "negotiation_avg_bonus",
             format = "f",
             big.mark = ",",
             digits = 0)

negotiation_avg_bonus_payment <-
  with(filter(reg_data, Auction == 0), mean(BonusPerAcre * GrossAcres * 1000))

m_logbonus_Grid10 <-
  feols(LogBonus ~
          Auction + bs(Acres, df = 4) |
          Grid10 + YearQtr, reg_data)

m_logbonus_Grid10Yr <-
  feols(LogBonus ~ Auction + bs(Acres, df = 4) |
          Grid10Yr + YearQtr, reg_data)

latex_number(coeftest(m_logbonus_Grid10)["Auction", 1] * 100,
             "Bonus_Grid10_log",
             format = "f", big.mark = ",", digits = 0)

latex_number(coeftest(m_logbonus_Grid10Yr)["Auction", 1] * 100,
             "Bonus_Grid10Yr_log",
             format = "f", big.mark = ",", digits = 0)

latex_number(round((exp(coeftest(m_logbonus_Grid10Yr)["Auction", 1]) - 1) *
                     negotiation_avg_bonus_payment, -3),
             "Bonus_Grid10Yr_log_total",
             format = "f", big.mark = ",", digits = 0)

#===============================================================================
# estimate FE models (fast)
#===============================================================================
main_lease_contract_models_fe <-
  feols(..depvars ~ Auction + bs(Acres, df = 4) |
          ..basefe,
        reg_data)

# note: this estimates a bunch of models we don't necessarily need, including
# a few that are not identified (PrivateSurface = FALSE, for example), but the
# notation is quite compact and it is easy to subset the relevant stuff ex post
robust_lease_contract_models_fe <-
  feols(..depvars ~ Auction + bs(Acres, df = 4) +
          csw0(..extra, ShaleThickness) |
          ..Grid10Yr,
        reg_data, fsplit = ~ PrivateSurface)

#===============================================================================
# estimate dml models (slow)
#===============================================================================
main_lease_contract_models_dml <-
  dml_outcomes %>%
  map(~ paste(., dml_fml(), sep = " ~ ")) %>%
  map(as.formula) %>%
  map(~ dml(., reg_data, "linear", n = dml_n, ml = "rf", workers = 8))

robust_lease_contract_models_dml <-
  dml_outcomes %>%
  map(robust_lease_contract_models_dml)

#===============================================================================
# make tables
#===============================================================================
# "main" tables
logbonus_tbl <-
  c(as.list(main_lease_contract_models_fe[lhs = 1]),
    list(main_lease_contract_models_dml[[1]]),
    robust_lease_contract_models_fe[sample = 1, lhs = 1, rhs = 3],
    list(robust_lease_contract_models_dml[[1]][[2]]),
    robust_lease_contract_models_fe[sample = 3, lhs = 1, rhs = 1],
    list(robust_lease_contract_models_dml[[1]][[3]])) %>%
  make_bonus_table

bonus_tbl <-
  c(as.list(main_lease_contract_models_fe[lhs = 4]),
    list(main_lease_contract_models_dml[[4]]),
    robust_lease_contract_models_fe[sample = 1, lhs = 4, rhs = 3],
    list(robust_lease_contract_models_dml[[4]][[2]]),
    robust_lease_contract_models_fe[sample = 3, lhs = 4, rhs = 1],
    list(robust_lease_contract_models_dml[[4]][[3]])) %>%
  make_bonus_table

royalty_stack <-
  c(as.list(main_lease_contract_models_fe[lhs = 2]),
    list(main_lease_contract_models_dml[[2]])) %>%
  make_rt_table(output_format = "df")

term_stack <-
  c(as.list(main_lease_contract_models_fe[lhs = 3]),
    list(main_lease_contract_models_dml[[3]])) %>%
  make_rt_table(output_format = "df")

royalty_term_tbl <-
  list(royalty_stack, term_stack) %>%
  regtable_stack(table_names = c("Royalty Rate", "Primary Term"),
                 n_bottom = TRUE,
                 output_format = "latex")


#===============================================================================
# save tables to disk
#===============================================================================
writeLines(bonus_tbl, file.path(tdir, "bonus_regressions.tex"))
writeLines(logbonus_tbl, file.path(tdir, "logbonus_regressions.tex"))
writeLines(royalty_term_tbl, file.path(tdir, "royalty_term_regressions.tex"))
