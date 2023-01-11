#==============================================================================
# code to estimate and format regression tables for drilled and output 
# conditional on drilling models
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
  filter(InSample)

#===============================================================================
# define specifications
#===============================================================================
vcov10 <- function(x) vcov(x, cluster = "Grid10", dof = dof(adj = FALSE))
vcov20 <- function(x) vcov(x, cluster = "Grid20", dof = dof(adj = FALSE))

# this reduces code complexity/duplication in the feols calls that follow
setFixest_fml(..depvars = ~ c(LogBonus, RoyaltyRate, Term, BonusPerAcre),
              ..basefe = ~sw(Grid10 + YearQtr,
                             Grid10Yr + YearQtr,
                             Grid10YrQtr,
                             Grid20Yr + YearQtr),
              ..Grid10Yr = ~ Grid10Yr + YearQtr)

# this reduces code complexity/duplication in the dml calls that follow
dml_spec <- "CentLat + CentLong + EffDate"

# functions to do the robust dml specs with a bit less boilerplate
dml_fml <- function(...) {
  left_fml <- "Auction"
  right_fml <- paste(dml_spec, "Acres", ..., sep = " + ")
  fml <- paste(left_fml, right_fml, sep = " | ")
  return(fml)
}

#===============================================================================
# table helper function
#===============================================================================
make_table <- function(l, output_format = "latex") {
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

#===============================================================================
# estimate FE drilled models (fast)
#===============================================================================
drilled_models_fe <-
  feols(Drilled ~ Auction + bs(Acres, df = 4) |
          ..basefe,
        filter(reg_data, !Censored))

#===============================================================================
# estimate FE models for log DBOE, conditional on drilled (fast)
#===============================================================================
logdboe_models_fe <-
  feols(log(DBOEPerAcre) ~ Auction + bs(Acres, df = 4) |
          ..basefe,
        filter(reg_data, !Censored, Drilled))

#===============================================================================
# estimate dml models (slow)
#===============================================================================
drilled_dml <-
  paste("Drilled", dml_fml(), sep = " ~ ") %>%
  as.formula %>%
  dml(., 
      filter(reg_data, !Censored), 
      "linear", 
      n = dml_n, 
      ml = "rf", 
      workers = 8)

logdboe_dml <-
  paste("log(DBOEPerAcre)", dml_fml(), sep = " ~ ") %>%
  as.formula %>%
  dml(., 
      filter(reg_data, !Censored, Drilled), 
      "linear", 
      n = dml_n, 
      ml = "rf", 
      workers = 8)

#===============================================================================
# make tables
#===============================================================================
drilled_tbl <-
  c(as.list(drilled_models_fe),
    list(drilled_dml)) %>%
  make_table

logdboe_tbl <-
  c(as.list(logdboe_models_fe),
    list(logdboe_dml)) %>%
  make_table

#===============================================================================
# save tables to disk
#===============================================================================
writeLines(drilled_tbl, file.path(tdir, "drilled_regressions.tex"))
writeLines(logdboe_tbl,
           file.path(tdir, "logdboe_drilled_regressions.tex"))
