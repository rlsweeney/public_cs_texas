#==============================================================================
# code to estimate and format regression tables for within-firm models
#==============================================================================
library(tidyverse)
library(lubridate)
library(fixest)
library(splines)
library(lmtest)
library(sandwich)

#==============================================================================
# BASIC TEXAS SETUP
#==============================================================================
library(here)
root <- here()

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))
source(file.path(root, "code", "functions", "regtable.R"))

#==============================================================================
# LOAD DATA CLEANED IN SAMPLE_SELECTION.R
#==============================================================================
load(file.path(gen, "final_leases.Rda"))

reg_data <-
  final_leases %>%
  filter(InSample) %>%
  mutate(LogBonus = log(BonusPerAcre * Acres))


#==============================================================================
# estimate models (include some specs already in bonus regs for presentation)
#==============================================================================
ms_logbonus <-
  feols(LogBonus ~ Auction + bs(Acres, df = 4) |
          Grid10Yr + YearQtr + sw0(NewFirm), reg_data, cluster = ~ Grid10)

ms_output <-
  feols(c(DBOEPerAcre, LeaseRevenuePerAcre) ~ Auction + bs(Acres, df = 4) |
          Grid10Yr + YearQtr + sw0(NewFirm), filter(reg_data, !Censored),
        cluster = ~ Grid10)

#==============================================================================
# make tables
#==============================================================================
firms_tbl <-
  c(as.list(ms_logbonus),
    as.list(ms_output[lhs=1:.N, fixef=1:.N])) %>%
  regtable(., est = "Auction",
           mnames = c(rep("Bonus", 2), rep("Output", 2), rep("Revenue", 2)),
           extra_rows = list("Firm FE" = rep(c("No", "Yes"), 3)),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           decimals = c(2, 2, 3, 3, 2, 2),
           output_format = "latex")

writeLines(firms_tbl, file.path(tdir, "firms_regressions.tex"))

