#===============================================================================
# code to estimate and format regression tables for models that explore how much
# heterogeneity there is across TE's for leases of different sizes
#===============================================================================
library(tidyverse)
library(lubridate)
library(fixest)
library(splines)
library(lmtest)
library(sandwich)

#===============================================================================
# BASIC TEXAS SETUP
#===============================================================================
library(here)
root <- here()

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))
source(file.path(root, "code", "functions", "regtable.R"))
source(file.path(root, "code", "functions", "latex_number.R"))

#===============================================================================
# LOAD DATA CLEANED IN SAMPLE_SELECTION.R
#===============================================================================
load(file.path(gen, "final_leases.Rda"))

reg_data <-
  final_leases %>%
  filter(InSample) %>%
  mutate(LogBonus = log(BonusPerAcre * GrossAcres))

#===============================================================================
# define the covariance helper functions
#===============================================================================
vcov10 <- function(x) vcov(x, cluster = "Grid10", dof = dof(adj = FALSE))
vcov20 <- function(x) vcov(x, cluster = "Grid20", dof = dof(adj = FALSE))

#===============================================================================
# estimate bonus models that have heterogeneity in TEs across different sizes
#===============================================================================
median_negotiated_size <-
  reg_data %>%
  filter(Auction == 0) %>%
  with(median(Acres))

latex_number(median_negotiated_size * 1000,
             "negotiation_med_size",
             format = "f",
             big.mark = ",",
             digits = 0)

reg_data <-
  reg_data %>%
  mutate(Big = 1 * (Acres >= median_negotiated_size),
         BigNegotiation = Big * (1 - Auction))

size_het_models <-
  feols(log(BonusPerAcre) ~
          Auction + csw0(BigNegotiation) |
          sw(Grid10Yr + YearQtr, Grid20Yr + YearQtr),
        reg_data)

size_het_models_spline <-
  feols(LogBonus ~ Auction + BigNegotiation + bs(Acres, df = 4) |
          sw(Grid10Yr + YearQtr, Grid20Yr + YearQtr),
        reg_data)

size_het_tbl <-
  regtable(c(as.list(size_het_models)[c(1,2)],
             as.list(size_het_models_spline)[1],
             as.list(size_het_models)[c(3,4)],
             as.list(size_het_models_spline)[2]),
           est = c("Auction", "BigNegotiation"),
           extra_rows = list("CRS" =
                               rep(c("Yes", "Yes", "No"), 2),
                             "Estimate" =
                               c(rep("G10Y", 3), rep("G20Y", 3))),
           se_fun = list(vcov10, vcov10, vcov10, vcov20, vcov20, vcov20),
           n_obs = TRUE,
           stats = NA,
           stats_names = NA,
           decimals = 2,           
           output_format = "latex")

writeLines(size_het_tbl, file.path(tdir, "logbonus_size_heterogeneity.tex"))


