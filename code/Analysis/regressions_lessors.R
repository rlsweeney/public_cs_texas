#===============================================================================
# code to estimate and format regression tables for models that explore how much
# heterogeneity there is across *lessors*
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

#===============================================================================
# classify lessors along potentially interesting margins
# - actual lessor name: how many leases have they sold
# - lessor name indicates they are in the busness
# - lessor name indicates they are a company not an individual
#===============================================================================

# first make some regexes
rFirm <- 
  regex("inc|ltd|llc|corp|asset|assoc|lp|company|holdings|partner|manag", 
  ignore_case = T)
rEP <- regex("oil|gas|production|explor|mineral", ignore_case = T)

ral_lessors <-
    final_leases %>%
    filter(LeaseType == "RAL", 
            OnShale, 
            year(Effective_Date) >= 2000,
            !is.na(Lessor),
            !TexasLessor) %>%
    group_by(Lessor) %>%
    summarize(NLessorLeases = n()) %>%
    mutate(LessorIsFirm = str_detect(Lessor, rFirm),
            LessorIsEP = str_detect(Lessor, rEP),
            LessorExperience5 = NLessorLeases >= 5,
            LessorExperience10 = NLessorLeases >= 10)

# merge the flags into final_leases and build reg_data
reg_data <-
  final_leases %>%
  filter(InSample) %>%
  mutate(LogBonus = log(BonusPerAcre * GrossAcres)) %>%
  left_join(ral_lessors, by = "Lessor") %>%
  replace_na(list(LessorIsFirm = FALSE, LessorIsEP = FALSE, 
                  LessorExperience5 = FALSE,
                  LessorExperience10 = FALSE)) %>%
  mutate(across(c(LessorIsFirm, LessorIsEP, 
                  LessorExperience5, LessorExperience10), ~ . * 1)) %>%
  group_by(Lessor, NewFirm) %>%
  mutate(Relationship = n() > 1,
         NegRelationship = (1-Auction) * Relationship) %>%
  ungroup


#===============================================================================
# define and run some specs
#===============================================================================
vcov10 <- function(x) vcov(x, cluster = "Grid10", dof = dof(adj = FALSE))
vcov20 <- function(x) vcov(x, cluster = "Grid20", dof = dof(adj = FALSE))

# this reduces code complexity/duplication in the feols calls that follow
setFixest_fml(..basefe = ~sw(Grid10Yr + YearQtr, Grid20Yr + YearQtr),
              ..hetero = ~sw(LessorExperience5, LessorExperience10,
                              LessorIsFirm, LessorIsEP))

# table helper function
make_table <- function(l, output_format = "latex") {
    regtable(l, est = c("Auction", "LessorExperience5", "LessorExperience10",
                        "LessorIsFirm", "LessorIsEP"),
             extra_rows = list("Grid" = c(rep("10", 4), rep("20", 4)),
                               "Time" = rep("GY,Q", 8)),
             n_obs = TRUE,
             stats = c("r.squared"),
             stats_names = c("$R^2$"),
             se_fun = c(rep(list(vcov10), 4), rep(list(vcov20), 4)),
             decimals = 2,
             output_format = output_format)
}

logbonus_models_with_lessor_heterogeneity <-
  feols(LogBonus ~ Auction + ..hetero + bs(Acres, df = 4) |
          ..basefe,
        reg_data)

logbonus_lessor_heterogeneity_tbl <-
  as.list(logbonus_models_with_lessor_heterogeneity) %>%
  make_table

writeLines(logbonus_lessor_heterogeneity_tbl, 
  file.path(tdir, "logbonus_regressions_lessor_heterogeneity.tex"))



