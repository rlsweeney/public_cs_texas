#==============================================================================
# code to estimate and format regression tables for RAL addenda models
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
source(file.path(root, "code", "functions", "load_crossfit.R"))

#==============================================================================
# LOAD DATA CLEANED IN SAMPLE_SELECTION.R
#==============================================================================
load(file.path(ddir, "generated_data/final_leases.Rda"))

reg_data <-
  final_leases %>%
  filter(InSample) %>%
  filter(Year >= 2005) %>%
  mutate(LogBonus = log(BonusPerAcre * GrossAcres))

#==============================================================================
# estimate models
#==============================================================================
m_Grid10Yr <-
  feols(LogBonus ~ Auction + bs(Acres, df = 4) | Grid10Yr + YearQtr,
        reg_data, cluster = ~ Grid10)

m_Grid10Yr_PgCl <-
  feols(LogBonus ~ Auction + bs(Acres, df = 4) +
          AddendaPages + AddendaClauses | Grid10Yr + YearQtr,
        reg_data, cluster = ~ Grid10)

m_Grid10Yr_Addenda <-
  feols(LogBonus ~ Auction + bs(Acres, df = 4) +
          c01 + c02 + c03 + c04 + c05 + c07 + c08 + c09 + c10 + c11 + c12 +
          c13 + c14 + c15 + c16 | Grid10Yr + YearQtr,
        reg_data, cluster = ~ Grid10)

m_Grid10Yr_output <-
  feols(DBOEPerAcre ~ Auction + bs(Acres, df = 4) | Grid20Yr + YearQtr,
        filter(reg_data, !Censored), cluster = ~ Grid20)

m_Grid10Yr_PgCl_output <-
  feols(DBOEPerAcre ~ Auction + bs(Acres, df = 4) +
          AddendaPages + AddendaClauses | Grid20Yr + YearQtr,
        filter(reg_data, !Censored), cluster = ~ Grid20)

m_Grid10Yr_Addenda_output <-
  feols(DBOEPerAcre ~ Auction + bs(Acres, df = 4) +
          c01 + c02 + c03 + c04 + c05 + c07 + c08 + c09 + c10 + c11 + c12 +
          c13 + c14 + c15 + c16 | Grid20Yr + YearQtr,
        filter(reg_data, !Censored), cluster = ~ Grid20)

m_dml <-
  dml(LogBonus ~ Auction | Acres + CentLat + CentLong + EffDate,
      reg_data, "linear", n = dml_n, ml = "rf", workers = 8)

m_dml_PgCl <-
  dml(LogBonus ~ Auction | Acres + CentLat + CentLong + EffDate +
        AddendaPages + AddendaClauses,
      reg_data, "linear", n = dml_n, ml = "rf", workers = 8)

m_dml_Addenda <-
  dml(LogBonus ~ Auction | Acres + CentLat + CentLong + EffDate +
        c01 + c02 + c03 + c04 + c05 + c07 + c08 + c09 + c10 + c11 + c12 +
        c13 + c14 + c15 + c16,
      reg_data, "linear", n = dml_n, ml = "rf", workers = 8)

m_dml_output <-
  dml(DBOEPerAcre ~ Auction | Acres + CentLat + CentLong + EffDate,
      filter(reg_data, !Censored), "linear", n = dml_n, ml = "rf", workers = 8)

m_dml_PgCl_output <-
  dml(DBOEPerAcre ~ Auction | Acres + CentLat + CentLong + EffDate +
        AddendaPages + AddendaClauses,
      filter(reg_data, !Censored), "linear", n = dml_n, ml = "rf", workers = 8)

m_dml_Addenda_output <-
  dml(DBOEPerAcre ~ Auction | Acres + CentLat + CentLong + EffDate +
        c01 + c02 + c03 + c04 + c05 + c07 + c08 + c09 + c10 + c11 + c12 +
        c13 + c14 + c15 + c16,
      filter(reg_data, !Censored), "linear", n = dml_n, ml = "rf", workers = 8)


#==============================================================================
# make tables
#==============================================================================
bonus_addenda_tbl <-
  list(m_Grid10Yr,
       m_dml,
       m_Grid10Yr_PgCl,
       m_dml_PgCl,
       m_Grid10Yr_Addenda,
       m_dml_Addenda) %>%
  regtable(.,
           est = c("Auction"),
           est_names = c("Auction"), 
           extra_rows = list("Addenda Controls" =
                               c(rep("None", 2),
                                 rep("Pages + Clauses", 2),
                                 rep("Individual", 2)),
                             "Grid" =
                               c("10", "DML", "10", "DML", "10", "DML"),
                             "Time" =
                               c("GY,Q", "DML", "GY,Q", "DML", "GY,Q", "DML")),
           n_obs = TRUE,
           stats = NA,
           stats_names = NA,
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "df")

output_addenda_tbl <-
  list(m_Grid10Yr_output,
       m_dml_output,
       m_Grid10Yr_PgCl_output,
       m_dml_PgCl_output,
       m_Grid10Yr_Addenda_output,
       m_dml_Addenda_output) %>%
  regtable(.,
           est = c("Auction"),
           est_names = c("Auction"), 
           extra_rows = list("Addenda Controls" =
                               c(rep("None", 2),
                                 rep("Pages + Clauses", 2),
                                 rep("Individual", 2)),
                             "Grid" =
                               c("10", "DML", "10", "DML", "10", "DML"),
                             "Time" =
                               c("GY,Q", "DML", "GY,Q", "DML", "GY,Q", "DML")),
           n_obs = TRUE,
           stats = NA,
           stats_names = NA,
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "df")

addenda_tbl <-
  list(bonus_addenda_tbl, output_addenda_tbl) %>%
  regtable_stack(table_names = c("log(Bonus)", "Output"),
                 n_bottom = FALSE,
                 output_format = "latex")


addenda_tbl_prez <-
  list(m_Grid10Yr,
       m_dml,
       m_Grid10Yr_PgCl,
       m_dml_PgCl,
       m_Grid10Yr_Addenda,
       m_dml_Addenda) %>%
  regtable(.,
           est = c("Auction"),
           est_names = c("Auction"),
           extra_rows = list("Grid" =
                               c("10", "DML", "10", "DML", "10", "DML"),
                             "Time" =
                               c("GY,Q", "DML", "GY,Q", "DML", "GY,Q", "DML"),
                             "Addenda" = c(" ", " ",
                                           rep("Pages, Clauses", 2),
                                           rep("Clause FE", 2))),
           n_obs = TRUE,
           stats = c("r.squared"),
           stats_names = c("$R^2$"),
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "latex")

writeLines(addenda_tbl,
           file.path(tdir, "stacked_regressions_addenda.tex"))
writeLines(addenda_tbl_prez,
           file.path(tdir, "bonus_regressions_addenda_prez.tex"))


