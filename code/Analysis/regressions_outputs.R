
#===============================================================================
# code to estimate and format regression tables for lease outcome models
# DBOE, LeaseRevenue, in linear/poisson specs, FE/dml
#===============================================================================
library(tidyverse)
library(lubridate)
library(splines)
library(lmtest)
library(sandwich)
library(fixest)

#===============================================================================
# BASIC TEXAS SETUP
#===============================================================================
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
  filter(InSample, !Censored) %>%
  mutate(DBOE = DBOEPerAcre * Acres,
         LeaseRevenue = LeaseRevenuePerAcre * Acres,
         SellerRevenue = SellerRevenuePerAcre * Acres)

#===============================================================================
# define specifications
#===============================================================================
vcov10 <- function(x) vcov(x, cluster = "Grid10", dof = dof(adj = FALSE))
vcov20 <- function(x) vcov(x, cluster = "Grid20", dof = dof(adj = FALSE))

# this reduces code complexity/duplication in the feols calls that follow
setFixest_fml(..depvars = ~ c(LeaseRevenue, DBOE, SellerRevenue),
              ..depvarspa = ~ c(LeaseRevenuePerAcre,
                                DBOEPerAcre,
                                SellerRevenuePerAcre),
              ..fe = ~sw(Grid10 + YearQtr,
                         Grid10Yr + YearQtr,
                         Grid10YrQtr,
                         Grid20Yr + YearQtr),
              ..g10y = ~ Grid10Yr + YearQtr,
              ..extra = c(parcel_characteristics_1,
                          parcel_characteristics_2,
                          "MultiPolygon",
                          "ShaleThickness"))

# this reduces code complexity/duplication in the dml calls that follow
dml_spec <- "CentLat + CentLong + EffDate + Acres"
dml_outcomes_pa <-
  list("LeaseRevenuePerAcre", "DBOEPerAcre", "SellerRevenuePerAcre")

dml_outcomes <- list("LeaseRevenue", "DBOE", "SellerRevenue")

# functions to do the robust dml specs with a bit less boilerplate
dml_fml <- function(...) {
  left_fml <- "Auction"
  right_fml <- paste(dml_spec, ..., sep = " + ")
  fml <- paste(left_fml, right_fml, sep = " | ")
  return(fml)
}

#===============================================================================
# table helper function
#===============================================================================
make_table <- function(l, output_format = "latex",
                       stats = c("r.squared"), stats_names = c("$R^2$")) {
    regtable(l, est = "Auction",
             extra_rows = list("Grid" =
                                 c("10", "10", "10", "20", "DML", "10"),
                               "Time" =
                                 c("Q", "GY,Q", "GYQ", "GY,Q", "DML", "GY,Q"),
                               "Extra" = c(rep("No", 5), "Yes")),
             n_obs = TRUE,
             stats = stats,
             stats_names = stats_names,
             se_fun = list(vcov10, vcov10, vcov10, vcov20, vcov, vcov10),
             decimals = 2,
             output_format = output_format)
}

#===============================================================================
# estimate FE models (fast)
#===============================================================================
# main output table models
linear_models_fe <-
  feols(..depvarspa ~ Auction + bs(Acres, df = 4) | ..fe,
        reg_data)

linear_models_extra_fe <-
  feols(..depvarspa ~ Auction + bs(Acres, df = 4) + ..extra | ..g10y,
        reg_data)

poisson_models_fe <-
  fepois(..depvars ~ Auction + bs(Acres, df = 4) | ..fe, reg_data)

poisson_models_extra_fe <-
  fepois(..depvars ~ Auction + bs(Acres, df = 4) + ..extra | ..g10y,
        reg_data)

#===============================================================================
# estimate dml models (slow)
#===============================================================================
linear_models_dml <-
  dml_outcomes_pa %>%
  map(~ paste(., dml_fml(), sep = " ~ ")) %>%
  map(as.formula) %>%
  map(~ dml(., reg_data, "linear", n = dml_n, ml = "rf", workers = 8))

poisson_models_dml <-
  dml_outcomes %>%
  map(~ paste(., dml_fml(), sep = " ~ ")) %>%
  map(as.formula) %>%
  map(~ dml(., reg_data, "poisson", n = dml_n, ml = "rf", workers = 8))


#===============================================================================
# compute some numbers we refer to in the text
#===============================================================================
negotiation_avg_revenue <-
  with(filter(reg_data, Auction == 0), 1000 * mean(LeaseRevenuePerAcre))

negotiation_avg_acres <-
  with(filter(reg_data, Auction == 0), 1000 * mean(Acres))

latex_number(negotiation_avg_revenue,
             "negotiation_avg_revenue",
             format = "f",
             big.mark = ",",
             digits = 0)

negotiation_avg_dboe <-
  with(filter(reg_data, Auction == 0), 1000 * mean(DBOEPerAcre))
latex_number(negotiation_avg_dboe,
             "negotiation_avg_dboe",
             format = "f",
             big.mark = ",",
             digits = 0)

seller_rev_mod <- linear_models_fe[lhs = 3, fixef = 2][[1]]
latex_number(round(coeftest(seller_rev_mod)["Auction", 1] *
                   1000 * negotiation_avg_acres, -3),
             "SellerRevenue_Grid10Yr_total",
             format = "f", big.mark = ",", digits = 0)

lease_rev_mod <- poisson_models_fe[lhs = 1, fixef = 2][[1]]
latex_number(coeftest(lease_rev_mod)["Auction", 1] * 100,
             "Poisson_LeaseRevenue_Grid10Yr",
             format = "f", big.mark = ",", digits = 0)

#===============================================================================
# make tables
#===============================================================================
# "main" tables
linear_leaserev_tbl <-
  c(as.list(linear_models_fe[lhs = 1]),
    list(linear_models_dml[[1]]),
    linear_models_extra_fe[lhs=1]) %>%
  make_table(output_format = "df", stats = NA, stats_names = NA)

linear_dboe_tbl <-
  c(as.list(linear_models_fe[lhs = 2]),
    list(linear_models_dml[[2]]),
    linear_models_extra_fe[lhs=2]) %>%
  make_table(output_format = "df", stats = NA, stats_names = NA)

linear_sellerrev_tbl <-
  c(as.list(linear_models_fe[lhs = 3]),
    list(linear_models_dml[[3]]),
    linear_models_extra_fe[lhs = 3]) %>%
  make_table(output_format = "df", stats = NA, stats_names = NA)


linear_output_tbl <-
  list(linear_leaserev_tbl, linear_dboe_tbl, linear_sellerrev_tbl) %>%
  regtable_stack(table_names = c("Lease Revenue", "Output", "Seller Revenue"),
                 n_bottom = TRUE,
                 output_format = "latex")

poisson_leaserev_tbl <-
  c(as.list(poisson_models_fe[lhs = 1]),
    list(poisson_models_dml[[1]]),
    poisson_models_extra_fe[lhs=1]) %>%
  make_table(output_format = "df", stats = NA, stats_names = NA)

poisson_dboe_tbl <-
  c(as.list(poisson_models_fe[lhs = 2]),
    list(poisson_models_dml[[2]]),
    poisson_models_extra_fe[lhs=2]) %>%
  make_table(output_format = "df", stats = NA, stats_names = NA)

poisson_sellerrev_tbl <-
  c(as.list(poisson_models_fe[lhs = 3]),
    list(poisson_models_dml[[3]]),
    poisson_models_extra_fe[lhs = 3]) %>%
  make_table(output_format = "df", stats = NA, stats_names = NA)


poisson_output_tbl <-
  list(poisson_leaserev_tbl, poisson_dboe_tbl, poisson_sellerrev_tbl) %>%
  regtable_stack(table_names = c("Lease Revenue", "Output", "Seller Revenue"),
                 n_bottom = TRUE,
                 output_format = "latex")

# compute range of point estimate values for each of:
auction_range <- function(ms) {
  coefficient_vals <-
    ms %>%
    map(coef) %>%
    map_dbl(~ pluck(., "Auction"))

  return(c(min(coefficient_vals), max(coefficient_vals)))
}

# lease rev/ac
lease_rev_linear_range <- 
  c(as.list(linear_models_fe[lhs = 1]),
    list(linear_models_dml[[1]]),
    linear_models_extra_fe[lhs=1]) %>%
  auction_range

latex_number(round(lease_rev_linear_range[1] * 1000, 0),
             "lease_rev_min",
             format = "f", big.mark = ",", digits = 0)

latex_number(round(lease_rev_linear_range[2] * 1000, 0),
             "lease_rev_max",
             format = "f", big.mark = ",", digits = 0)

# output/ac
dboe_linear_range <- 
  c(as.list(linear_models_fe[lhs = 2]),
    list(linear_models_dml[[2]]),
    linear_models_extra_fe[lhs=2]) %>%
  auction_range

latex_number(round(dboe_linear_range[1] * 100, 0),
             "dboe_min",
             format = "f", big.mark = ",", digits = 0)

latex_number(round(dboe_linear_range[2] * 100, 0),
             "dboe_max",
             format = "f", big.mark = ",", digits = 0)

# range of lease rev and output in poisson models
poisson_range <-
  c(as.list(poisson_models_fe[lhs = 1]),
    list(poisson_models_dml[[1]]),
    poisson_models_extra_fe[lhs=1],
    as.list(poisson_models_fe[lhs = 2]),
    list(poisson_models_dml[[2]]),
    poisson_models_extra_fe[lhs=2]) %>%
  auction_range

latex_number(round(poisson_range[1] * 100, 0),
             "poisson_output_min",
             format = "f", big.mark = ",", digits = 0)

latex_number(round(poisson_range[2] * 100, 0),
             "poisson_output_max",
             format = "f", big.mark = ",", digits = 0)

# total seller revenues
linear_sellerrev_range <-
  c(as.list(linear_models_fe[lhs = 3]),
    list(linear_models_dml[[3]]),
    linear_models_extra_fe[lhs = 3]) %>%
  auction_range

latex_number(round(linear_sellerrev_range[1] * 1000, 0),
             "sellerrev_min",
             format = "f", big.mark = ",", digits = 0)

latex_number(round(linear_sellerrev_range[2] * 1000, 0),
             "sellerrev_max",
             format = "f", big.mark = ",", digits = 0)


#===============================================================================
# save tables to disk
#===============================================================================
writeLines(linear_output_tbl, file.path(tdir, "stacked_output_levels.tex"))
writeLines(poisson_output_tbl, file.path(tdir, "stacked_output_poisson.tex"))
