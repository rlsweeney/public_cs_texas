# compute some basic summary statistics and figures

#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
library(here)
root <- here()
source(file.path(root, "code/paths.R"))

library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)
library(broom)

source(file.path(root, "code", "texas_constants.R"))
source(file.path(root, "code", "functions", "latex_number.R"))

# SAMPLE DEFINED IN sample_selection.R
load(file.path(gen, "final_leases.Rda"))

lease_data <- 
  final_leases %>%
  filter(ValidLease, !flag_early, !flag_late)

# report how many of each lease type t there are
save_sample_size <- function(df, tlabel) {
  df %>%
    nrow %>%
    latex_number(paste("Nsample_", tlabel, sep = ""),
                 format = "f", big.mark = ",", digits = 0)
}

# save sample sizes before filtering to in sample
lease_data %>%
  filter(LeaseType == "RAL") %>%
  save_sample_size("RAL_RAW")

lease_data %>%
  filter(LeaseType == "STATE") %>%
  save_sample_size("STATE_RAW")

# then do the in sample filters
lease_data %>%
  filter(InSample, Auction == 0) %>%
  save_sample_size("NEGOTIATION_CLEAN")

lease_data %>%
  filter(InSample, Auction == 1) %>%
  save_sample_size("AUCTION_CLEAN")

# COHORTS OVER TIME FIGURE ----------------------------------------------------
# how much data do we have and what time periods does it cover?
cohorts_data <-
  lease_data %>%
  filter(InSample) %>%
  mutate(Year = factor(year(Effective_Date)),
         Mechanism = if_else(Auction == 1, "Auction", "Negotiation")) %>%
  mutate(Mechanism = factor(Mechanism),
         Mechanism = relevel(Mechanism, "Negotiation")) %>%
  select(Lease_Number, Year, Mechanism)

cohorts <-
  cohorts_data %>%
  ggplot(aes(Year)) +
  geom_bar(aes(fill = Mechanism)) +
  ylab("Leases") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_fill_manual(values = c("#a50026", "#74add1"))

ggsave(file.path(fdir, "cohorts.png"),
       height = 2.75, width = 6)

ggsave(file.path(fdir, "cohorts.pdf"),
       height = 2.75, width = 6)

# make a presentation version with the legend in the figure and no legend title
cohorts_prez <-
  cohorts_data %>%
  ggplot(aes(Year)) +
  geom_bar(aes(fill = Mechanism)) +
  ylab("Leases") +
  xlab(NULL) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = c(0.75, 0.75),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1.0)),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent")) +
  scale_fill_manual(values = c("red", "blue"))

ggsave(file.path(fdir, "cohorts_prez.png"),
       height = 2.25, width = 7,
       bg = "transparent")

# CREATE SUMMARY STATISTICS TABLE  ---------------------------------------------
lease_data %>%
  filter(InSample, Auction == 1, is.na(ShaleThickness)) %>%
  save_sample_size(tlabel = "AUCTION_NOTHICK")

lease_data %>%
  filter(InSample, Auction == 0, is.na(ShaleThickness)) %>%
  save_sample_size(tlabel = "NEGOTIATION_NOTHICK")

sumvars <-
  lease_data %>%
  filter(InSample) %>%
  select(Bonus = BonusPerAcre,
         RoyaltyRate,
         Term,
         LeaseRevenue = LeaseRevenuePerAcre,
         SellerRevenue = SellerRevenuePerAcre,
         Output = DBOEPerAcre,
         Acres,
         Auction,
         ShapeQuality,
         MultiPolygon,
         ShaleThickness)

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
  lease_data %>%
  filter(InSample, Auction == 0) %>%
  nrow %>%
  paste("Negotiation (N = ", ., ")", sep = "")

n_Auction <-
  lease_data %>%
  filter(InSample, Auction == 1) %>%
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
           variable == "MultiPolygon" ~ 3,
           variable == "ShaleThickness" ~ 4,
           variable == "Bonus" ~ 5,
           variable == "Term" ~ 6,
           variable == "RoyaltyRate" ~ 7,
           variable == "Output" ~ 8,
           variable == "LeaseRevenue" ~ 9,
           variable == "SellerRevenue" ~ 10,
           TRUE ~ NA_real_)) %>%
  mutate(variable = case_when(
           variable == "ShapeQuality" ~ "Shape Quality",
           variable == "MultiPolygon" ~ "Multiple Parcels",
           variable == "ShaleThickness" ~ "Shale Thickness",
           variable == "RoyaltyRate" ~ "Royalty Rate",
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
  group_rows("Land Characteristics", 1, 4) %>%
  group_rows("Lease Characteristics", 5, 7) %>%
  group_rows("Lease Outcomes", 8, 10) %>%  
  add_header_above(n_header)

# NOTES that we should add in the draft
# Bonus, Revenue and Surplus are all reported in 1000 nominal dollars per acre.
# Output is reported in barrels of oil equivalent per acre.  Wells are reported
# per 1000 acres.  Acres are reported in 1000s

write(summary_stats_table,
      file = file.path(tdir, "summary_stats_by_type.tex"),
      append = FALSE,
      sep = " ")

# COMBINE AND MAKE IT INTO A TABLE, smaller version for presentation
summary_stats_table <-
  inner_join(sumvar_stats, sumvar_tstats, by = "variable") %>%
  select(variable,
         mean_0, sd_0, min_0, max_0,
         mean_1, sd_1, min_1, max_1) %>%
  mutate(temp = case_when(
           variable == "Acres" ~ 1,
           variable == "ShapeQuality" ~ 2,
           variable == "MultiPolygon" ~ 3,
           variable == "ShaleThickness" ~ 4,           
           variable == "Bonus" ~ 5,
           variable == "Term" ~ 6,
           variable == "RoyaltyRate" ~ 7,
           variable == "Output" ~ 8,
           variable == "LeaseRevenue" ~ 9,
           variable == "SellerRevenue" ~ 10,
           TRUE ~ NA_real_)) %>%
  arrange(temp) %>%
  select(-temp) %>%
  kable(digits = 2,
        format = "latex",
        booktabs = TRUE,
        linesep = "",
        col.names = c("Variable",
                      "mean", "sd", "min", "max", 
                      "mean", "sd", "min", "max")) %>%
  group_rows("Land Characteristics", 1, 4) %>%
  group_rows("Lease Characteristics", 5, 7) %>%
  group_rows("Lease Outcomes", 8, 10) %>%  
  add_header_above(n_header)

# NOTES that we should add in the draft
# Bonus, Revenue and Surplus are all reported in 1000 nominal dollars per acre.
# Output is reported in barrels of oil equivalent per acre.  Wells are reported
# per 1000 acres.  Acres are reported in 1000s

write(summary_stats_table,
      file = file.path(tdir, "summary_stats_by_type_prez.tex"),
      append = FALSE,
      sep = " ")



# DATA CONSTRUCTION WATERFALL -----------------------------------------------
# create dictionary of variables for the table, filtering conditions for 
# lease_data, and the order we want to display them in

negotiation_raw <-
  lease_data %>%
  filter(Auction == 0) %>%
  nrow

auction_raw <-
  lease_data %>%
  filter(Auction == 1) %>%
  nrow

waterfall <-
  lease_data %>%
  group_by(Auction, drop_reason) %>%
  tally %>%
  pivot_wider(drop_reason,
              names_from = Auction,
              values_from = n,
              values_fill = 0) %>%
  mutate(across(c(`0`,`1`), as.numeric)) %>%
  bind_rows(tibble(drop_reason = "",
                   `0` = negotiation_raw,
                   `1` = auction_raw)) %>%
  mutate(temp_order = case_when(
           drop_reason == "" ~ 0,
           drop_reason == "wrongtype" ~ 1,
           drop_reason == "wrongplace" ~ 2,
           drop_reason == "size" ~ 3,
           drop_reason == "netgross" ~ 4,
           drop_reason == "undivided" ~ 5,
           drop_reason == "term" ~ 6,
           drop_reason == "statenegotiate" ~ 7,
           drop_reason == "ralauction" ~ 8,
           drop_reason == "nonstandard" ~ 9,
           TRUE ~ 10)) %>%
  arrange(temp_order) %>%
  mutate(temp_name = case_when(
           drop_reason == "" ~ "Initial Sample",
           drop_reason == "wrongtype" ~ "Not RAL or State lease",           
           drop_reason == "wrongplace" ~ "Not on Shale",
           drop_reason == "size" ~ "Less Than 10 or Greater Than 1,000 Acres",
           drop_reason == "netgross" ~ "Gross and Net Acreage Differ",
           drop_reason == "undivided" ~ "Undivided Interest",
           drop_reason == "term" ~ "Term Less Than 1 Year",
           drop_reason == "statenegotiate" ~ "Negotiated State Lease",
           drop_reason == "ralauction" ~ "Auctioned RAL Lease",
           drop_reason == "nonstandard" ~ "Nonstandard Lease Category",
           TRUE ~ "Final Sample")) %>%
  mutate(cum0 = if_else(drop_reason %in% c("", "notdropped"), 0, `0`),
         cum1 = if_else(drop_reason %in% c("", "notdropped"), 0, `1`)) %>%
  mutate(cum0 = -1 * cum0, cum1 = -1 * cum1) %>%
  mutate(cum0 = if_else(drop_reason == "", `0`, cum0),
         cum1 = if_else(drop_reason == "", `1`, cum1)) %>%
  mutate(cum0 = cumsum(cum0), cum1 = cumsum(cum1)) %>%
  mutate(drop_reason = if_else(drop_reason %in% c("", "notdropped"),
                               temp_name,
                               "")) %>%
  rename(Negotiation = cum0, Auction = cum1) %>%
  select(-c(`0`,`1`, temp_order)) %>%
  mutate(temp_name = if_else(temp_name %in% c("Initial Sample",
                                              "Final Sample"),
                             "",
                             temp_name)) %>%
  rename(`Drop Reason` = drop_reason) %>%
  kable(digits = 2,
        format = "latex",
        booktabs = TRUE,
        linesep = "",
        col.names = c("", "Drop Reason", "Negotiation", "Auction"), 
        format.args = list(big.mark = ','))

write(waterfall,
      file = file.path(tdir, "summary_data_construction.tex"),
      append = FALSE,
      sep = " ")
 
# ADDITIONAL STATS FOR PAPER =================================================
final_leases %>%
    filter(InSample, !Censored) %>% 
    select(Drilled, DBOEPerAcre) %>% summary()

td <- final_leases %>%
    filter(InSample, !Censored) %>% 
    filter(Drilled == 1) 

pctiles <- quantile(td$DBOEPerAcre, probs = seq(0, 1, by= 0.1)) # decile

pctiles

diff9010 = log(pctiles[10]) - log(pctiles[2])

diff9010
  
