#===============================================================================
# code to make quarterly plots of parcel lease status
#===============================================================================
library(tidyverse)
library(lubridate)
library(fixest)
library(splines)
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

#===============================================================================
# LOAD DATA CLEANED IN final_parcel_outcomes.R
#===============================================================================
load(file.path(gen, "outcomes_earlyLeases.Rda"))

#===============================================================================
# for each day d, check whether the parcel has an active lease
#===============================================================================

active_date_analysis <- function(d, it_data) {
  m <-
    feols(active ~ Auction + bs(Acres, df = 7) | Grid10,
          filter(it_data, Date == d))

  est <-
    coeftable(m)["Auction",] %>%
    enframe %>%
    pivot_wider %>%
    mutate(lower = Estimate - 1.96 * `Std. Error`,
           upper = Estimate + 1.96 * `Std. Error`) %>%
    select(Estimate, `Lower CI` = lower, `Upper CI` = upper) %>%
    mutate(Date = d)

  return(est)
}

to_qdate <- function(d) {
  return(case_when(month(d) <= 3 ~ make_date(year(d), 3, 31),
                   month(d) <= 6 ~ make_date(year(d), 6, 30),
                   month(d) <= 9 ~ make_date(year(d), 9, 30),
                   TRUE ~ make_date(year(d), 12, 31)))
}

reg_data <-
  outcomes_earlyLeases$parcel_month_outcomes %>%
  left_join(select(outcomes_earlyLeases$parcel_outcomes, 
                   ControlNum, ParcelType, ParcelAcres, ActiveRevStartDate,
                   OnShale, Grid10, Grid20)) %>%
  mutate(Auction = if_else(ParcelType == "STATE",1,0),
         Acres = ParcelAcres,
         active = NLeases > 0) %>%
  filter(OnShale, ParcelType != "FREE", ParcelType != "OTHER") %>%
  mutate(Date = to_qdate(Date)) %>%
  group_by(ControlNum, Acres, Auction, Grid10, ActiveRevStartDate, Date) %>%
  summarize(active = max(active)) %>%
  ungroup

fd <- to_qdate(SampleLeaseStart)
ld <- to_qdate(SampleLeaseEnd)
plot_dates <-
  reg_data %>%
  select(Date) %>%
  distinct %>%
  arrange(Date) %>%
  filter(Date >= fd, Date <= ld) %>%
  pluck(1)

active_plot <-
  plot_dates %>%
  map_dfr(~ active_date_analysis(.x, it_data = reg_data)) %>%
  ggplot(aes(x = Date, y = Estimate)) +
  geom_pointrange(aes(ymin = `Lower CI`, ymax = `Upper CI`)) +
  geom_hline(yintercept = 0) +
  scale_x_date(date_breaks = "1 year", labels = year) +
  theme_classic() +
  xlab("Date") +
  ylab(expression(tau[t])) +   
  theme(legend.position = c(0.7, 0.2),
        legend.title = element_blank(),
        text = element_text(size=20),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"))

ggsave(file.path(fdir, "active_plot.png"),
       active_plot, width = 7.5, height = 5,
       bg = "transparent")

ggsave(file.path(fdir, "active_plot.pdf"),
       active_plot, width = 7.5, height = 5,
       bg = "transparent")

## drop parcels active as of jan 2004
reg_data_nojan2004 <-
  reg_data %>%
  filter(ActiveRevStartDate == 0)

active_plot_nojan2004 <-
  plot_dates %>%
  map_dfr(~ active_date_analysis(.x, it_data = reg_data_nojan2004)) %>%
  ggplot(aes(x = Date, y = Estimate)) +
  geom_pointrange(aes(ymin = `Lower CI`, ymax = `Upper CI`)) +
  geom_hline(yintercept = 0) +
  scale_x_date(date_breaks = "1 year", labels = year) +
  theme_classic() +
  xlab("Date") +
  ylab(expression(tau[t])) +   
  theme(legend.position = c(0.7, 0.2),
        legend.title = element_blank(),
        text = element_text(size=20),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"))


