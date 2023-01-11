# Created by Thom Covert in late May 2017, edited in November 2018
# Cleaning EIA WTI and HH price data

library(tidyverse)
library(lubridate)
library(readxl)

#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
library(here)
root <- here()
source(file.path(root, "code/paths.R"))


#===========================================================================
# READ IN EIA DATA
#===========================================================================

# henry hub spot prices denominated in $/mmbtu
hh <-
    file.path(ddir, "raw_data/prices/RNGWHHDm.xls") %>%
    read_excel(sheet = "Data 1",
               skip = 3,
               col_names = c("Date", "price")) %>%
    mutate(series = "gas") %>%
    mutate(Date = as.Date(ISOdate(year(Date), month(Date), 1)))

# wti spot prices denominated in $/bbl
wti <-
    file.path(ddir, "raw_data/prices/RWTCm.xls") %>%
    read_excel(sheet = "Data 1",
               skip = 3,
               col_names = c("Date", "price")) %>%
    mutate(series = "oil") %>%
    mutate(Date = as.Date(ISOdate(year(Date), month(Date), 1)))


#===========================================================================
# COMBINE, RESHAPE AND SAVE
#===========================================================================
prices <-
    bind_rows(hh, wti) %>%
    spread(series, price) %>%
    mutate(Date = as.Date(Date)) %>%
    as_tibble()

save(prices, file = file.path(gen, "prices.Rda"))

    
