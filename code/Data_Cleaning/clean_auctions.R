# reading in and cleaning bid notice and tabulation data
# initial version by Yixin Sun 5/1/2017
# Modified by Eric Karsten starting 08/07/2018
# modified again by Thom Covert in November 2020

# load packages we need 
library(tidyverse)
library(lubridate)
library(readxl)
library(here)

root = here()
source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))

# =============================================================================
# define a function to read and do a first step at cleaning bid data, for the
# set of bid tabulations we didn't (enirely) do by hand
# =============================================================================
read_glo <- function(x) {
  glo <-
    read_excel(x,
               col_names = c("page",
                             "mgl",
                             "Bidder",
                             "tract",
                             "Bid",
                             "comments",
                             "County",
                             "Acres")) %>%
    replace_na(list(page = "0")) %>%
    filter(!str_detect(page, regex("[a-zA-Z]", ignore_case = TRUE))) %>%
    mutate(Date = str_match(x, regex("([\\d\\.]+)\\.xlsx"))[, 2]) %>%
    mutate(Date = mdy(Date)) %>%
    fill(mgl, tract, County, Acres) %>%
    group_by(mgl) %>%
    mutate(survey = (row_number() == 1)) %>% 
    ungroup()

  glo_bidders <-
    glo %>%
    filter(!survey) %>%
    select(-survey) 
  
  glo_surveys <-
    glo %>%
    filter(survey) %>%
    select(mgl, Bidder) %>%
    rename(survey = Bidder) %>%
    unique()
  
  glo <-
    left_join(glo_bidders, glo_surveys) %>%
    mutate(Lease_Number = str_match(comments, regex("[\\d]+")), 
           Lease_Number = ifelse(Lease_Number == 0, NA, Lease_Number)) %>%
    group_by(mgl) %>%
    mutate(Lease_Number = paste0("MF", unique(na.omit(Lease_Number)))) %>%
    ungroup() %>%
    select(Date, page, mgl, County, survey, tract,
           Acres, Bidder, Bid, Lease_Number)
 
  return(glo)
}

# =============================================================================
# read in "main" glo bids (2005-first half of 2017)
# =============================================================================
# note: there are 17 "bids" on 15 auctions that are not ultimately awarded a
# lease, which we can see by the absence of a real lease number.  we'll drop
# these bids, since they all seem to be below the stated reserve price.  similar
# steps happen for the other bid data entry
main_bids <-
  list.files(raw_bids, pattern = "^Final_GLO_", full.names = TRUE) %>%
  map_df(read_glo) %>%
  select(Date, Acres, Bidder, Bonus = Bid, mgl,
         Lease = Lease_Number, County) %>%
  mutate(Acres = str_remove_all(Acres, "\\,"),
         Tract = str_replace_all(mgl, "\\D+", "")) %>%
  mutate(across(c(Acres, Tract, Bonus), as.numeric)) %>%
  select(Date, County, Tract, Acres, Bidder, Bonus, Lease_Number = Lease) %>%
  filter(str_length(Lease_Number) == 8)

# =============================================================================
# read in pre-2005 bids we manually entered into excel in the spring of 2020
# =============================================================================
older_bids <-
  file.path(raw_bids, "old_bids.xlsx") %>%
  read_excel %>%
  mutate(Date = mdy(notice_date),
         Lease_Number = str_replace(lease_no, "M-", "MF")) %>%
  select(Date, County = county, Tract = mgl_no, Acres = acres, Bidder = bidder,
         Bonus = bid, Lease_Number) # %>%

older_bids_with_lease_number <-
  older_bids %>%
  filter(str_length(Lease_Number) == 8, Lease_Number != "MF000000") %>%
  select(Date, Tract, Lease_Number) %>%
  distinct

older_bids <-
  older_bids %>%
  select(-Lease_Number) %>%
  left_join(older_bids_with_lease_number, by = c("Date", "Tract")) %>%
  filter(!is.na(Lease_Number))

# =============================================================================
# read in 2017-2018 bids we manually entered into excel in the summer of 2020
# =============================================================================
newer_bids <-
  file.path(raw_bids, "newest_bids.xlsx") %>%
  read_excel %>%
  mutate(Date = as_date(date),
         Lease_Number = str_replace(lease_no, "MF-", "MF"),
         bidder = str_to_upper(bidder),
         county = str_to_upper(county)) %>%
  select(Date, County = county, Tract = mgl_no, Acres = acres, Bidder = bidder,
         Bonus = bid, Lease_Number)

newer_bids_with_lease_number <-
  newer_bids %>%
  filter(!is.na(Lease_Number)) %>%
  select(Date, Tract, Lease_Number) %>%
  distinct

newer_bids <-
  newer_bids %>%
  select(-Lease_Number) %>%
  left_join(newer_bids_with_lease_number, by = c("Date", "Tract")) %>%
  filter(!is.na(Lease_Number))

# ==============================================================================
# combine them and do a few last manual fixes to remove duplicate bids, 
# including cleaning the bidder names
# ==============================================================================
# merge in alias names for lessee and asignee
company_names <-
  file.path(int, "manually_improved_names.xlsx") %>%
  read_excel %>%
  select(Name, alias = Alias) %>%
  mutate(Name = str_to_upper(Name)) %>%
  select(Name, alias) %>%
  unique

bids <-
  main_bids %>%
  bind_rows(older_bids) %>%
  bind_rows(newer_bids) %>%
  mutate(County = str_to_upper(County)) %>%
  arrange(Date, Tract, Bidder, desc(Bonus)) %>%
  group_by(Date, Tract, Bidder) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  mutate(duplicate_entry = case_when(
           Date == make_date(2015, 8, 4) &
           Tract == 48 &
           str_detect(Bidder, "Rudolfo") ~ TRUE,
           Date == make_date(2016, 7, 19) &
           Tract == 7 &
           str_detect(Bidder, "PEC2") ~ TRUE,
           Date == make_date(2018, 4, 3) &
           Tract == 10 &
           Bidder == "SCALA ENERGY ASSETS, LLC" ~ TRUE,
           TRUE ~ FALSE)) %>%
  filter(!duplicate_entry) %>%
  select(-duplicate_entry) %>%
  mutate(Bidder2 = str_to_upper(Bidder)) %>%
  left_join(company_names, by = c("Bidder2" = "Name")) %>%
  mutate(alias = if_else(is.na(alias), Bidder2, alias)) %>%
  rename(Firm = alias) %>%
  mutate(Firm = str_replace_all(Firm, "&", "AND")) %>%
  select(-Bidder2) %>%
  group_by(Lease_Number) %>%
  mutate(Winner = Bonus == max(Bonus)) %>%
  ungroup

# =============================================================================
# read in and clean bid notice information, starting with the analogous "main"
# sample
# =============================================================================
main_notices <-
  file.path(int, "glo_notices_final.csv") %>%
  read_csv %>%
  rename(Tract = MGL) %>%
  filter(Agency %in% c("PERMANENT SCHOOL FUND", "SURVEYED SCHOOL LAND")) %>%
  select(Date, Tract, Acres, County, MinBid, merge_id)

# read in old notices
old_notices <-
  file.path(raw_notices, "old_notices.xlsx") %>%
  read_excel %>%
  mutate(Date = mdy(notice_date), Tract = mgl_no, Acres = acres,
         County = county, MinBid = min_bid) %>%
  select(Date, Tract, Acres, County, MinBid)

# merge in merge_ids for the old notices (only done for auctions in 2004)
old_notice_merge_ids <-
  file.path(int, "glo_notices_final.csv") %>%
  read_csv %>%
  select(Date, Tract = MGL, merge_id) %>%
  filter(!is.na(merge_id))

old_notices <-
  old_notices %>%
  left_join(old_notice_merge_ids)

# read in new notices
new_notices <-
  file.path(raw_notices, "newest_notices.xlsx") %>%
  read_excel %>%
  mutate(Date = as_date(date), Tract = mgl_no, Acres = acres,
         MinBid = `minimum bid`,
         County = str_to_upper(county)) %>%
  select(Date, Tract, Acres, County, MinBid)

notices <-
  main_notices %>%
  bind_rows(old_notices) %>%
  bind_rows(new_notices) %>%
  mutate(County = str_to_upper(County)) %>%
  mutate(Acres = if_else(Date == make_date(2015, 8, 4) & Tract == 37,
                         666.0, Acres))
                         

# =============================================================================
# compare bid and notice data for consistency in county and acres
# note, at this point, there are still a decent number of date/tract combos
# that are in the bid data but not that nominations data.  the overwhelming
# majority of these are because in many bid tabulaions, we recorded everything,
# including bids in auctions on non-PSF land, whereas we tried to *only* read
# in bid notice data for PSF land, because these files were harder to do data
# entry on
# =============================================================================
compare_notices <-
  notices %>%
  select(Date, Tract, NoticeAcres = Acres, NoticeCounty = County)

compare_bids <-
  bids %>%
  select(Date, Tract, BidAcres = Acres, BidCounty = County) %>%
  distinct

temp <-
  inner_join(compare_notices, compare_bids)

# only 4 of these and two of them are "slash" issues
diff_counties <-
  temp %>%
  filter(NoticeCounty != BidCounty)

# 13 of these, but only one is meaningfully different, and we'll ultimately just
# use the lease data for lease size when we get to an analysis of bonus per acre
# in the auction data
diff_acres <-
  temp %>%
  filter(round(NoticeAcres) != round(BidAcres))

# =============================================================================
# fill in missing merge_ids
# =============================================================================
max_merge_id <-
  notices %>%
  filter(!is.na(merge_id)) %>%
  select(merge_id) %>%
  distinct %>%
  arrange(desc(merge_id)) %>%
  filter(row_number() == 1) %>%
  pluck(1)

missing_merge_ids <-
  notices %>%
  filter(is.na(merge_id)) %>%
  mutate(UnmatchedMergeID = TRUE,
         merge_id = max_merge_id + row_number()) %>%
  select(Date, Tract, UnmatchedMergeID, merge_id)

has_merge_id <-
  notices %>%
  filter(!is.na(merge_id)) %>%
  mutate(UnmatchedMergeID = FALSE)

unmatched_merge_id <-
  notices %>%
  filter(is.na(merge_id)) %>%
  select(-merge_id) %>%
  left_join(missing_merge_ids)

notices <-
  bind_rows(has_merge_id, unmatched_merge_id)

# =============================================================================
# save to disk: bids, notices
# =============================================================================
clean_bids <- bids
clean_notices <- notices

save(clean_bids, file = file.path(gen, "clean_bids.Rda"))
save(clean_notices, file = file.path(gen, "clean_notices.Rda"))


