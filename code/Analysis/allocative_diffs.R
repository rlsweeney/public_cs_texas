# determine with auctions and RAL leases end up in the hands of different firms

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

# constants
source(file.path(root, "code/texas_constants.R"))

#===========================================================================
# SAMPLE DEFINED IN sample_selection.R
#===========================================================================
load(file.path(ddir, "generated_data/final_leases.Rda"))

lease_data <-
  final_leases %>%
  filter(InSample) %>%
  mutate(Permian = if_else(ShalePlay %in% c("DELAWARE", "SPRABERRY"), 1, 0),
         AuctionPermian = Auction * Permian)

#===========================================================================
# Top 10 lessees, everyone else, their share of auctions
# plus same stats but computed in the set of permian counties
#===========================================================================
test_all <-
  lease_data %>%
  group_by(NewFirm) %>%
  summarize(n = n(), Auction = sum(Auction)) %>%
  mutate(RAL = n - Auction) %>%
  arrange(desc(n)) %>%
  mutate(FirmRank = row_number()) %>%
  mutate(Firm = if_else(FirmRank <= 40, NewFirm, "ALL OTHERS"),
         FirmRank = if_else(FirmRank <= 40, FirmRank, as.integer(11))) %>%
  group_by(FirmRank, Firm) %>%
  summarize(n = sum(n), Auction = sum(Auction), RAL = sum(RAL)) %>%
  ungroup %>%
  with(cbind(Auction, RAL)) %>% prop.test(correct = TRUE)

test_permian <-
  lease_data %>%
  filter(ShalePlay %in% c("DELAWARE", "SPRABERRY")) %>%
  group_by(NewFirm) %>%
  summarize(n = n(), Auction = sum(Auction)) %>%
  mutate(RAL = n - Auction) %>%
  arrange(desc(n)) %>%
  mutate(FirmRank = row_number()) %>%
  mutate(Firm = if_else(FirmRank <= 10, NewFirm, "ALL OTHERS"),
         FirmRank = if_else(FirmRank <= 10, FirmRank, as.integer(11))) %>%
  group_by(FirmRank, Firm) %>%
  summarize(n = sum(n), Auction = sum(Auction), RAL = sum(RAL)) %>%
  ungroup %>%
  with(cbind(Auction, RAL)) %>% prop.test(correct = TRUE)

test_eagleford <-
  lease_data %>%
  filter(ShalePlay == "EAGLE FORD") %>%
  group_by(NewFirm) %>%
  summarize(n = n(), Auction = sum(Auction)) %>%
  mutate(RAL = n - Auction) %>%
  arrange(desc(n)) %>%
  mutate(FirmRank = row_number()) %>%
  mutate(Firm = if_else(FirmRank <= 10, NewFirm, "ALL OTHERS"),
         FirmRank = if_else(FirmRank <= 10, FirmRank, as.integer(11))) %>%
  group_by(FirmRank, Firm) %>%
  summarize(n = sum(n), Auction = sum(Auction), RAL = sum(RAL)) %>%
  ungroup %>%
  with(cbind(Auction, RAL)) %>% prop.test(correct = TRUE)

tbl <-
  lease_data %>%
  group_by(NewFirm) %>%
  summarize(n = n(), Auction = sum(Auction)) %>%
  mutate(RAL = n - Auction) %>%
  arrange(desc(n)) %>%
  mutate(FirmRank = row_number()) %>%
  mutate(Firm = if_else(FirmRank <= 10, NewFirm, "ALL OTHERS"),
         FirmRank = if_else(FirmRank <= 10, FirmRank, as.integer(11))) %>%
  group_by(FirmRank, Firm) %>%
  summarize(n = sum(n), Auction = sum(Auction), RAL = sum(RAL)) %>%
  ungroup %>%
  mutate(AuctionShare = Auction / sum(Auction),
         RALShare = RAL / sum(RAL),
         TotalShare = (Auction + RAL) / (sum(Auction) + sum(RAL))) %>%
  select(Firm, n, AuctionShare, RALShare, TotalShare) %>%
  kable(format = "latex",
        digits = c(0, 0, 3, 3, 3),
        booktabs = TRUE,
        linesep = "",
        align = c("l", "c", "c", "c", "c"),
        col.names = c("Firm",
                      "Leases",
                      "Auction Share",
                      "Negotiation Share",
                      "Overall Share")) %>%
  row_spec(10, extra_latex_after = "\\midrule")  %>%
  column_spec(2:5, "5em")

write(tbl,
      file = file.path(tdir, "allocative.tex"),
      append = FALSE,
      sep = " ")

tbl_top_share_auction <-
  lease_data %>%
  filter(Auction == 1) %>%
  group_by(NewFirm) %>%
  summarize(n=n()) %>%
  arrange(desc(n)) %>%
  mutate(pct = n / sum(n)) %>%
  filter(row_number() <= 40) %>%
  select(pct) %>%
  with(sum(pct))

tbl_top_share_negotiation <-
  lease_data %>%
  filter(Auction == 0) %>%
  group_by(NewFirm) %>%
  summarize(n=n()) %>%
  arrange(desc(n)) %>%
  mutate(pct = n / sum(n)) %>%
  filter(row_number() <= 40) %>%
  select(pct) %>%
  with(sum(pct))

