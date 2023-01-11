# BASIC TEXAS SETUP ============================================================
library(tidyverse)
library(readxl)
library(lubridate)
library(knitr)
library(kableExtra)
library(splines)
library(fixest)
library(lmtest)
library(here)

root <- here()

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))
source(file.path(root, "code", "functions", "utils.R"))
source(file.path(root, "code", "functions", "regtable.R"))
source(file.path(root, "code", "functions", "latex_number.R"))
source(file.path(root, "code", "functions", "load_crossfit.R"))

# Use GPV techniques to estimate the distribution of values using the bid data
# and compute the allocative efficiency gains from selecting the highest bidder
#===============================================================================
# READ IN AND COMBINE BID AND BID NOTICE DATA, restrict to Acres in [10,1000]
# counties that are in sample, and 2004-2016
#===============================================================================
load(file.path(gen, "final_leases.Rda"))

in_sample_leases <-
  final_leases %>%
  filter(InSample) %>%
  select(Lease_Number)

in_sample_counties <-
  final_leases %>%
  filter(OnShale) %>%
  select(County, ShalePlay) %>%
  group_by(County, ShalePlay) %>%
  summarize(CSPCount = n()) %>%
  ungroup

# both notices and bids sometimes list an auction as being in 2 counties
# here we pick one by first chosing in shale counties over non-in-shale counties
# and then within in shale counties choose the more common one
load(file.path(gen, "clean_notices.Rda"))
load(file.path(gen, "clean_bids.Rda"))

shale_notices <-
  clean_notices %>%
  select(Date, Tract, County) %>%
  separate_rows(County, sep = "/") %>%
  inner_join(in_sample_counties, by = "County") %>%
  arrange(Date, Tract, desc(CSPCount)) %>%
  group_by(Date, Tract) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  select(Date, Tract, NewCounty = County, ShalePlay) %>%
  distinct

final_notices <-
  clean_notices %>%
  inner_join(shale_notices, by = c("Date", "Tract")) %>%
  filter(year(Date) >= 2004, year(Date) <= 2016) %>%
  filter(Acres <= 1000, Acres >= 10) %>%
  arrange(Date, Tract, ShalePlay) %>%
  mutate(AuctionID = row_number(), MinBidPerAcre = MinBid / Acres) %>%
  select(-County) %>%
  rename(County = NewCounty)

final_bids <-
  clean_bids %>%
  select(-County, -Acres) %>%
  inner_join(select(final_notices, Date, Tract, ShalePlay, County, Acres,
                    AuctionID, MinBidPerAcre),
             by = c("Date", "Tract")) %>%
  mutate(BidPerAcre = Bonus / Acres) %>%
  filter(BidPerAcre >= MinBidPerAcre)

# add back number of realized bidders to the notices data
realized_nbidders <-
  final_bids %>%
  group_by(AuctionID) %>%
  summarize(NBids = n())

final_notices <-
  final_notices %>%
  left_join(realized_nbidders, by = "AuctionID") %>%
  replace_na(list(NBids = 0))

final_bids <-
  final_bids %>%
  group_by(AuctionID) %>%
  mutate(NBids = n()) %>%
  ungroup

#===============================================================================
# tabulate incidence of failed auctions and fraction parcel ids that ever lease
#===============================================================================
tract_date_leased <-
  final_notices %>%
  filter(NBids > 0) %>%
  nrow

tract_date_leased_insample <-
  final_bids %>%
  filter(Winner) %>%
  semi_join(in_sample_leases, by = "Lease_Number") %>%
  nrow

tract_date_leased_share <-
  final_notices %>%
  with(100 * mean(NBids == 0))

tract_ever_leased_share <-
  final_notices %>%
  group_by(merge_id) %>%
  summarize(EverLeased = max(NBids > 0)) %>%
  with(100 * mean(EverLeased))


latex_number(tract_date_leased,
             "tract_date_leased",
             format = "f",
             big.mark = ",",
             digits = 0)

latex_number(tract_date_leased_insample,
             "tract_date_leased_insample",
             format = "f",
             big.mark = ",",
             digits = 0)

latex_number(tract_date_leased_share,
             "tract_date_leased_share",
             format = "f",
             big.mark = ",",
             digits = 0)

latex_number(tract_ever_leased_share,
             "tract_ever_leased_share",
             format = "f",
             big.mark = ",",
             digits = 0)


#===============================================================================
# make the Top Auction Pairs table
#===============================================================================
# broker_regex and nonbroker_regex both defined in utils.R
top_pair_bids <-
  final_bids %>%
  arrange(AuctionID, desc(BidPerAcre), Bidder) %>%
  group_by(AuctionID) %>%
  mutate(bidrank = rank(-BidPerAcre, ties.method = "first")) %>%
  ungroup %>%
  mutate(broker = str_detect(Firm, broker_regex) &
           !str_detect(Firm, nonbroker_regex)) %>%
  filter(!broker) %>%
  group_by(Firm) %>%
  filter(n() >= 10) %>%
  select(Firm, Lease_Number, BidPerAcre)

eqprop_pval <- function(x, n) {map2_dbl(x, n, ~ binom.test(.x, .y)$p.value)}

top_table <-
  top_pair_bids %>%
  rename(Firm2 = Firm, BidPerAcre2 = BidPerAcre) %>%
  left_join(top_pair_bids, by = "Lease_Number") %>%
  filter(Firm2 < Firm) %>%
  mutate(win = BidPerAcre2 > BidPerAcre) %>%
  group_by(Firm2, Firm) %>%
  summarize(n = n(), wins = sum(win)) %>%
  ungroup %>%
  arrange(desc(n)) %>%
  filter(n >= 10) %>%
  mutate(p = round(eqprop_pval(wins, n), 3)) %>%
  mutate(wins = round(wins / n, 2)) %>%
  kable(format = "latex",
        booktabs = TRUE,
        linesep = "",
        escape = FALSE,
        col.names = c("Firm A", "Firm B",
                      "Auctions", "Share A $>$ B",
                      "p-value"))

write(top_table,
      file = file.path(tdir, "TopPairAuctionShares.tex"),
      append = FALSE,
      sep = " ")

#===============================================================================
# tabulate what share of negotiations are won by firms who ever make bids in
# OnShale auctions during our sample period, and what share of bidders ever
# win a negotiation
#===============================================================================
negotiation_winners <-
  final_leases %>%
  filter(InSample, Auction == 0) %>%
  group_by(Firm = NewFirm) %>%
  summarize(N_negotiations = n())

ever_bidders <-
  final_bids %>%
  group_by(Firm) %>%
  summarize(N_auction_bids = n())

negotiations_won_by_auction_bidders <-
  negotiation_winners %>%
  left_join(ever_bidders) %>%
  replace_na(list(N_auction_bids = 0)) %>%
  mutate(wonby = N_negotiations * (N_auction_bids > 0))

ever_bidder_negotiation_share <-
  negotiations_won_by_auction_bidders %>%
  with(sum(wonby)/sum(N_negotiations))

latex_number(ever_bidder_negotiation_share * 100,
             "ever_bidder_negotiation_share",
             format = "f",
             big.mark = ",",
             digits = 0)

auctions_won_by_negotiation_winners <-
  final_leases %>%
  filter(InSample, Auction == 1) %>%
  rename(Firm = NewFirm) %>%
  group_by(Firm) %>%
  summarize(N_auctions = n()) %>%
  ungroup %>%
  left_join(negotiation_winners) %>%
  replace_na(list(N_negotiations = 0)) %>%
  mutate(wonby = N_auctions * (N_negotiations > 0))

negotiation_winner_auction_share <-
  auctions_won_by_negotiation_winners %>%
  with(sum(wonby) / sum(N_auctions))
             
latex_number(negotiation_winner_auction_share * 100,
             "negotiation_winner_auction_share",
             format = "f",
             big.mark = ",",
             digits = 0)

#===============================================================================
# do bonus regressions (just g10y) using as left hand side variable as
# (a) actual bonus payments
# (b) reserve price
# (c) lowest bid for auctions
# (d) second highest bid for auctions
# in b + c, for 1 bidder auctions use the reserve price
#===============================================================================
max2 <- function(v) {
  return(sort(v, decreasing = TRUE)[2])
}

auction_bonuses <-
  final_bids %>%
  group_by(Lease_Number) %>%
  mutate(BestBidPerAcre = max(BidPerAcre),
         SecondBestBidPerAcre = if_else(NBids > 1,
                                          max2(BidPerAcre),
                                          MinBidPerAcre),
         WorstBidPerAcre = if_else(NBids > 1,
                                     min(BidPerAcre),
                                     MinBidPerAcre)) %>%
  ungroup %>%
  select(Lease_Number, BestBidPerAcre, MinBidPerAcre, NBids,
         SecondBestBidPerAcre, WorstBidPerAcre) %>%
  distinct %>%
  mutate(across(contains("BidPerAcre"), ~ .x / 1000))

reg_data <-
  final_leases %>%
  filter(InSample) %>%
  left_join(auction_bonuses, by = "Lease_Number") %>%
  mutate(BestBidPerAcre = if_else(is.na(BestBidPerAcre),
                                  BonusPerAcre,
                                  BestBidPerAcre),
         SecondBestBidPerAcre = if_else(is.na(SecondBestBidPerAcre),
                                        BonusPerAcre,
                                        SecondBestBidPerAcre),
         WorstBidPerAcre = if_else(is.na(WorstBidPerAcre),
                                   BonusPerAcre,
                                   WorstBidPerAcre),
         MinBidPerAcre = if_else(is.na(MinBidPerAcre),
                                 BonusPerAcre,
                                 MinBidPerAcre)) %>%
  mutate(BestBid = Acres * BestBidPerAcre, 
          SecondBestBid = Acres * SecondBestBidPerAcre, 
          WorstBid = WorstBidPerAcre * Acres, 
          MinBid = MinBidPerAcre * Acres,
          Bonus = BonusPerAcre * Acres)

fe_models <-
  feols(c(log(BestBid), log(MinBid),
          log(WorstBid), log(SecondBestBid)) ~
          Auction + bs(Acres, df = 4) | Grid10Yr + YearQtr,
        cluster = "Grid10",
        reg_data)

m_RF_base <-
  paste("log(BestBid)", "Auction", sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate + Acres", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, "linear", n = dml_n, ml = "rf", workers = 8)

m_RF_minbid <-
  paste("log(MinBid)", "Auction", sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate + Acres", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, "linear", n = dml_n, ml = "rf", workers = 8)

m_RF_worstbid <-
  paste("log(WorstBid)", "Auction", sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate + Acres", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, "linear", n = dml_n, ml = "rf", workers = 8)

m_RF_secondbid <-
 paste("log(SecondBestBid)", "Auction", sep = " ~ ") %>%
  paste("CentLat + CentLong + EffDate + Acres", sep = " | ") %>%
  as.formula %>%
  dml(reg_data, "linear", n = dml_n, ml = "rf", workers = 8)


regression_tbl <-
  list(fe_models[[1]], 
       m_RF_base,
       fe_models[[2]], 
       m_RF_minbid,
       fe_models[[3]], 
       m_RF_worstbid,
       fe_models[[4]], 
       m_RF_secondbid) %>%
  regtable(., est = "Auction",
           extra_rows = list("Auction Bonus" =
                               c("Winner", "Winner",
                                 "Reserve", "Reserve",
                                 "Worst", "Worst",
                                 "Second", "Second"),
                             "Grid" = rep(c("10", "DML"), 4),
                             "Time" = rep(c("GY,Q", "DML"), 4)),
           n_obs = TRUE,
           stats = NA,
           stats_names = NA,
           se_fun = list(vcov, vcov, vcov, vcov, vcov, vcov, vcov, vcov),
           decimals = 2,
           output_format = "latex")
writeLines(regression_tbl, file.path(tdir, "auction_bonus_regressions.tex"))



#===============================================================================
# do the GPV work
#===============================================================================
# first, estimate r = xb + u in the notice data, and separately recover xb and u
heterogeneity_model <-
  final_notices %>%
  lm(log(MinBidPerAcre) ~ factor(County) + factor(Date) + bs(Acres, df = 4),
     data = .)

notices_u <- residuals(heterogeneity_model)
notices_het_index <- predict(heterogeneity_model)

final_notices <-
  final_notices %>%
  bind_cols(u = notices_u) %>%
  bind_cols(HetIndex = notices_het_index)

# merge u and HetIndex back into bids and verify that they predict bids nicely
final_bids <-
  final_bids %>%
  left_join(select(final_notices, Date, Tract, HetIndex, u))

het_index_model <-
  final_bids %>%
  lm(log(BidPerAcre) ~ HetIndex + u, data = .)

het_index_model2 <-
  final_bids %>%
  lm(log(BidPerAcre) ~ log(MinBidPerAcre) +
       factor(Date) + factor(County), data = .)

het_index_model3 <-
  final_bids %>%
  lm(log(BidPerAcre) ~ log(MinBidPerAcre), data = .)

# of note:
# 1) bid data says that the relationship between bids and date/county isn't
# quite the same as the relationship between reserves and date/county, though it
# is fairly close
# 2) because slopes on u and hetindex in the bid model are nearly identical and
# very close to unity, this procedure is probably extremely similar to just
# directly conditioning on the reserve price itself

trikern <- function(u) {
  k <- if_else(abs(u) <= 1, (35 / 32) * (1 - u^2)^3, 0)
  return(k)
}

# compute point estimates of the values, conditional on bids, observed
# heterogeneity, and unobserved heterogeneity
gpv <- function(auction_data, bid_data,
                NBids, HetIndex, UHet,
                BidPerAcre, MinBidPerAcre,
                bw_scale = 1, kernfun = trikern) {

  # do tidy programming basics
  NBids <- enquo(NBids)
  HetIndex <- enquo(HetIndex)
  UHet <- enquo(UHet)
  BidPerAcre <- enquo(BidPerAcre)
  MinBidPerAcre <- enquo(MinBidPerAcre)
  
  # estimate pr no bid model and predict for subsample of auctions with bids
  L0 <- nrow(auction_data)
  L <-
    auction_data %>%
    filter(!!NBids > 0) %>%
    nrow
  
  N <-
    auction_data %>%
    select(!!NBids) %>%
    distinct %>%
    pluck(1) %>%
    max

  # compute bandwidths: computing two versions for each of the HetIndex and
  # UHet bandwidths.  one for the sample of all auctions, and another for the
  # sample of all successful auctions.  the first set goes into the pr(nobid)
  # calculation, the second goes into the value/density calculations
  h_hi0 <-
    auction_data %>%
    select(!!HetIndex) %>%
    pluck(1) %>%
    sd
  h_hi0 <- bw_scale * h_hi0 * 1.06 * L0^(-1/5)

  h_u0 <-
    auction_data %>%
    select(!!UHet) %>%
    pluck(1) %>%
    sd
  h_u0 <- bw_scale * h_u0 * 1.06 * L0^(-1/5)

  h_hi <-
    auction_data %>%
    filter(!!NBids > 0) %>%
    select(!!HetIndex) %>%
    pluck(1) %>%
    sd
  h_hi <- bw_scale * h_hi * 1.06 * L^(-1/5)

  h_u <-
    auction_data %>%
    filter(!!NBids > 0) %>%    
    select(!!UHet) %>%
    pluck(1) %>%
    sd
  h_u <- bw_scale * h_u * 1.06 * L^(-1/5)

  # for each (hi, u) pair, compute
  # (a) density at (hi, u)
  # (b) kernel weighted sum
  data_HetIndex <-
    auction_data %>%
    select(!!HetIndex) %>%
    pluck(1)

  data_UHet <-
    auction_data %>%
    select(!!UHet) %>%
    pluck(1)

  data_NBids <-
    auction_data %>%
    select(!!NBids) %>%
    pluck(1)

  # next compute the probability of bidding, conditional on the heterogeneity
  # index and the unobserved heterogeneity u.  since we are going to follow the
  # gpv setup, and assume that the number of bidders is binomial, we estimate 
  # that probability of bidding as nbids/N ~ hetindex + u
  # this all comes from taking the formula from gpv page 550

  
  # precompute f * (Phi / (1 - Phi)) since thats what we ultimately need
  prnobid <- function(hi, u) {
    k_hi <- sapply(data_HetIndex, function(x) kernfun((hi - x) / h_hi0))
    k_u <- sapply(data_UHet, function(x) kernfun((u - x) / h_u0))
    f <- (1 / L0) * (1 / h_hi0) * (1 / h_u0) * sum(k_hi * k_u)
    v <- 1 - (1 / L0) * (1 / N) * (1 / f) * (1 / h_hi0) * (1 / h_u0) *
      sum(data_NBids * k_hi * k_u)
    return(list(v, (v / (1 - v)) * f))
  }

  # now run this on bids to get prnobid for each bid observation and then
  # compute s
  newbids <-
    bid_data %>%
    mutate(OddsRatio = map2_dbl(!!HetIndex, !!UHet,
                                ~ prnobid(.x, .y)[[2]])) %>%
    mutate(PrNoBid = map2_dbl(!!HetIndex, !!UHet,
                              ~ prnobid(.x, .y)[[1]])) %>%
    mutate(s = sqrt(BidPerAcre - MinBidPerAcre))

  # estimate g on s
  h_s <-
    newbids %>%
    select(s) %>%
    pluck(1) %>%
    sd
  h_s <- bw_scale * h_s * 1.06 * nrow(newbids)^(-1/5)

  data_s <-
    newbids %>%
    select(s) %>%
    pluck(1)

  data_HetIndex <-
    newbids %>%
    select(!!HetIndex) %>%
    pluck(1)

  data_UHet <-
    newbids %>%
    select(!!UHet) %>%
    pluck(1)

  data_NBids <-
    newbids %>%
    select(!!NBids) %>%
    pluck(1)  
  
  k_s <- outer(data_s, data_s, function(x, y) kernfun((x - y) / h_s))
  k_hi <- outer(data_HetIndex, data_HetIndex,
                function(x, y) kernfun((x - y) / h_hi))
  k_u <- outer(data_UHet, data_UHet, function(x, y) kernfun((x - y) / h_u))
  nn <- t(matrix(rep((1 / data_NBids), length(data_NBids)),
                 length(data_NBids), length(data_NBids)))
  g <- apply(k_s * k_hi * k_u * nn, 1, sum) *
    (1 / L) * (1 / h_s) * (1 / h_hi) * (1 / h_u)

  # estimate G on s
  I_s <- outer(data_s, data_s, function(x, y) 1 * (y <= x))
  G <- apply(I_s * k_hi * k_u * nn, 1, sum) * (1 / L) * (1 / h_hi) * (1 / h_u)

  # compute pseudo-values, flag values to potentially be trimmed
  newbids <-
    newbids %>%
    bind_cols(g = g, G = G) %>%
    mutate(v = !!MinBidPerAcre + s^2 +
             2 * (s / (N - 1)) * (1 / g) * (G + OddsRatio)) %>%
    mutate(Trimmed = if_else((s < min(s) + h_s) |
                             (s > max(s) - h_s) |
                             (!!HetIndex < min(!!HetIndex) + h_hi) |
                             (!!HetIndex > max(!!HetIndex) - h_hi) |
                             (!!UHet < min(!!UHet) + h_u) |
                             (!!UHet > max(!!UHet) - h_u),
                             TRUE,
                             FALSE))

  print(paste("het index bandwidth is: ", h_hi))
  print(paste("unobserved het index bandwidth is: ", h_u))

  # finally, compute joint density of values at avg for HetIndex and u, for a
  # large of values.  can use these quasi-densities as weights in a sample
  # routine outside of this program.  note, they are not true univariate
  # densities because we do not divide gv by the density at (avg_hi/avg_u)
  avg_hi <-
    newbids %>%
    select(Date, Tract, HetIndex) %>%
    distinct %>%
    with(mean(HetIndex))

  avg_u <-
    newbids %>%
    select(Date, Tract, u) %>%
    distinct %>%
    with(mean(u))

  print(paste("het index avg is: ", avg_hi))
  print(paste("unobserved het index avg is: ", avg_u))
  
  data_v <-
    newbids %>%
    select(v) %>%
    pluck(1)

  h_v <- bw_scale * sd(data_v) * 1.06 * nrow(newbids)^(-1/5)

  k_v <- outer(data_v, data_v, function(x, y) kernfun((x - y) / h_v))
  k_hi_avg <- sapply(data_HetIndex, function(x) kernfun((avg_hi - x) / h_hi))
  k_u_avg <- sapply(data_UHet, function(x) kernfun((avg_u - x) / h_u))
  gv <- apply(k_v * k_hi_avg * k_u_avg * nn, 1, sum) *
    (1 / L) * (1 / h_v) * (1 / h_hi) * (1 / h_u)

  # and compute PrNoBid at those exact values as well
  prnobid_avg <- prnobid(avg_hi, avg_u)
  
  newbids <-
    newbids %>%
    bind_cols(gv = gv) %>%
    mutate(PrNoBidAvg = prnobid_avg[[1]])

  return(newbids)
}

drop_bidder_sim <- function(d, n, ns, gap = 1) {
  seq(1, ns) %>%
    map_dbl(~ log(max(sample(d$v, n, replace = TRUE, prob = d$gv))) -
              log(max(sample(d$v, n-gap, replace = TRUE, prob = d$gv))))
}

# do GPV calculations
# all auctions, standard bandwidth
tbl_data_baseline <-
  gpv(final_notices, final_bids, NBids, HetIndex, u, BidPerAcre, MinBidPerAcre,
      bw_scale = 1) %>%
  mutate(sample = "all", bw = 1) %>%
  group_by(Date, Tract) %>%
  mutate(HasTrimmed = max(Trimmed) == 1) %>%
  arrange(BidPerAcre) %>%
  mutate(vr = sort(v)) %>%
  mutate(Rearranged = max(v != vr) == 1) %>%
  ungroup %>%
  select(-v) %>%
  rename(v = vr)

# all auctions, double bandwidth
tbl_data_double <-
  gpv(final_notices, final_bids, NBids, HetIndex, u, BidPerAcre, MinBidPerAcre,
      bw_scale = 2) %>%
  mutate(sample = "all", bw = 2) %>%
  group_by(Date, Tract) %>%
  mutate(HasTrimmed = max(Trimmed) == 1) %>%
  arrange(BidPerAcre) %>%
  mutate(vr = sort(v)) %>%
  mutate(Rearranged = max(v != vr) == 1) %>%
  ungroup %>%
  select(-v) %>%
  rename(v = vr)

# all auctions, half bandwidth
tbl_data_half <-
  gpv(final_notices, final_bids, NBids, HetIndex, u, BidPerAcre, MinBidPerAcre,
      bw_scale = 0.5) %>%
  mutate(sample = "all", bw = 0.5) %>%
  group_by(Date, Tract) %>%
  mutate(HasTrimmed = max(Trimmed) == 1) %>%
  arrange(BidPerAcre) %>%
  mutate(vr = sort(v)) %>%
  mutate(Rearranged = max(v != vr) == 1) %>%
  ungroup %>%
  select(-v) %>%
  rename(v = vr)

#===============================================================================
# compute the values in the drop a bidder exercise and latex_number them
#===============================================================================
tbl_data_for_sim <-
  tbl_data_baseline %>%
  filter(HetIndex <= 6.33, HetIndex >= 5.67, u <= 0.12, u >= -0.12)

v_21 <- 
  tbl_data_for_sim %>%
  drop_bidder_sim(2, 100000, gap = 1) %>%
  mean

v_32 <- 
  tbl_data_for_sim %>%
  drop_bidder_sim(3, 100000, gap = 1) %>%
  mean

v_31 <- 
  tbl_data_for_sim %>%
  drop_bidder_sim(3, 100000, gap = 2) %>%
  mean

latex_number(v_21,
             "drop_bidder_v_21",
             format = "f",
             big.mark = ",",
             digits = 2)

latex_number(v_32,
             "drop_bidder_v_32",
             format = "f",
             big.mark = ",",
             digits = 2)

latex_number(v_31,
             "drop_bidder_v_31",
             format = "f",
             big.mark = ",",
             digits = 2)

#===============================================================================
# make the main auction data table
#===============================================================================
options(knitr.kable.NA = '')
main_auction_tbl <-
  tbl_data_baseline %>%
  left_join(select(final_leases, Lease_Number, InSample)) %>%
  replace_na(list(InSample = FALSE)) %>%
  filter(InSample) %>%
  mutate(NBidders = if_else(NBids <= 2, as.character(NBids), "3+")) %>%
  arrange(Date, Tract, desc(BidPerAcre)) %>%
  group_by(Date, Tract) %>%
  mutate(BidRank = row_number()) %>%
  ungroup %>%
  select(Date, Tract, MinBidPerAcre, NBidders, BidPerAcre, ValPerAcre = v) %>%
  group_by(Date, Tract, MinBidPerAcre, NBidders) %>%
  summarize(Bonus = max(BidPerAcre),
            ReserveMargin = log(max(BidPerAcre)) - log(max(MinBidPerAcre)),
            MarkupMargin = log(max(BidPerAcre)) - log(max2(BidPerAcre)),
            MarkupMargin2 = log(max(BidPerAcre)) - log(min(BidPerAcre)),
            ValueMargin = log(max(ValPerAcre)) - log(max2(ValPerAcre)),
            GainMargin = log(max(ValPerAcre)) - log(min(ValPerAcre))) %>%
  group_by(NBidders) %>%
  summarize(Auctions = n(),
            AvgReserve = mean(MinBidPerAcre),
            AvgBonus = mean(Bonus),
            AvgReserveMargin = mean(ReserveMargin),
            AvgMarkupMargin = mean(MarkupMargin),
            AvgMarkupMargin2 = mean(MarkupMargin2),            
            AvgValueMargin = mean(ValueMargin),
            AvgGainMargin = mean(GainMargin)) %>%
  mutate(Fraction = Auctions / sum(Auctions)) %>%
  mutate(AvgMarkupMargin = if_else(NBidders == 1,
                                   NA_real_,
                                   AvgMarkupMargin),
         AvgMarkupMargin2 = if_else(NBidders <= 2,
                                    NA_real_,
                                    AvgMarkupMargin2),         
         AvgValueMargin = if_else(NBidders == 1,
                                  NA_real_,
                                  AvgValueMargin),
         AvgGainMargin = if_else(NBidders <= 2,
                                 NA_real_,
                                 AvgGainMargin)) %>%
  select(NBidders, Auctions, AvgReserve, AvgBonus, AvgReserveMargin,
         AvgMarkupMargin, AvgMarkupMargin2, AvgValueMargin, AvgGainMargin) %>%
  kable(digits = c(2, 2, 0, 0, 2, 2, 2, 2),
        format.args = list(big.mark = ','),
        format = "latex",
        booktabs = TRUE,
        linesep = "",
        escape = FALSE,
        align = "c",
        col.names = c("Bids", "Auctions", 
                      "Average Reserve",                      
                      "Average Bonus",
                      "Average Reserve Margin",
                      "Average 1-to-2 Markup",
                      "Average 1-to-N Markup",            
                      "Average 1-to-2 Allocative Gain",
                      "Average 1-to-N Allocative Gain")) %>%
  column_spec(1:10, "6em")
  
write(main_auction_tbl,
      file = file.path(root, "output/tables/auction_number_bids.tex"),
      append = FALSE, sep = " " )

# presentation version with no fraction of sample colum 
main_auction_tbl_prez <-
  tbl_data_baseline %>%
  left_join(select(final_leases, Lease_Number, InSample)) %>%
  replace_na(list(InSample = FALSE)) %>%
  filter(InSample) %>%
  mutate(NBidders = if_else(NBids <= 5, as.character(NBids), "6+")) %>%
  arrange(Date, Tract, desc(BidPerAcre)) %>%
  group_by(Date, Tract) %>%
  mutate(BidRank = row_number()) %>%
  ungroup %>%
  select(Date, Tract, MinBidPerAcre, NBidders, BidPerAcre, ValPerAcre = v) %>%
  group_by(Date, Tract, MinBidPerAcre, NBidders) %>%
  summarize(Bonus = max(BidPerAcre),
            ReserveMargin = log(max(BidPerAcre)) - log(max(MinBidPerAcre)),
            MarkupMargin = log(max(BidPerAcre)) - log(max2(BidPerAcre)),
            ValueMargin = log(max(ValPerAcre)) - log(max2(ValPerAcre)),
            GainMargin = log(max(ValPerAcre)) - log(min(ValPerAcre))) %>%
  group_by(NBidders) %>%
  summarize(Auctions = n(),
            AvgBonus = mean(Bonus),
            AvgReserveMargin = mean(ReserveMargin),
            AvgMarkupMargin = mean(MarkupMargin),
            AvgValueMargin = mean(ValueMargin),
            AvgGainMargin = mean(GainMargin)) %>%
  mutate(AvgMarkupMargin = if_else(NBidders == 1,
                                   NA_real_,
                                   AvgMarkupMargin),
         AvgValueMargin = if_else(NBidders == 1,
                                  NA_real_,
                                  AvgValueMargin),
         AvgGainMargin = if_else(NBidders == 1,
                                 NA_real_,
                                 AvgGainMargin)) %>%
  select(NBidders, Auctions, AvgBonus,
         AvgReserveMargin, AvgMarkupMargin, AvgValueMargin, AvgGainMargin) %>%
  kable(digits = c(2, 2, 0, 2, 2),
        format.args = list(big.mark = ','),
        format = "latex",
        booktabs = TRUE,
        linesep = "",
        escape = FALSE,
        align = "c",
        col.names = c("Bids", "Auctions", 
                      "Average Bonus",
                      "Average Reserve Margin",
                      "Average Markup",
                      "Average 1-to-2 Allocative Gain",
                      "Average 1-to-N Allocative Gain")) %>%
  column_spec(3:7, "6em")
  
write(main_auction_tbl_prez,
      file = file.path(root, "output/tables/auction_number_bids_prez.tex"),
      append = FALSE, sep = " " )

#===============================================================================
# make the appendix robustness version
#===============================================================================
appendix_row0 <-
  tbl_data_half %>%
  left_join(select(final_leases, Lease_Number, InSample)) %>%
  replace_na(list(InSample = FALSE)) %>%
  filter(InSample, NBids > 1) %>%
  arrange(Date, Tract, desc(BidPerAcre)) %>%
  group_by(Date, Tract) %>%
  mutate(BidRank = row_number()) %>%
  ungroup %>%
  filter(BidRank <= 2) %>%
  select(Date, Tract, MinBidPerAcre, BidPerAcre, ValPerAcre = v) %>%
  group_by(Date, Tract, MinBidPerAcre) %>%
  summarize(MarkupMargin = log(max(BidPerAcre)) - log(min(BidPerAcre)),
            ValueMargin = log(max(ValPerAcre)) - log(min(ValPerAcre))) %>%
  ungroup %>%
  summarize(N = n(),
            MarkupMargin = mean(MarkupMargin),
            ValueMargin = mean(ValueMargin)) %>%
  mutate(Sample = "All", Bandwidth = 0.5)

appendix_row1 <-
  tbl_data_baseline %>%
  left_join(select(final_leases, Lease_Number, InSample)) %>%
  replace_na(list(InSample = FALSE)) %>%
  filter(InSample, NBids > 1) %>%
  arrange(Date, Tract, desc(BidPerAcre)) %>%
  group_by(Date, Tract) %>%
  mutate(BidRank = row_number()) %>%
  ungroup %>%
  filter(BidRank <= 2) %>%
  select(Date, Tract, MinBidPerAcre, BidPerAcre, ValPerAcre = v) %>%
  group_by(Date, Tract, MinBidPerAcre) %>%
  summarize(MarkupMargin = log(max(BidPerAcre)) - log(min(BidPerAcre)),
            ValueMargin = log(max(ValPerAcre)) - log(min(ValPerAcre))) %>%
  ungroup %>%
  summarize(N = n(),
            MarkupMargin = mean(MarkupMargin),
            ValueMargin = mean(ValueMargin)) %>%
  mutate(Sample = "All", Bandwidth = 1)

appendix_row2 <-
  tbl_data_double %>%
  left_join(select(final_leases, Lease_Number, InSample)) %>%
  replace_na(list(InSample = FALSE)) %>%
  filter(InSample, NBids > 1) %>%
  arrange(Date, Tract, desc(BidPerAcre)) %>%
  group_by(Date, Tract) %>%
  mutate(BidRank = row_number()) %>%
  ungroup %>%
  filter(BidRank <= 2) %>%
  select(Date, Tract, MinBidPerAcre, BidPerAcre, ValPerAcre = v) %>%
  group_by(Date, Tract, MinBidPerAcre) %>%
  summarize(MarkupMargin = log(max(BidPerAcre)) - log(min(BidPerAcre)),
            ValueMargin = log(max(ValPerAcre)) - log(min(ValPerAcre))) %>%
  ungroup %>%
  summarize(N = n(),
            MarkupMargin = mean(MarkupMargin),
            ValueMargin = mean(ValueMargin)) %>%
  mutate(Sample = "All", Bandwidth = 2)

appendix_row3 <-
  tbl_data_baseline %>%
  left_join(select(final_leases, Lease_Number, InSample)) %>%
  replace_na(list(InSample = FALSE)) %>%
  filter(InSample, NBids > 1, !HasTrimmed) %>%
  arrange(Date, Tract, desc(BidPerAcre)) %>%
  group_by(Date, Tract) %>%
  mutate(BidRank = row_number()) %>%
  ungroup %>%
  filter(BidRank <= 2) %>%
  select(Date, Tract, MinBidPerAcre, BidPerAcre, ValPerAcre = v) %>%
  group_by(Date, Tract, MinBidPerAcre) %>%
  summarize(MarkupMargin = log(max(BidPerAcre)) - log(min(BidPerAcre)),
            ValueMargin = log(max(ValPerAcre)) - log(min(ValPerAcre))) %>%
  ungroup %>%
  summarize(N = n(),
            MarkupMargin = mean(MarkupMargin),
            ValueMargin = mean(ValueMargin)) %>%
  mutate(Sample = "Drop Trimmed", Bandwidth = 1)

appendix_row4 <-
  tbl_data_baseline %>%
  left_join(select(final_leases, Lease_Number, InSample)) %>%
  replace_na(list(InSample = FALSE)) %>%
  filter(InSample, NBids > 1, !Rearranged) %>%
  arrange(Date, Tract, desc(BidPerAcre)) %>%
  group_by(Date, Tract) %>%
  mutate(BidRank = row_number()) %>%
  ungroup %>%
  filter(BidRank <= 2) %>%
  select(Date, Tract, MinBidPerAcre, BidPerAcre, ValPerAcre = v) %>%
  group_by(Date, Tract, MinBidPerAcre) %>%
  summarize(MarkupMargin = log(max(BidPerAcre)) - log(min(BidPerAcre)),
            ValueMargin = log(max(ValPerAcre)) - log(min(ValPerAcre))) %>%
  ungroup %>%
  summarize(N = n(),
            MarkupMargin = mean(MarkupMargin),
            ValueMargin = mean(ValueMargin)) %>%
  mutate(Sample = "Drop Rearranged", Bandwidth = 1)

appendix_row5 <-
  tbl_data_baseline %>%
  left_join(select(final_leases, Lease_Number, InSample)) %>%
  replace_na(list(InSample = FALSE)) %>%
  filter(InSample, NBids > 1, !Rearranged, !HasTrimmed) %>%
  arrange(Date, Tract, desc(BidPerAcre)) %>%
  group_by(Date, Tract) %>%
  mutate(BidRank = row_number()) %>%
  ungroup %>%
  filter(BidRank <= 2) %>%
  select(Date, Tract, MinBidPerAcre, BidPerAcre, ValPerAcre = v) %>%
  group_by(Date, Tract, MinBidPerAcre) %>%
  summarize(MarkupMargin = log(max(BidPerAcre)) - log(min(BidPerAcre)),
            ValueMargin = log(max(ValPerAcre)) - log(min(ValPerAcre))) %>%
  ungroup %>%
  summarize(N = n(),
            MarkupMargin = mean(MarkupMargin),
            ValueMargin = mean(ValueMargin)) %>%
  mutate(Sample = "Drop Trimmed and Rearranged", Bandwidth = 1)


appendix_tbl <-
  bind_rows(appendix_row0,
            appendix_row1,
            appendix_row2,
            appendix_row3,
            appendix_row4,
            appendix_row5) %>%
  select(Sample, Bandwidth, N, MarkupMargin, ValueMargin) %>%
  kable(digits = c(1, 1, 0, 2, 2),
        format.args = list(big.mark = ','),
        format = "latex",
        booktabs = TRUE,
        linesep = "",
        escape = FALSE,
        align = c("l", "c", "c", "c", "c"),
        col.names = c("Sample", "Bandwidth Scaling", "Auctions",
                      "Average 1-to-2 Markup",
                      "Average 1-to-2 Allocative Gain")) %>%
  column_spec(4:5, "7em")
  
write(appendix_tbl,
      file = file.path(root, "output/tables/auction_appendix.tex"),
      append = FALSE, sep = " " )
