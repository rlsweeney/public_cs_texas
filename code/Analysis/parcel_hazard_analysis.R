#===============================================================================
# code to make monthly plots of parcel lease status
#===============================================================================
library(tidyverse)
library(lubridate)
library(splines)
library(lmtest)
library(sandwich)

library(RISCA)
library(fixest)
library(boot)

library(knitr)
library(kableExtra)
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
load(file.path(gen, "outcomes_earlyLeases_earlydates.Rda"))

#===============================================================================
# compute unleased spells, do some filtering
#===============================================================================
parcel_month_outcomes <- outcomes_earlyLeases_earlydates$parcel_month_outcomes
parcel_outcomes <- outcomes_earlyLeases_earlydates$parcel_outcomes

firstdate <- min(parcel_month_outcomes$Date)

spells <-
  parcel_month_outcomes %>%
  mutate(Leased = NLeases > 0) %>% 
  arrange(ControlNum, Date) %>% 
  group_by(ControlNum) %>% 
  mutate(NewSpell = case_when(row_number() == 1 ~ 1, 
                              Leased != lag(Leased) ~ 1, 
                              TRUE ~ 0)) %>% 
  mutate(Spell = cumsum(NewSpell)) %>%
  group_by(ControlNum, Spell, Leased) %>% 
  summarize(duration = n(), mindate = min(Date), maxdate = max(Date)) %>%
  ungroup %>% 
  arrange(ControlNum, Spell) %>% 
  mutate(LeftCensored = mindate == firstdate) %>%
  group_by(ControlNum) %>%
  mutate(maxspell = max(Spell),
          EndStatus = if_else(Spell < maxspell, lead(Leased), Leased)) %>%
  ungroup %>%
  mutate(RightCensored = maxdate == make_date(2019,3,1),
          CohortY = year(mindate),
          CohortYQ = paste(year(mindate), quarter(mindate), sep = "-")) %>%
  inner_join(select(parcel_outcomes, ControlNum, ParcelType, ParcelAcres, 
                    OnShale, ShalePlay, Grid10, Grid20)) %>%
  mutate(Auction = if_else(ParcelType == "STATE",1,0), Acres = ParcelAcres) %>%
  filter(OnShale, ParcelType %in% c("RAL", "STATE"), !Leased) 
  
# identify first (left) uncensored spell for each controlnum
uncensored_spells <-
  spells %>%
  filter(!LeftCensored) %>%
  group_by(ControlNum) %>%
  filter(Spell == min(Spell)) %>%
  ungroup %>%
  select(ControlNum, FirstUncensoredSpellID = Spell)

spells <-
  spells %>%
  left_join(uncensored_spells) %>%
  replace_na(list(FirstUncensoredSpellID = 0)) %>%
  mutate(FirstUncensoredSpell = Spell == FirstUncensoredSpellID) %>%
  group_by(ControlNum) %>%
  mutate(FirstSpell = Spell == min(Spell)) %>%
  ungroup %>%
  select(-FirstUncensoredSpellID)

# tabulate some stats about each of the possible samples
# for each cohorty, how many of each parcel type do we get
spell_stats_all <-
  spells %>%
  group_by(CohortY, Auction) %>%
  tally %>%
  mutate(sample = "all")

spell_stats_uncensored <-
  spells %>%
  filter(!LeftCensored) %>%
  group_by(CohortY, Auction) %>%
  tally %>%
  mutate(sample = "uncensored")

spell_stats_first <-
  spells %>%
  filter(FirstSpell) %>%
  group_by(CohortY, Auction) %>%
  tally %>%
  mutate(sample = "first")

spell_stats_fu <-
  spells %>%
  filter(FirstUncensoredSpell) %>%
  group_by(CohortY, Auction) %>%
  tally %>%
  mutate(sample = "fu")

spell_stats <-
  bind_rows(spell_stats_all, 
            spell_stats_uncensored, 
            spell_stats_first, 
            spell_stats_fu) %>%
  pivot_wider(names_from = Auction, values_from = n) %>%
  pivot_wider(names_from = sample,  
              values_from = c(`0`,`1`)) %>%
  ungroup %>%
  rename(AllRAL = `0_all`, AllState = `1_all`,
          URAL = `0_uncensored`, UState = `1_uncensored`,
          FRAL = `0_first`, FState = `1_first`,
          FURAL = `0_fu`, FUState = `1_fu`) %>%
  replace_na(list(AllRAL = 0, URAL = 0, FRAL = 0, FURAL = 0,
                  AllState = 0, UState = 0, FState = 0, FUState = 0)) %>%
  select(Year = CohortY, 
          AllRAL, AllState, 
          URAL, UState, 
          FRAL, FState, 
          FURAL, FUState) %>%  
  kable(digits = 0,
        format = "latex",
        booktabs = TRUE,
        linesep = "",
        col.names = c("Year", rep(c("RAL", "State"), 4))) %>%
  add_header_above(c(" ", 
                      "All" = 2, 
                      "Uncensored" = 2, 
                      "First" = 2, 
                      "First Uncensored" = 2))

write(spell_stats,
      file = file.path(tdir, "spell_stats.tex"),
      append = FALSE,
      sep = " ")


#===============================================================================
# inverse propensity weighted kaplan-meier curves and log rank test
# based on some wrappers around ipw.log.rank and ipw.survival from the RISCA
# package
# methods from: https://www.stat.purdue.edu/~junxie/akme.pdf
# and bootstrap suggestions from: https://www.jstor.org/stable/pdf/2532163.pdf
#===============================================================================
# what is going on in each of these models?

# standard kaplan meier estimate for survival to time t is the product up to t
# of (1 - died_at_t/still_alive_at_t)

# if we have 2 groups (treatment and control) and compute a propensity score
# then we can compute the adjusted "died_at_t" by taking the set of guys i in 
# treated (can do same for control) who die at t, compute sum_i 1/p_i, and also
# compute the number of guys j in treated who are still alive at the start oft t
# and compute sum_j 1/p_j, and then construct the adjusted survival curve, for
# treated guys, using these inverse propensity score weighted objects

# now lets do the log rank test
# with no covariate adjustment, we compare the number of treated deaths at time
# t, d_Tt, to the number of control deaths, d_Ct, weighted by the relative size
# of the risk sets.  If Y_Tt is the size of the treated risk set (number of 
# guys that are still alive at the start of t) and Y_Ct is the same for control,
# then at each period t we compute 
# d_Tt * (Y_Ct/(Y_Tt+Y_Ct)) - d_Ct * (Y_Tt/(Y_Tt+Y_Ct))
# for large N, and under the hypothesis that the two survival curves are
# identical, the sum of this object across all time periods is normally
# distributed with a computable variance

# to covariate adjust, we do the same thing as we did for the curve plotting,
# *EXCEPT* that instead of "raw" inverse propensity weights, we need to rescale
# them so that in each time period and each group the weights sum to one

# the above paper shows that as long as the propensity scores are consistently
# estimated, the "standard" standard errors/pvals/covariance matrices work
# fine without any adjustment

# here we have a bunch of fixed effects, and fixed effects can create
# consistency problems in nonlinear models, so I also implement a randomization
# inference type pval for the logrank test (what the cited paper calls 
# "boostrapping"), and, additionally, a formal bootstrap version of the 
# hypothesis test


ipwkm <- function(fm, data, duration, dead) {
  # compute the propensity scores
  m <- feglm(fm, data, family = binomial(), notes = FALSE)
  tvar <- all.vars(fm)[1]
  pscore <- predict(m, type = "response")
  newdata <- data[obs(m),]
  treatment <- pluck(newdata, tvar)
  w <- treatment/pscore + (1-treatment) / (1-pscore)
  newdata <- bind_cols(newdata, pscore = pscore, w = w)

  # make the plot of covariate adjusted kaplan-meier curves
  fig <-
    ipw.survival(pluck(newdata, duration), 
                        pluck(newdata, dead), 
                        pluck(newdata, tvar),
                        pluck(newdata, "w")) %>%
    pluck("table.surv") %>%
    as_tibble %>%
    select(times, survival, Type = variable) %>%
    mutate(Type = if_else(Type == 0, "RAL", "State"))

  return(fig)
}

ipwlrt0<- function(fm, data, duration, dead, ripvalreps = 0) {
  # estimate propensity scores
  m <- feglm(fm, data, family = binomial(), notes = FALSE)
  tvar <- all.vars(fm)[1]
  pscore <- predict(m, type = "response")
  newdata <- data[obs(m),]
  treatment <- pluck(newdata, tvar)
  w <- treatment/pscore + (1-treatment) / (1-pscore)
  newdata <- bind_cols(newdata, pscore = pscore, w = w)

  # compute point estimate
  Z0 <- ipw.log.rank(pluck(newdata, duration),
                      pluck(newdata, dead),
                      pluck(newdata, tvar),
                      pluck(newdata, "w"))

  Z0 <- c(Z0, N = nrow(newdata))

  if(ripvalreps > 0) {
    print("doing randomization inference simulations")
    # simulate a bunch of treatment realizations and associated weights
    t2 <- matrix(runif(nrow(newdata) * ripvalreps), nrow(newdata), ripvalreps)
    t2 <- t2 <= pscore
    w2 <- t2/pscore + (1-t2)/(1-pscore)
    Z1 <-
      seq(1,ripvalreps) %>%
      map_dbl(~ pluck(ipw.log.rank(pluck(newdata, duration),
                                    pluck(newdata, dead),
                                    t2[,.],
                                    w2[,.]), "statistic"))
    pval = mean(abs(Z1) >= abs(Z0$statistic))
    return(c(Z0, ripval = pval))
  } else {
    return(Z0)
  }
}

ipwlrtboot <- function(fm, data, duration, dead, B = 1000) {
  print("doing bootstrap simulations")
  b <- boot(data, 
            function(x,inds) pluck(ipwlrt0(fm, x[inds,], duration, dead), 
                                    "statistic"),
            B)

  # p-value here is going to be the quantile of |t-t0|/sd(t) that equals |t0|
  pval <- 1 - ecdf(abs(b$t-b$t0)/sd(b$t))(abs(b$t0))
  return(pval)
}

ipwlrt <- function(fm, data, duration, dead, B = 1000) {
  # compute analytic and "randomization inference" pvals
  Z <- ipwlrt0(fm, data, duration, dead, ripvalreps = B)

  # compute traditional nonparametric bootstrap pvals
  bspval <- ipwlrtboot(fm, data, duration, dead, B = B)

  return(c(Z, bspval = bspval))
}

logrank10_all <- 
  ipwlrt(Auction ~ bs(ParcelAcres, df = 4) | Grid10 + CohortY, 
          spells, "duration", "EndStatus")

logrank10_uncensored <- 
  ipwlrt(Auction ~ bs(ParcelAcres, df = 4) | Grid10 + CohortY, 
          filter(spells, !LeftCensored), "duration", "EndStatus")

logrank10_first <- 
  ipwlrt(Auction ~ bs(ParcelAcres, df = 4) + poly(CohortY, degree = 3) |
          Grid10, 
          filter(spells, FirstSpell), "duration", "EndStatus")

logrank10_fu <- 
  ipwlrt(Auction ~ bs(ParcelAcres, df = 4) + poly(CohortY, degree = 3) |
          Grid10, 
          filter(spells, FirstUncensoredSpell), "duration", "EndStatus")

stat_order <- c("statistic", "p.value", "ripval", "bspval", "N")
tbl10 <- 
  list(all = logrank10_all, 
        uncensored = logrank10_uncensored, 
        first = logrank10_first, 
        fu = logrank10_fu) %>%
  map_dfr(~ enframe(as_vector(.)), .id = "model") %>% 
  mutate(name = factor(name, levels = stat_order, ordered = TRUE)) %>%
  mutate(value = format(value, digits = 2, big.mark = ",")) %>%
  mutate(value = if_else(name == "N", str_sub(value, 1, -4), value)) %>%
  mutate(value = if_else(name == "N", str_trim(value), value)) %>%
  mutate(value = if_else(name != "N", str_sub(value, -5, -1), value)) %>%
  mutate(value = str_pad(value, 5, side = "left")) %>%
  pivot_wider(names_from = model, values_from = value) %>%
  arrange(name)

logrank20_all <- 
  ipwlrt(Auction ~ bs(ParcelAcres, df = 4) | Grid20 + CohortY, 
          spells, "duration", "EndStatus")

logrank20_uncensored <- 
  ipwlrt(Auction ~ bs(ParcelAcres, df = 4) | Grid20 + CohortY, 
          filter(spells, !LeftCensored), "duration", "EndStatus")

logrank20_first <- 
  ipwlrt(Auction ~ bs(ParcelAcres, df = 4) + poly(CohortY, degree = 3) |
          Grid20, 
          filter(spells, FirstSpell), "duration", "EndStatus")

logrank20_fu <- 
  ipwlrt(Auction ~ bs(ParcelAcres, df = 4) + poly(CohortY, degree = 3) |
          Grid20, 
          filter(spells, FirstUncensoredSpell), "duration", "EndStatus")

tbl20 <- 
  list(all = logrank20_all, 
        uncensored = logrank20_uncensored, 
        first = logrank20_first, 
        fu = logrank20_fu) %>%
  map_dfr(~ enframe(as_vector(.)), .id = "model") %>% 
  mutate(name = factor(name, levels = stat_order, ordered = TRUE)) %>%
  mutate(value = format(value, digits = 2, big.mark = ",")) %>%
  mutate(value = if_else(name == "N", str_sub(value, 1, -4), value)) %>%
  mutate(value = if_else(name == "N", str_trim(value), value)) %>%
  mutate(value = if_else(name != "N", str_sub(value, -5, -1), value)) %>%
  mutate(value = str_pad(value, 5, side = "left")) %>%
  pivot_wider(names_from = model, values_from = value) %>%
  arrange(name)

colnames <- c(" ", "All", "Uncensored", "First", "First Uncensored")
logrank_table <-
  bind_rows(tbl10, tbl20) %>%
  kable(format = "latex",
        booktabs = TRUE,
        linesep = "",
        col.names = colnames,
        align = 'lrrrr') %>%
  # kable_styling() %>%
  pack_rows("Grid 10", 1, 5) %>%
  pack_rows("Grid 20", 6, 10) %>%
  column_spec(2:5, "8em")

write(logrank_table,
      file = file.path(tdir, "logrank_stats.tex"),
      append = FALSE,
      sep = " ")

# same, but make the figures
fig10_all <- 
  ipwkm(Auction ~ bs(ParcelAcres, df = 4) | Grid10 + CohortY,
        spells, "duration", "EndStatus")

fig10_uncensored <- 
  ipwkm(Auction ~ bs(ParcelAcres, df = 4) | Grid10 + CohortY,
        filter(spells, !LeftCensored), "duration", "EndStatus")

fig10_first <- 
  ipwkm(Auction ~ bs(ParcelAcres, df = 4) + poly(CohortY, degree = 3) |
        Grid10,
        filter(spells, FirstSpell), "duration", "EndStatus")

fig10_fu <- 
  ipwkm(Auction ~ bs(ParcelAcres, df = 4) + poly(CohortY, degree = 3) |
        Grid10,
        filter(spells, FirstUncensoredSpell), "duration", "EndStatus")

fig10 <-
  bind_rows(bind_cols(fig10_all, sample = "All"),
            bind_cols(fig10_uncensored, sample = "Uncensored"),
            bind_cols(fig10_first, sample = "First"),
            bind_cols(fig10_fu, sample = "First Uncensored")) %>%
  mutate(Type = factor(Type)) %>%
  ggplot(aes(x = times, y = survival, color = Type)) +
  geom_line() +
  facet_wrap(~ sample) +
  theme_classic()

ggsave(file.path(fdir, "ipwkm10.png"),
        fig10,
        width = 7.5, height = 5, bg = "transparent")


fig20_all <- 
  ipwkm(Auction ~ bs(ParcelAcres, df = 4) | Grid20 + CohortY,
        spells, "duration", "EndStatus")

fig20_uncensored <- 
  ipwkm(Auction ~ bs(ParcelAcres, df = 4) | Grid20 + CohortY,
        filter(spells, !LeftCensored), "duration", "EndStatus")

fig20_first <- 
  ipwkm(Auction ~ bs(ParcelAcres, df = 4) + poly(CohortY, degree = 3) |
        Grid20,
        filter(spells, FirstSpell), "duration", "EndStatus")

fig20_fu <- 
  ipwkm(Auction ~ bs(ParcelAcres, df = 4) + poly(CohortY, degree = 3) |
        Grid20,
        filter(spells, FirstUncensoredSpell), "duration", "EndStatus")

fig20 <-
  bind_rows(bind_cols(fig20_all, sample = "All"),
            bind_cols(fig20_uncensored, sample = "Uncensored"),
            bind_cols(fig20_first, sample = "First"),
            bind_cols(fig20_fu, sample = "First Uncensored")) %>%
  mutate(Type = factor(Type)) %>%
  ggplot(aes(x = times, y = survival, color = Type)) +
  geom_line() +
  facet_wrap(~ sample) +
  theme_classic()

ggsave(file.path(fdir, "ipwkm20.png"),
        fig20,
        width = 7.5, height = 5, bg = "transparent")

