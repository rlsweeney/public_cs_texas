# read in and clean up the royalty payment data

#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
library(here)
root <- here()

library(tidyverse)
library(lubridate)
library(readxl)

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))

options(scipen = 99999)

#==============================================================================
# helper functions
#==============================================================================

process_sheet <- function(filename, sheet, debug = FALSE) {
  if(debug) {
    print(paste("doing file:", filename))
    print(paste("doing sheet:", sheet))
  }
  y <-
    read_excel(filename, sheet = sheet, col_types = c("text",
                                                      "text",
                                                      "text",
                                                      "text",
                                                      "date",
                                                      "text",
                                                      "numeric")) %>%
    mutate(sheet = sheet)
  return(y)
}

process_file <- function(filename) {
  excel_sheets(filename) %>%
    map_dfr(~ process_sheet(filename, .))
}

lifo_accounting <- function(Date, pos, neg) {
  n <- length(pos)
  newpos <- pos
  newneg <- neg

  for(i in seq(1, n)) {
    if(newneg[i] > 0) {
      for(j in seq(i, n)) {
        eat <- pmin(newneg[i], newpos[j])
        if(eat > 0) {
          newpos[j] <- newpos[j] - eat
          newneg[i] <- newneg[i] - eat
        }
      }
    }
  }
  return(tibble(LIFODate = Date, NewPos = newpos, NewNeg = newneg))
}

#==============================================================================
# read in the data
#==============================================================================
glo_royalties <-
  list.files(raw_payments,
             pattern = "Royalty_Payments",
             full.names = TRUE) %>%
  map_dfr(process_file) %>%
  extract("sheet",
          c("Production_Type", "Period"),
          "(OIL|GAS) ([[:alnum:]]+)",
          remove = TRUE) %>%
  rename(Commodity = Production_Type) %>%
  mutate(Year = as.numeric(str_sub(varProductionYearMonth, 1, 4)),
         Month = as.numeric(str_sub(varProductionYearMonth, 5, 6)),
         PaymentDate = as_date(datClearanceDate)) %>%
  mutate(ProductionDate = make_date(Year, Month, 1)) %>%
  select(Lease_Number = varLeaseNumber,
         Commodity,
         PaymentDate,
         ProductionDate,
         Payment = curPaymentAmount) %>%
  mutate(Commodity = if_else(Commodity == "GAS", "Gas", "Oil"),
         Lease_Number = str_replace(Lease_Number, "Mf", "MF")) %>%
  mutate(Lease_Number = if_else(Lease_Number == "MF12347",
                                "MF012347",
                                Lease_Number),
         Lease_Number = if_else(Lease_Number == "MF12354",
                                "MF012354",
                                Lease_Number),
         Lease_Number = if_else(Lease_Number == "MF47930",
                                "MF047930",
                                Lease_Number),
         Lease_Number = if_else(Lease_Number == "MF03868",
                                "MF103868",
                                Lease_Number),
         Lease_Number = if_else(Lease_Number == "MF0470796",
                                "MF047796",
                                Lease_Number))

#==============================================================================
# fix the MF112395 vs. MF112024 mixup, appears to be just oil, not gas
# until the December 2015.  Solution: for each ProductionDate, PaymentDate
# combo, in the oil data, swap the payments *if* the ratio of 112024 to 112395
# is smaller than 1.  The ratio, by acres, is 644, but we don't know for sure
# that it should always be that high.  However, we do know that for the most
# productive well in this section, the smaller lease should have 1/644 as much
# production as the larger lease (we verified the division order).
#==============================================================================
glo_royalties_special <-
  glo_royalties %>%
  filter(Lease_Number %in% c("MF112395", "MF112024")) %>%
  group_by(Lease_Number, Commodity, PaymentDate, ProductionDate) %>%
  summarize(Payment = sum(Payment)) %>%
  pivot_wider(names_from = Lease_Number, values_from = Payment) %>%
  filter((MF112024 != 0 | MF112395 != 0)) %>%
  mutate(r = MF112024 / MF112395) %>%
  mutate(SwapPayment = if_else(r < 1, TRUE, FALSE)) %>%
  mutate(New024 = if_else(SwapPayment, MF112395, MF112024),
         New395 = if_else(SwapPayment, MF112024, MF112395)) %>%
  select(-MF112395, -MF112024, -SwapPayment, -r) %>%
  rename(MF112395 = New395, MF112024 = New024) %>%
  ungroup %>%
  pivot_longer(c(MF112024, MF112395),
               names_to = "Lease_Number",
               values_to = "Payment")

glo_royalties <-
  glo_royalties %>%
  filter(!(Lease_Number %in% c("MF112395", "MF112024"))) %>%
  bind_rows(glo_royalties_special)

pre_lifo <-
  glo_royalties %>%
  group_by(Lease_Number, Commodity, ProductionDate) %>%
  summarize(Payment = sum(Payment))
  

#==============================================================================
# revise it to appropriately deal with negative payments
# From GLO on negative payments: The “negative payment” isn’t a payment at all.
# It’s actually the result of the company overpaying in the past 
# (something that happens quite often, in fact, and sometimes they only 
# catch it after a long time), so it’s an adjustment. If you look at the 
# records, you’ll see that even when it happens several times to the same 
# company, they usually end up with a positive amount.  There are exceptions, 
# such as if they overpaid, but now the lease has expired, and the last
# recorded transaction was a refund, then it shows as negative.
#==============================================================================
# match payment SIZES within a commodity-production date tuple, ignoring
# payment date
size_approach <-
  glo_royalties %>%
  mutate(Payment = round(Payment),
         Sign = if_else(Payment > 0, "Pos", "Neg")) %>%
  filter(abs(Payment) > 0) %>%
  mutate(Payment = abs(Payment)) %>%
  group_by(Lease_Number, Commodity, ProductionDate, Payment, Sign) %>%
  tally %>%
  ungroup %>%
  pivot_wider(c(Lease_Number, Commodity, ProductionDate, Payment),
              names_from = Sign,
              values_from = n,
              values_fill = 0) %>%
  mutate(Net = Pos - Neg) %>%
  pivot_wider(c(Lease_Number, ProductionDate, Payment),
              names_from = Commodity,
              values_from = Net,
              values_fill = 0)

# if there is a net negative payment in one commodity that can be offset in the
# other, then do it
net_positives <-
  size_approach %>%
  filter(Oil + Gas > 0) %>%
  mutate(Commodity = if_else(Gas > Oil, "Gas", "Oil"),
         Payment = Payment * (Oil + Gas)) %>%
  select(Lease_Number, ProductionDate, Commodity, Payment)

# leave net negatives as they are and see if they can be offset within the
# production date 
net_negatives <-
  size_approach %>%
  filter(Oil + Gas < 0) %>%
  pivot_longer(c(Gas, Oil)) %>%
  mutate(Payment = Payment * value) %>%
  rename(Commodity = name) %>%
  select(-value) %>%
  filter(Payment < 0)

# check if there are any net negatives at the production date level after
# adding back net_positives
revised_payments <-
  bind_rows(net_positives, net_negatives) %>%
  group_by(Lease_Number, Commodity, ProductionDate) %>%
  summarize(NetPayment = sum(Payment)) %>%
  ungroup

# do LIFO at the commodity level on remaining negative payments
revised_payments_no_lifo <-
  revised_payments %>%
  group_by(Lease_Number, Commodity) %>%
  filter(min(NetPayment) >= 0) %>%
  ungroup

revised_payments_do_lifo <-
  revised_payments %>%
  group_by(Lease_Number, Commodity) %>%
  filter(min(NetPayment) < 0) %>%
  ungroup %>%
  mutate(Pos = if_else(NetPayment > 0, NetPayment, 0),
         Neg = if_else(NetPayment < 0, -1 * NetPayment, 0)) %>%
  select(-NetPayment) %>%
  arrange(Lease_Number, Commodity, desc(ProductionDate))

revised_payments_lifo <-
  revised_payments_do_lifo %>%
  group_by(Lease_Number, Commodity) %>%
  group_modify(~ lifo_accounting(.x$ProductionDate, .x$Pos, .x$Neg)) %>%
  ungroup %>%
  rename(ProductionDate = LIFODate)

revised_payments_lifo <-
  revised_payments_do_lifo %>%
  left_join(revised_payments_lifo) %>%
  mutate(NetPayment = NewPos - NewNeg, LIFO = TRUE) %>%
  select(-Pos, -Neg, -NewPos, -NewNeg)

revised_payments_post_lifo <-
  bind_rows(revised_payments_no_lifo, revised_payments_lifo) %>%
  replace_na(list(LIFO = FALSE)) %>%
  arrange(Lease_Number, Commodity, ProductionDate)

glo_royalties <- revised_payments_post_lifo
glo_royalties_lifo_flag <-
  glo_royalties %>%
  select(Lease_Number, Commodity, LIFO) %>%
  distinct %>%
  group_by(Lease_Number, Commodity) %>%
  summarize(LIFO = max(LIFO) == 1) %>%
  ungroup

glo_royalties <-
  glo_royalties %>%
  select(-LIFO)

# save to disk
save(glo_royalties, file = file.path(gen, "glo_royalties.Rda"))
save(glo_royalties_lifo_flag,
     file = file.path(gen, "glo_royalties_lifo_flag.Rda"))


