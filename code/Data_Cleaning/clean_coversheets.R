# Created by Yixin Sun 8/19/2017
# Loading in RA data entry for the RAL coversheets and recommended rentals
library(here)
root <- here()
source(file.path(root, "code/paths.R"))

library(tidyverse)
library(readxl)

#=============================================================
# load in excel sheets and format 
#=============================================================
# 2 datasets - Final_Term_Sheet and Highlighted_Terms 
# Final_Term_Sheet has every RAL lease the RA's looked at
# Highlighted_Terms are the ones they still had quesitons on that 
# Sunny went over. This section combines the two 

ral_final <-
  file.path(raw_coversheets, "Final_Term_Sheet.xlsx") %>%
  read_excel() %>%
  select(Lease_Number, Terms_Offered, BPA_Offered, Rental_Offered,
         Royalty_Offered, Terms_Rec, BPA_Rec, Rental_Rec, Royalty_Rec, 
         No_Review, No_Pdf)

# Loads in an Excel spreadsheet with the highlighted rows ONLY
ral_highlighted <-
  file.path(raw_coversheets, "Highlighted_Terms.xlsx") %>%
  read_excel() %>%
  mutate(No_Review = NA,
         No_Pdf = NA)

coversheets <- 
  ral_final %>%
  anti_join(select(ral_highlighted, Lease_Number)) %>%
  rbind(ral_highlighted) %>%
  unique() %>%
  as_tibble()

# make terms into number
coversheets <- 
  coversheets %>%
  mutate(Terms_Offered = str_replace_all(Terms_Offered, 
                                         "([0-9\\.]+).*$", 
                                         "\\1"), 
         Terms_Rec = str_replace_all(Terms_Rec, 
                                     "([0-9\\.]+).*$", 
                                     "\\1"), 
         Terms_Offered = as.numeric(Terms_Offered),
         Terms_Rec = as.numeric(Terms_Rec))

#===============================================================
# create a dataset of delay rentals 
#=================================================================
raw_rec_rentals <-
  coversheets %>%
  filter(is.na(No_Pdf), is.na(No_Review)) %>%
  select(Lease_Number, Terms_Rec, Rental_Rec)

# Separates the delay rentals by their "/"
recommended_rentals <-
  raw_rec_rentals %>%
  mutate(Rental_Recommended = strsplit(Rental_Rec, '/')) %>%
  unnest(Rental_Recommended)  %>%
  group_by(Lease_Number) %>%
  mutate(Rental_Year = row_number() + 1) %>%
  ungroup() %>%
  select(-Rental_Rec) %>%
  rename(Terms_Recommended = Terms_Rec) %>%
  as_tibble

# discount delay rentals
recommended_rentals <-
  recommended_rentals %>%
  mutate(Rental_Recommended = as.numeric(Rental_Recommended), 
         Rentals_Discounted = Rental_Recommended / (1.1 ^ (Rental_Year - 1)))


# Sends file to Dropbox
save(recommended_rentals, 
     file = file.path(gen, "recommended_rentals.Rda"))
save(coversheets, 
     file = file.path(gen, "coversheets.Rda"))
