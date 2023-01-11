# code for cleaning the assignments data from GLO

# ==============================================================================
#  BASIC TEXAS SETUP
# ==============================================================================
library(here)
root <- here()

library(tidyverse)
library(lubridate)
library(readxl)

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "functions", "utils.R"))

time_buffer <- 184

# ==============================================================================
# read in glo assignments
# ==============================================================================
glo_assignments_all <-
  read_csv(file.path(raw_assign, "glo_assignments.csv")) %>%
  select(Lease_Number, Effective_Date, County, 
         Assignor = starts_with("Assignor"), 
         Assignee = starts_with("Assignee"),
         AssignDate = starts_with("Executed"), 
         PercAssigned_Comments = starts_with("Percent")) %>%
  filter(!is.na(Assignor)) %>%
  group_by(Lease_Number) %>%
  arrange(AssignDate) %>%
  ungroup %>%
  mutate(AssignDate = mdy(AssignDate), Effective_Date = mdy(Effective_Date))


# ==============================================================================
# Which partial assignments should we keep?
# 1. keep ones with 50% assigned or more
# 2. Get rid of ones with less than 50%
# 3. manually fix ones with ORRI issues
# ==============================================================================
assign_keep <-
  list("All rights" = 100,
       "100%" = 100, 
       "100% leasehold" = 100, 
       "All rights." = 100, 
       "All Rights" = 100,
       "all rights" = 100,
       "all Rights" = 100, 
       "100% WI" = 100, 
       "50% WI"  = 50, 
       "All rights - 100% WI" = 100,
       "100" = 100, 
       "50%" = 50, 
       "75%" = 75, 
       "59.52% leasehold" = 59.52, 
       "50% leasehold" = 50, 
       "50% rights" = 50,
       "50% WI depths below B of Olmos at 5,595'" = 50, 
       "50% Leashold in depths below the Olmos or 5,595'" = 50, 
       "50% of all depths below 5,595' (base of Olmos)" = 50,
       "100% all rights" = 100,
       "50% of 8/8ths" = 50, 
       "75% WI" = 75, 
       "92% WI" = 92, 
       "100% interest" = 100, 
       "75%" = 75,
       "All rights T Bone Spring Lime to T of Wolfcamp" = 100, 
       "100%, 75% NRI" = 75,
       "50% of Assignors rights" = 50, 
       "100% interest" = 100, 
       "100%, all rights" = 100, 
       "50 Percent of Assignors rights" = 50, 
       "87.5 Percent" = 87.5,
       "100%, burdens of ORRI" = 100, 
       "59.42% leasehold" = 59.42, 
       "80% of 100%" = 80, 
       "90% (Less 25% ORI of 8/8th)" = 90,
       "100% undivided" = 100, 
       "50% interest, less 25% ORRI, 37.50% NRI" = 50, 
       "50% of all rights" = 50, 
       "50.0%" = 50, 
       "59.42 Percent of Assignors Rights" = 59.42,
       "72.474998%" = 72.474998, 
       "75% undivided" = 75, 
       "85% WI" = 85, 
       "100% (& reserved ORI)" = 100, 
       "75% NRI reserving 25% ORRI" = 75,
       "75% WI / 90% NRI" = 75, 
       "75.180000% WI" = 75.18, 
       "80% of 100%" = 80, 
       "87.5" = 87.5, 
       "50.00%" = 50, 
       "51.41%" = 51.41)

assign_keep <-
  assign_keep %>%
  enframe %>%
  unnest %>%
  rename(PercAssigned_Comments = name, PercAssigned = value)

glo_assignments_perc <-
  glo_assignments_all %>% 
  filter(AssignDate - Effective_Date < time_buffer) %>%
  mutate(
    PercAssigned_Comments = str_replace(PercAssigned_Comments, "five", "5"),
    PercAssigned_Comments =
      str_replace(PercAssigned_Comments, "fifty|Fifty", "50"),
    PercAssigned_Comments =
      str_replace(PercAssigned_Comments, "Seventy Five", "75"),
    PercAssigned_Comments =
      str_replace(PercAssigned_Comments, " Percent| percent", "%"),
    PercAssigned_Comments = str_replace(PercAssigned_Comments, "!00", "100"),
    partial = if_else(
      str_detect(PercAssigned_Comments,
                 regex("all rights", ignore_case = T)),
      "100%", 
      str_extract(PercAssigned_Comments, "[0-9.]+%")), 
    partial = as.numeric(str_replace(partial, "%", ""))) %>%
  filter(partial >= 50 | is.na(partial)) %>%
  select(-partial)

glo_assignments_perc <-
  glo_assignments_perc %>%
  left_join(assign_keep, by = "PercAssigned_Comments") %>%
  mutate(PercAssigned = if_else(
           str_detect(PercAssigned_Comments,
                      regex("all right", ignore.case = T)) &
           !str_detect(PercAssigned_Comments, "of"),
           100,
           as.numeric(PercAssigned)))

# here we include a manual check of all partial assignments that occur within
# 6 months of a lease's effective date that is partially assigned

# check <-
#   glo_assignments %>%
#   filter(is.na(PercAssigned))
#
# write_csv(check, path =
#             file.path(int, "assignments/partial_assignments.csv"))

manual <-
  file.path(int, "assignments/partial_assignments.csv") %>%
  read_csv() %>%
  filter(!is.na(PercAssigned)) %>%
  mutate(AssignDate = mdy(AssignDate), 
         Effective_Date = mdy(Effective_Date))

glo_assignments_perc <-
  glo_assignments_perc %>%
  anti_join(select(manual, Lease_Number)) %>%
  bind_rows(manual)

# ==============================================================================
# read in and clean up the earlier set of assignments done manually in September
# and October of 2020
# ==============================================================================
old_manual_assignments <-
  file.path(int, "assignments/manual_pass.xlsx") %>%
  read_excel(col_types = c("text", "date", "text", "date", "date",
                           "text", "text", "text", "text")) %>%
  select(Lease_Number, Effective_Date,
         OriginalLessee = original_lessee,
         AssignmentEffectiveDate = assignment_effective_date,
         AssignmentDate = date_executed,
         Assignor = assignor,
         Assignee = assignee) %>%
  filter(Assignor != "NA", Assignee != "NA") %>%
  mutate(Effective_Date = as_date(Effective_Date),
         AssignmentEffectiveDate = as_date(AssignmentEffectiveDate),
         AssignmentDate = as_date(AssignmentDate)) %>%
  replace_na(list(AssignmentEffectiveDate = make_date(2099, 12, 31))) %>%
  mutate(AssignmentDate = pmin(AssignmentDate, AssignmentEffectiveDate)) %>%
  select(-AssignmentEffectiveDate)

# first, cap assignment date below at Effective_Date + 30
# doing this because there is a single cluster of assignments from Segundo
# Navarro to BP that actually look real but are recorded technically before the
# effective date
old_manual_assignments <-
  old_manual_assignments %>%
  mutate(AssignmentDate = if_else(AssignmentDate < Effective_Date,
                                  Effective_Date + days(30),
                                  AssignmentDate))

# second step is to filter down to earliest assignment date and then drop leases
# that don't have a single unambiguous assignee on that date
# since we didn't collect percent assinged here, just record it as 1
old_manual_assignments <-
  old_manual_assignments %>%
  arrange(Lease_Number, AssignmentDate) %>%
  group_by(Lease_Number) %>%
  filter(AssignmentDate == min(AssignmentDate)) %>%
  filter(n_distinct(Assignee) == 1) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  mutate(PercAssigned = 1) %>%
  select(Lease_Number, Effective_Date, Assignor, Assignee,
         AssignDate = AssignmentDate, PercAssigned)

# ==============================================================================
# fix assignments whose date is before the leases' effective date
# ==============================================================================
fix <-
  glo_assignments_perc %>%
  filter(AssignDate - Effective_Date < 0) %>%
  unique()

fix_all <-
  glo_assignments_perc %>%
  inner_join(select(fix, Lease_Number)) %>%
  mutate(ToFix = if_else(AssignDate - Effective_Date < 0, 1, 0)) %>%
  unique() 

# leases where there is another assignment within some N months of the 
# effective date
assign1 <- 
  fix_all %>% 
  group_by(Lease_Number) %>% 
  filter(mean(ToFix) < 1, n() > 1) %>%
  arrange(Lease_Number) %>%
  filter(ToFix == 0) %>%
  filter(abs(Effective_Date - AssignDate) < time_buffer) %>%
  group_by(Lease_Number) %>%
  arrange(Lease_Number, AssignDate) %>%
  filter(row_number() == 1)
  

glo_assignments_perc <-
  glo_assignments_perc %>%
  anti_join(select(assign1, Lease_Number))

# for the rest of the leases, manually fix the assignment dates that seem too
# early by reviewing the PDF documents on the GLO land grants website

# assign2 <-
#   fix %>% 
#   anti_join(select(assign1, Lease_Number)) %>%
#   select(-ID) %>%
#   unique
# 
# write_csv(assign2, 
#           path = file.path(int, "assignments/glo_assignments_fix.csv"))

assign2 <-
  file.path(int, "assignments/glo_assignments_fix.csv") %>%
  read_csv() %>%
  filter(!is.na(AssignDate_Imputed)) %>%
  select(-AssignDate) %>%
  rename(AssignDate = AssignDate_Imputed) %>%
  mutate(AssignDate = mdy(AssignDate), 
         Effective_Date = mdy(Effective_Date)) %>%
  select(-PercAssigned, -FixEffDate) %>%
  inner_join(select(glo_assignments_perc, Lease_Number, Assignor, Assignee, 
                   PercAssigned, PercAssigned_Comments))

glo_assignments_perc <-
  glo_assignments_perc %>%
  anti_join(select(assign2, -AssignDate))

# ==============================================================================
# bind glo assignments back together
# ==============================================================================
assignments <-
  glo_assignments_perc %>%
  bind_rows(assign1) %>%
  bind_rows(assign2) %>%
  bind_rows(old_manual_assignments) %>%
  filter(AssignDate > Effective_Date) %>%
  group_by(Lease_Number) %>%
  arrange(AssignDate) %>%
  filter(row_number() == 1) %>%
  select(-ToFix) %>%
  filter(AssignDate - Effective_Date < time_buffer) %>%
  ungroup
  
save(assignments, file = file.path(gen, "assignments.Rda"))
