#===========================================================================
# BASIC TEXAS SETUP
#===========================================================================
library(here)
root <- here()

library(tidyverse)
library(lubridate)
library(sf)
library(lwgeom)

source(file.path(root, "code", "paths.R"))
source(file.path(root, "code", "texas_constants.R"))

# =========================================================================
# read in and convert public school fund shapefile
# =========================================================================
# Read the feature class
psf <- 
  file.path(raw_shape, "PSF/Chicago_PSF_180207.gdb") %>%
  st_read(layer = "Chicago_PSF_180207", stringsAsFactors = F) %>%
  st_transform(main_crs) %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid %>%
  rename(State = STATE, 
         CountyNum = COUNTYNUM, 
         Survey = SURVEY, 
         Block= BLOCK, 
         Grantee = GRANTEE, 
         SectionNum = SECTIONNUM, 
         Abstract = ABSTRACT,
         County = COUNTYNAME, 
         BasefileNum = BASEFILENUM, 
         LandType = LANDTYPE, 
         LandID = LANDID, 
         ControlNum = CONTROLNUM, 
         geometry = SHAPE) %>%
  select(-SHAPE_Length, -SHAPE_Area) %>%
  mutate(Polygon_Acres = as.numeric(st_area(.)) * m2_to_acre) %>%
  mutate_at(vars(Block, Survey, SectionNum, Abstract),
            funs(ifelse(. == "' '", NA, .))) %>%
  mutate_at(vars(Block, Survey, SectionNum, Abstract), 
            funs(ifelse(. == " ", NA, .))) %>%
  mutate_at(vars(Block, Survey, SectionNum, Abstract), 
            funs(ifelse(. == "", NA, .)))

save(psf, file = file.path(shape, "psf.Rda"))
