# Prepare trawl bycatch data
rm(list = ls())
library(tidyverse)
library(vmstools)
library(sf)

## Load study area grid
load(file = "/home/julian/Documents/WGFBIT/Barents_Sea/region_grid.RData")

## Load longevity

ln <- read_csv("/home/julian/Documents/WGFBIT/traits/traits.csv") %>%
  dplyr::select(phylum = Phylum, class = Class, taxa = Species, L2, L2_5, L5_10, L10_20, L20_50, L50) %>%
  mutate(taxa = str_to_lower(taxa))

dt <- read_csv("/home/julian/Documents/WGFBIT/Barents_Sea/Barents Sea raw data 2011-13 and 2015-17.csv") %>%
  dplyr::select(-c(cruise, depth, phylum, class, characteristics)) %>%
  mutate(taxa = str_to_lower(taxa)) %>%
  filter(!(taxa == "pandalus borealis"),
         !(taxa == "scyphozoa"),
         !(taxa == "cyanea capillata")) %>%
  mutate(csquare = CSquare(lon, lat, 0.05)) %>%
  group_by(csquare, taxa) %>%
  dplyr::summarize(biomass = mean(biomass, na.rm = T)) %>%
  ungroup()


#%>%
#  left_join(gr %>% st_set_geometry(NULL), by = "csquare")

# Some dt taxa are not in the traits database...
m <- match(dt$taxa, ln$taxa)
unique(dt$taxa[is.na(m)])

dt <- dt %>%
  left_join(ln, by = "taxa") %>%
  rename(LG1 = L2,
         LG2 = L2_5,
         LG3 = L5_10,
         LG4 = L10_20,
         LG5 = L20_50,
         LG6 = L50) %>%
  mutate_at(vars(contains("LG")), list( ~ replace_na(., 0)))

dt <- dt %>%
  dplyr::select(csquare, taxa, biomass, LG1, LG2, LG3, LG4, LG5, LG6) %>%
  mutate(sl = LG1 + LG2 + LG3 + LG4 + LG5 + LG6,
         LG1 = LG1 / sl,
         LG2 = LG2 / sl,
         LG3 = LG3 / sl,
         LG4 = LG4 / sl,
         LG5 = LG5 / sl,
         LG6 = LG6 / sl,
         LG1 = LG1 * biomass, # multiply biomasss with fuzzy coded trait data
         LG2 = LG2 * biomass,
         LG3 = LG3 * biomass,
         LG4 = LG4 * biomass,
         LG5 = LG5 * biomass,
         LG6 = LG6 * biomass) %>%
  dplyr::select(-sl)

save(dt, file = "/home/julian/Documents/WGFBIT/Barents_Sea/dt.Rdata")


# Make geopackage only with used stations
sa <- read_sf("/home/julian/Documents/WGFBIT/Barents_Sea/study_area2/study_area.shp")

dt <- read_csv("/home/julian/Documents/WGFBIT/Barents_Sea/Barents Sea raw data 2011-13 and 2015-17.csv") %>%
  select(lon, lat, station) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  filter(st_intersects(x = ., y = sa, sparse = FALSE)) %>%
  write_sf("/home/julian/Documents/WGFBIT/Barents_Sea/used_stations.gpkg")
