# Prepare trawl bycatch data
rm(list = ls())
library(tidyverse)
library(vmstools)

## Load study area grid
load(file = "/home/julian/Documents/WGFBIT/Norway/region_grid.RData")

## Load longevity
ln <- read_csv("/home/julian/Documents/WGFBIT/traits/traits.csv") %>%
  dplyr::select(phylum = Phylum, class = Class, taxa = Species, L2, L2_5, L5_10, L10_20, L20_50, L50) %>%
  mutate(taxa = str_to_lower(taxa))

## Load beam trawl data
dt2 <- read_tsv("/home/julian/Documents/WGFBIT/Norway/dwca-imr_mareano_beamtrawl-v1.1/extendedmeasurementorfact.txt") %>%
  dplyr::select(id, measurementType, measurementValue) %>%
  filter(measurementType == "Biomass") %>%
  dplyr::select(id, biomass = measurementValue)

dt <- read_tsv("/home/julian/Documents/WGFBIT/Norway/dwca-imr_mareano_beamtrawl-v1.1/occurrence.txt") %>%
  dplyr::select(station = eventID,
         id,
         lat = decimalLatitude,
         lon = decimalLongitude,
         taxa = scientificName,
         phylum,
         class,
         order,
         family,
         genus) %>%
  mutate(csquare = CSquare(lon, lat, 0.05),
         phylum = str_to_lower(phylum),
         class = str_to_lower(class),
         order = str_to_lower(order),
         family = str_to_lower(family),
         genus = str_to_lower(genus),
         taxa = str_to_lower(taxa)) %>%
  left_join(dt2, by = "id") %>%
  group_by(csquare, taxa) %>%
  dplyr::summarize(biomass = mean(biomass, na.rm = T)) %>%
  ungroup() %>%
  left_join(gr %>% st_set_geometry (NULL), by = "csquare")

dt <- dt %>%
  left_join(ln, by = "taxa") %>%
  rename(LG1 = L2,
         LG2 = L2_5,
         LG3 = L5_10,
         LG4 = L10_20,
         LG5 = L20_50,
         LG6 = L50)%>%
  mutate_at(vars(contains("LG")), list( ~ replace_na(., 0)))

dt <- dt %>%
  dplyr::select(csquare, taxa, biomass, temp, depth, grain, LG1, LG2, LG3, LG4, LG5, LG6) %>%
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

save(dt, file = "/home/julian/Documents/WGFBIT/Norway/dt.Rdata")
