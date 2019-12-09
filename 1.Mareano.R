# Prepare csquare grid with environmental data

rm(list = ls())

library(tidyverse)
library(sf)
library(raster)
library(vmstools)

# Load polygon with study area
sa <- read_sf("/home/julian/Documents/WGFBIT/Norway/study_area/study_area.shp")

## Load EEZ's
path <- "/home/julian/Documents/Mapping/EEZs/World_EEZ_v10_20180221/eez_v10.shp"
eez <- read_sf(path) %>%
  st_crop(sa) %>%
  dplyr::select(Country = Sovereign1)

## Load ICES ecoregions
path <- "/home/julian/Documents/Mapping/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp"
ecoregion <- read_sf(path)%>%
  st_crop(sa) %>%
  dplyr::select(Ecoregion)


## Load OSPAR reporting units
path <- "/home/julian/Documents/Mapping/OSPAR_Reporting_Units_20180813.gdb"
st_layers(path)

ospar <- st_read(path, layer = "OSPAR_RU_Level2_v5_170215") %>%
  st_crop(sa) %>%
  dplyr::select(OSPARL2 = L2_Region_)


## Load MSFD habitat
path <- "/home/julian/Documents/Mapping/eusm2019_model_and_confidence/EUSM2019_EUNIS_BroadscaleModel.gdb"
st_layers(path)

hab <- st_read(path, layer = "EUSM_Arctic_Atlantic")

sam <- sa %>%
  st_transform(st_crs(hab))

hab <- hab %>%
  st_crop(sam) %>%
  st_transform(4326) %>%
  dplyr::select(MSFDhab = MSFD_BBHT)

## Create grid

sabb <- st_bbox(sa)
xs <- seq(from = floor(sabb$xmin), to = ceiling(sabb$xmax), by = 0.05)
ys <- seq(from = floor(sabb$ymin), to = ceiling(sabb$ymax), by = 0.05)

gr <-as_tibble(expand_grid(lon = xs, lat = ys)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(sa), remove = FALSE) %>%
  mutate(csquare = CSquare(lon, lat, 0.05)) %>%
  st_join(sa) %>%
  filter(id == 1) %>%
  dplyr::select(-id) %>%
  st_join(eez) %>%
  st_join(ecoregion) %>%
  st_join(ospar) %>%
  st_join(hab)

# Attach environmental data
grain <- read_sf("/home/julian/Documents/WGFBIT/NGU_Sediments/Norway_regional/NGU_MarineBunnsedimenter_kornstr_regional/KornstorrelseFlate_regional.shp") %>%
  dplyr::select (grain = SEDKORNSTR) %>%
  mutate(grain = as.numeric(grain))


gr <- gr %>%
  st_join(grain)

r_temp <- raster("/home/julian/Documents/NovasArc/data/rasters/tmean.tif")
r_depth <- raster("/home/julian/Documents/NovasArc/data/rasters/bathy.tif")

xy <- gr %>%
  dplyr::select(geometry) %>%
  st_transform(crs(r_temp)) %>%
  st_coordinates()

gr <- gr %>%
  mutate(temp = extract(r_temp, xy),
         depth = extract(r_depth, xy))

save(gr, file = "/home/julian/Documents/WGFBIT/Norway/region_grid.RData")
