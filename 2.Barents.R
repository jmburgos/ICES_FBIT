# Attach fishing effort
rm(list = ls())
library(tidyverse)
library(sf)
library(rlang)

load(file = "/home/julian/Documents/WGFBIT/Barents_Sea/region_grid.RData")

path <- "/home/julian/Documents/WGFBIT/VMS/"


## Add mean SAR

for (yr in 2009:2018){

  file <- str_c(path, "OSPAR_intensity_Otter_Barents Sea_", yr, ".Rdata")

  if(file_test("-f", file)){

    load(file)
    OT <- OT %>%
      as_tibble() %>%
      dplyr::select(surface_sar, c_square)

    colname <- str_c("Otter_SAR_", yr)

    gr <- gr %>%
      left_join(OT, by = c("csquare" = "c_square")) %>%
      mutate(surface_sar = replace_na(surface_sar, 0)) %>%
      rename(!!colname :=  surface_sar)
  }


  file <- str_c(path, "OSPAR_intensity_Seine_Barents Sea_", yr, ".Rdata")

  if(file_test("-f", file)){

    load(file)
    SN <- SN %>%
      dplyr::select(surface_sar, c_square)

    colname <- str_c("Seine_SAR_", yr)

    gr <- gr %>%
      left_join(OT, by = c("csquare" = "c_square")) %>%
      mutate(surface_sar = replace_na(surface_sar, 0)) %>%
      rename(!!colname :=  surface_sar)

  }

}

## Do this better, select some columns and apply a function to them

gr <- gr %>%
  mutate(Depl2009 = 0.06 * Otter_SAR_2009,
         Depl2010 = 0.06 * Otter_SAR_2010,
         Depl2011 = 0.06 * Otter_SAR_2011 + 0.06 * Seine_SAR_2011,
         Depl2012 = 0.06 * Otter_SAR_2012 + 0.06 * Seine_SAR_2012,
         Depl2013 = 0.06 * Otter_SAR_2013 + 0.06 * Seine_SAR_2013,
         Depl2014 = 0.06 * Otter_SAR_2014 + 0.06 * Seine_SAR_2014,
         Depl2015 = 0.06 * Otter_SAR_2015 + 0.06 * Seine_SAR_2015,
         Depl2016 = 0.06 * Otter_SAR_2016 + 0.06 * Seine_SAR_2016,
         Depl2017 = 0.06 * Otter_SAR_2017 + 0.06 * Seine_SAR_2017,
         Depl2018 = 0.06 * Otter_SAR_2018)

## Add a column with the total SAR in 2009-2018

gr <- gr %>%
  mutate(sumSAR = gr %>%
           st_set_geometry(NULL) %>%
           dplyr::select(., contains("SAR")) %>%
           replace(is.na(.), 0) %>%
           rowwise() %>%
           rowSums(na.rm = TRUE))

save(gr, file = "/home/julian/Documents/WGFBIT/Barents_Sea/region_grid.RData")
