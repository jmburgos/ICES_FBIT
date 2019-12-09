# Attach fishing effort
rm(list = ls())
library(tidyverse)
library(sf)

load(file = "/home/julian/Documents/WGFBIT/Norway/region_grid.RData")

path <- "/home/julian/Documents/WGFBIT/VMS/"


## Add mean SAR

for (yr in 2009:2018){

  file1 <- str_c(path, "OSPAR_intensity_Otter_Barents Sea_", yr, ".Rdata")
  file2 <- str_c(path, "OSPAR_intensity_Otter_Norwegian Sea_", yr, ".Rdata")

  OT <- NULL
  OT1 <- NULL
  OT2 <- NULL

  if(file_test("-f", file1)){

    load(file1)
    OT1 <- OT %>%
      as_tibble() %>%
      dplyr::select(surface_sar, c_square)

  }

  if(file_test("-f", file2)){

    load(file2)
    OT2 <- OT %>%
      as_tibble() %>%
      dplyr::select(surface_sar, c_square)

  }

  OT <- rbind(OT1, OT2)

  if (!is.null(OT)){

    OT <- distinct(OT)

    colname <- str_c("Otter_SAR_", yr)
    gr <- gr %>%
      left_join(OT, by = c("csquare" = "c_square")) %>%
      mutate(surface_sar = replace_na(surface_sar, 0)) %>%
      rename(!!colname :=  surface_sar)
  }


  file1 <- str_c(path, "OSPAR_intensity_Seine_Barents Sea_", yr, ".Rdata")
  file2 <- str_c(path, "OSPAR_intensity_Seine_Norwegian Sea_", yr, ".Rdata")

  SN <- NULL
  SN1 <- NULL
  SN2 <- NULL

  if(file_test("-f", file1)){
    load(file1)
    SN1 <- SN %>%
      as_tibble() %>%
      dplyr::select(surface_sar, c_square)
  }

  if(file_test("-f", file2)){
    load(file2)
    SN2 <- SN %>%
      as_tibble() %>%
      dplyr::select(surface_sar, c_square)
  }

  SN <- rbind(SN1, SN2)

  if(!is.null(SN)){

    SN <- distinct(SN)
    colname <- str_c("Seine_SAR_", yr)

    gr <- gr %>%
      left_join(SN, by = c("csquare" = "c_square")) %>%
      mutate(surface_sar = replace_na(surface_sar, 0)) %>%
      rename(!!colname :=  surface_sar)
  }
}



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

save(gr, file = "/home/julian/Documents/WGFBIT/Norway/region_grid.RData")
