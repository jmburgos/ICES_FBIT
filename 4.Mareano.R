# Run model
rm(list = ls())
library(tidyverse)

library(lme4)
library(rje)
library(sf)

load(file = "/home/julian/Documents/WGFBIT/Norway/region_grid.RData")
load(file = "/home/julian/Documents/WGFBIT/Norway/dt.Rdata")

dt <- dt %>%
  group_by(csquare) %>%
  summarise_at(vars(biomass, LG1, LG2, LG3, LG4, LG5, LG6), list(~sum(.,na.rm=TRUE))) %>%
  mutate_at(vars(contains("LG")), list( ~ "/"(., biomass))) %>%
  left_join(gr, by = "csquare") %>%
  filter(sumSAR == 0)

# now prepare for statistical analysis
st <- tibble(
  csquare = rep(dt$csquare, 5),
  ID = rep(dt$csquare,5),
  Cumb = c(dt$LG1,dt$LG1 + dt$LG2, dt$LG1 + dt$LG2 + dt$LG3, dt$LG1 + dt$LG2 + dt$LG3 + dt$LG4,
           dt$LG1 + dt$LG2 + dt$LG3 + dt$LG4 + dt$LG5),
  Longevity = c(rep(2,nrow(dt)),rep(5,nrow(dt)),rep(10,nrow(dt)), rep(20,nrow(dt)),
                rep(50,nrow(dt))),
  temp = rep(dt$temp, 5),
  depth = rep(dt$depth, 5),
  grain = rep(dt$grain, 5)) %>%
  mutate(ll = log(Longevity),
         Cumb = if_else(Cumb < 1e-3, 1e-3, Cumb), # add a small number to values very close to 0 and 1
         Cumb = if_else(Cumb > 0.999, 0.999, Cumb)) %>%
  drop_na()


# fit a linear mixed model with sampling station as random factor and MSFD habitats as exploratory variable
mod1   <-  glmer(Cumb ~ ll + (1 | ID), data=st, family=binomial)
mod2   <-  glmer(Cumb ~ ll + scale(temp) +  (1 | ID), data=st, family=binomial)
mod3   <-  glmer(Cumb ~ ll + scale(depth) +  (1 | ID), data=st, family=binomial)
mod4   <-  glmer(Cumb ~ ll + scale(grain) +  (1 | ID), data=st, family=binomial)
mod5   <-  glmer(Cumb ~ ll + scale(temp) + scale(depth) + scale(grain) +  (1 | ID), data=st, family=binomial)
mod6   <-  glmer(Cumb ~ ll + scale(temp) * ll+ scale(depth) + scale(grain) +  (1 | ID), data=st, family=binomial)
mod7   <-  glmer(Cumb ~ ll + scale(temp) + scale(depth) * ll+ scale(grain) +  (1 | ID), data=st, family=binomial)
mod8   <-  glmer(Cumb ~ ll + scale(temp) + scale(depth) + scale(grain) * ll+  (1 | ID), data=st, family=binomial)

aic <- AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)

which.min(aic$AIC)
modcoeff  <-  fixef(mod7) #Extract the fixed-effects estimates

save(modcoeff,file="/home/julian/Documents/WGFBIT/Norway/Coefficients_Bdata.RData")
