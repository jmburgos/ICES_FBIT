rm(list=ls())

library(tidyverse)
library(rje)

load(file = "/home/julian/Documents/WGFBIT/Barents_Sea/region_grid.RData")
gr <- gr %>%
  dplyr::filter(!is.na(grain),
                !is.na(depth),
                !is.na(temp))

load(file="/home/julian/Documents/WGFBIT/Barents_Sea/Coefficients_Bdata.RData")

(modcoeff)

#mod7   <-  glmer(Cumb ~ ll + scale(temp) + scale(depth) * ll+ scale(grain) +  (1 | ID), data=st, family=binomial)

medLong <- exp((logit(0.5) - modcoeff[1] - modcoeff[3] * scale(gr$temp) - modcoeff[4] * scale(gr$depth) - modcoeff[5] * scale(gr$grain)) / (modcoeff[2] + modcoeff[6] * scale(gr$depth)))


gr$medLong <- medLong

gr <- gr %>%
  mutate(slope = modcoeff[2] + modcoeff[6] * scale(depth),
         intercept = modcoeff[1] + modcoeff[3] * scale(temp) + modcoeff[4] * scale(depth) +
           modcoeff[5] * scale(grain))

RBS <- function(Fd,a,b){
  #  a = slope of binomial model, b = intercept of binomial model, Fd = fishing SAR x depletion rate (gear specific)

  # 3 equations
  step.size=0.5
  longevity=seq(1,200,by=step.size)
  H <- 5.31

  r = H/longevity
  K = (a*exp(a * log(longevity)+b))/(longevity * (exp(a*log(longevity) + b) + 1)^2)     #1st derivative of logistic
  B = K*(1 - Fd / r); B[B<0]=0

  RBS=sum(B)*step.size
  RBS

}

gr <-gr %>%  mutate(rbs2009 = 1,
                    rbs2010 = 1,
                    rbs2011 = 1,
                    rbs2012 = 1,
                    rbs2013 = 1,
                    rbs2014 = 1,
                    rbs2015 = 1,
                    rbs2016 = 1,
                    rbs2017 = 1,
                    rbs2018 = 1)


for (j in which(gr$Depl2009 > 0)){
  gr$rbs2009[j] <- RBS(a=gr$slope[j],b=gr$intercept[j],Fd=gr$Depl2009[j])}
for (j in which(gr$Depl2010 > 0)){
  gr$rbs2010[j] <- RBS(a=gr$slope[j],b=gr$intercept[j],Fd=gr$Depl2010[j])}
for (j in which(gr$Depl2011 > 0)){
  gr$rbs2011[j] <- RBS(a=gr$slope[j],b=gr$intercept[j],Fd=gr$Depl2011[j])}
for (j in which(gr$Depl2012 > 0)){
  gr$rbs2012[j] <- RBS(a=gr$slope[j],b=gr$intercept[j],Fd=gr$Depl2012[j])}
for (j in which(gr$Depl2013 > 0)){
  gr$rbs2013[j] <- RBS(a=gr$slope[j],b=gr$intercept[j],Fd=gr$Depl2013[j])}
for (j in which(gr$Depl2014 > 0)){
  gr$rbs2014[j] <- RBS(a=gr$slope[j],b=gr$intercept[j],Fd=gr$Depl2014[j])}
for (j in which(gr$Depl2015 > 0)){
  gr$rbs2015[j] <- RBS(a=gr$slope[j],b=gr$intercept[j],Fd=gr$Depl2015[j])}
for (j in which(gr$Depl2016 > 0)){
  gr$rbs2016[j] <- RBS(a=gr$slope[j],b=gr$intercept[j],Fd=gr$Depl2016[j])}
for (j in which(gr$Depl2017 > 0)){
  gr$rbs2017[j] <- RBS(a=gr$slope[j],b=gr$intercept[j],Fd=gr$Depl2017[j])}
for (j in which(gr$Depl2018 > 0)){
  gr$rbs2018[j] <- RBS(a=gr$slope[j],b=gr$intercept[j],Fd=gr$Depl2018[j])}

save(gr, file = "/home/julian/Documents/WGFBIT/Barents_Sea/region_grid.RData")
