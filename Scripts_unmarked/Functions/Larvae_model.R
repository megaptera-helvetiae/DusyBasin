## Purpose: Run abundance model for tad poles
## Author: Zack Steel
## Created: 8/25/18

larvae_model <- function() {
  library(tidyverse)
  library(unmarked)
  
  ## read in data
  load("Data/Intermediate/larvae_umf.Rdata")
  
  summary(larv_cnt_um)
  
  ## Run models with depth or surface area
  
  m1 <- pcount(~ julian + julian2 + stime + obs_cnt
               ~ depth_l + fish + pbasin1_dry,  
               data = larv_cnt_um, mixture = "NB")
  m2 <- pcount(~ julian + julian2 + stime + obs_cnt
               ~ surf_lm + fish + pbasin1_dry, 
               data = larv_cnt_um, mixture = "NB")
  
  ## Model comparison
  mlist <- fitList(larv_dm = m1, larv_sm = m2)
  aictab <- modSel(mlist)
  
  ## Save for later
  larv_m <- m2
  
  save(larv_m, aictab, larv_cnt_um, larvae_obs, sc.orig, sc.stand, 
       file = "Models/larv_mod.RData")
}
