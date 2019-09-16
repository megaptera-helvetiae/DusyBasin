## Purpose: Run abundance model for eggs
## Author: Zack Steel
## Created: 8/25/18

egg_model <- function() {
  library(tidyverse)
  library(unmarked)
  
  ## read in data
  load("Data/Intermediate/egg_umf.Rdata")
  
  summary(eggs_cnt_um)
  
  m1 <- pcount(~ julian + julian2 + stime + obs_cnt
               ~ depth_l + fish + pbasin1_dry, 
               data = eggs_cnt_um, mixture = "NB")
  m2 <- pcount(~ julian + julian2 + stime + obs_cnt
               ~ surf_lm + fish + pbasin1_dry, 
               data = eggs_cnt_um, mixture = "NB")
  
  ## Model comparison
  mlist <- fitList(egg_dm = m1, egg_sm = m2)
  aictab <- modSel(mlist)
  
  ## Save best for later
  egg_m <- m2
  
  save(egg_m, aictab, eggs_cnt_um, eggs_obs, sc.orig, sc.stand, 
       file = "Models/egg_mod.RData")
}
