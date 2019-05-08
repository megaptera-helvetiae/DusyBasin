## Purpose: Run abundance model for tad poles
## Author: Zack Steel
## Created: 8/25/18

larvae_model <- function() {
  library(tidyverse)
  library(unmarked)
  
  ## read in data
  load("Data/Intermediate/larvae_umf.Rdata")
  
  # summary(larv_cnt_um)
  
  m.nb <- pcount(~ julian + julian2 + offset(stime)
                 ~ depth + surf_lm + fish, 
                 data = larv_cnt_um, mixture = "NB")
  
  ## Save for later
  larv_m <- m.nb
  
  save(larv_m, larv_cnt_um, larvae_obs, sc.orig, sc.stand, 
       file = "Models/larv_mod.RData")
}
