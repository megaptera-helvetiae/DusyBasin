## Purpose: Run abundance model for eggs
## Author: Zack Steel
## Created: 8/25/18

egg_model <- function() {
  library(tidyverse)
  library(unmarked)
  
  ## read in data
  load("Data/Intermediate/egg_umf.Rdata")
  
  # summary(eggs_cnt_um)
  
  m2.nb <- pcount(~ julian + julian2 + offset(stime)
                  ~ depth + surf_lm + fish, 
                  data = eggs_cnt_um, mixture = "NB")
  
  ## Save for later
  egg_m <- m2.nb
  
  save(egg_m, eggs_cnt_um, eggs_obs, sc.orig, sc.stand, 
       file = "Models/egg_mod.RData")
}
