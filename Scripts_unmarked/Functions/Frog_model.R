## Purpose: Run abundance model for frogs
## Author: Zack Steel
## Created: 9/6/18

frog_model <- function() {
  library(tidyverse)
  library(unmarked)
  
  ## read in data
  load("Data/Intermediate/frog_umf.Rdata")
  
  # str(frogs_pc_um)
  
  ## Run models with depth or surface area
  
  m1 <- pcount(~ julian + julian2 + stime + obs_cnt
               ~ depth_l + fish + pbasin1_dry, 
               data = frogs_pc_um, mixture = "NB")
  m2 <- pcount(~ julian + julian2 + stime + obs_cnt
               ~ surf_lm + fish + pbasin1_dry, 
               data = frogs_pc_um, mixture = "NB")
  
  ## Model comparison
  mlist <- fitList(frog_dm = m1, frog_sm = m2)
  aictab <- modSel(mlist)
  
  ## Save for later
  frog_pcm <- m2
  
  save(frog_pcm, aictab, frogs_pc_um, frogs_pc_obs, sc.orig, sc.stand,
       file = "Models/frog_pc_mod.RData")
  
}