## Purpose: Run abundance model for frogs
## Author: Zack Steel
## Created: 9/6/18

frog_model <- function() {
  library(tidyverse)
  library(unmarked)
  
  ## read in data
  load("Data/Intermediate/frog_umf.Rdata")
  
  # str(frogs_pc_um)
  
  m <- pcount(~ julian + julian2 + offset(stime)
               ~ depth + surf_lm + fish, 
               data = frogs_pc_um, mixture = "NB")
  
  ## Save for later
  frog_pcm <- m
  
  save(frog_pcm, frogs_pc_um, frogs_pc_obs, sc.orig, sc.stand,
       file = "Models/frog_pc_mod.RData")
  
}