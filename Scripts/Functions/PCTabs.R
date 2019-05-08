## Purpose - Tabulate AIC values and parameter estimates from PCOpen models
## Created - 7/24/18
## Author - Zack Steel

PCTabs <- function() {
  library(tidyverse)
  library(unmarked)
  
  ## Read in N-mixture models
  load("Models/egg_mod.RData")
  load("Models/larv_mod.RData")
  load("MOdels/frog_pc_mod.RData")
  
  ## Set up subfunction to pull out info
  f1 <- function(m, stage) {
    ## Pull out confidence intervals
    det.cis <- confint(m, type = "det") %>%
      data.frame() %>%
      mutate(par = row.names(.),
             type = "det",
             lower95 = X0.025,
             upper95 = X0.975) %>%
      select(par, type, lower95, upper95)
    lam.cis <- confint(m, type = "state") %>%
      data.frame() %>%
      mutate(par = row.names(.),
             type = "lambda",
             lower95 = X0.025,
             upper95 = X0.975) %>%
      select(par, type, lower95, upper95)
    
    ## Pull out basic summary info
    s.tab <- summary(m)
    
    det.tab <- s.tab$det %>%
      mutate(par = row.names(.)) %>%
      bind_cols(det.cis) 
    lam.tab <- s.tab$state %>%
      mutate(par = row.names(.)) %>%
      bind_cols(lam.cis) 
    
    out <- bind_rows(det.tab, lam.tab) %>%
      mutate(outcome = "abundance",
             stage = stage) %>%
      #note use of backticks (not quotes)
      select(stage, outcome, par, par1, type,
             Estimate, lower95, upper95, SE, z, p_val = `P(>|z|)`) %>%
      mutate_if(is.numeric, round, digits = 3)
  }
  
  ## Run through each model type
  egg_d <- f1(egg_m, stage = "egg")
  larvae_d <- f1(larv_m, stage = "larvae")
  frog_d <- f1(frog_pcm, stage = "frog")
  
  ## Put all together
  ests <- bind_rows(egg_d, larvae_d, frog_d)
  
  ## Save for later
  write.csv(ests, file = "data/Intermediate/PCPars.csv", row.names =F)
}