## Purpose: Set up unmarked dataframe for frog PC models
## Author: Zack Steel
## Created: 11/21/18

frog_umf <- function() {
  library(tidyverse)
  library(unmarked)
  
  ## Read in data
  load("Data/Intermediate/Prepped.RData")
  
  frogs_obs <- filter(obs, year < 2009) %>%
    select(year, julian, lake = site, total_time, obs_cnt, frogs) %>%
    merge(unique(sc.orig[,c("lake", "surf_mean")]), by = "lake", all.x = T) %>%
    mutate(lake = factor(lake), #Treat as categorical
           effort = log(total_time+1) / log(surf_mean), #add one minute bc of zero times
           julian = as.integer(julian)) %>% 
    ## Assign visit (secondary sampling period) order for each year x lake
    group_by(year, lake) %>%
    arrange(julian) %>%
    mutate(visit = row_number()) %>%
    arrange(year, lake, visit) %>% # don't need these for the model
    ungroup() 
  
  ## Spread observation data
  frogs_obs_w <- select(frogs_obs, lake, year, visit, frogs) %>% 
    spread(key = visit, value = frogs) %>% 
    arrange(lake, year)
  
  ## Same for observation covariates
  julian <- select(frogs_obs, lake, year, visit, julian) %>%
    mutate(julian = scale(julian)) %>% #standardize for modeling
    spread(key = visit, value = julian)
  effort <- select(frogs_obs, lake, year, visit, effort) %>%
    mutate(effort = scale(effort)) %>% #standardize for modeling
    spread(key = visit, value = effort)
  stime <- select(frogs_obs, lake, year, visit, total_time) %>%
    mutate(stime = log(total_time + 1),
           stime = scale(stime)) %>%
    select(-total_time) %>% #standardize for modeling
    spread(key = visit, value = stime)
  obs_cnt <- select(frogs_obs, lake, year, visit, obs_cnt) %>%
    mutate(obs_cnt = scale(obs_cnt)) %>% #standardize for modeling
    spread(key = visit, value = obs_cnt)
  
  ## line up site covs
  frogs_sc <- select(frogs_obs_w, lake, year) %>%
    merge(sc.stand, all.x = T) %>%
    arrange(lake, year) #%>%
  # mutate(year = as.factor(year))
  
  ## Make sure everything lines up still
  identical(select(frogs_obs_w, lake, year), select(julian, lake, year))
  identical(select(frogs_obs_w, lake, year), select(effort, lake, year))
  identical(as.data.frame(select(frogs_obs_w, lake, year)), 
            select(frogs_sc, lake, year))
  
  ## Put y and obsCovs in correct format
  frogs_y <- select(frogs_obs_w, -lake, -year)
  julian_s <- select(julian, -lake, -year) %>%
    as.matrix()
  julian_s2 <- julian_s * julian_s #Adding a quadratic term
  effort_s <- select(effort, -lake, -year) %>%
    as.matrix()
  stime_s <- select(stime, -lake, -year) %>%
    as.matrix()
  obs_cnt_s <- select(obs_cnt, -lake, -year) %>%
    as.matrix()
  
  ## Put it into an unmarked dataframe for static abundance
  frogs_pc_um <- unmarkedFramePCount(y = frogs_y,
                                     siteCovs = frogs_sc,
                                     obsCovs = list(julian = julian_s,
                                                    julian2 = julian_s2,
                                                    effort = effort_s,
                                                    stime = stime_s,
                                                    obs_cnt = obs_cnt_s))
  
  frogs_pc_obs <- frogs_obs
  
  save(frogs_pc_um, frogs_pc_obs, sc.orig, sc.stand, 
       file = "Data/Intermediate/frog_umf.RData")
  
}