## Purpose: Set up unmarked dataframe for larvae occupancy models
## Author: Zack Steel
## Created: 8/14/18

larvae_umf <- function() {
  library(tidyverse)
  library(unmarked)
  
  ## Read in data
  load("Data/Intermediate/Prepped.RData")
  
  larvae_obs <- filter(obs, year < 2009) %>%
    select(year, julian, lake = site, total_time, larvae) %>%
    merge(unique(sc.orig[,c("lake", "surf_mean")]), by = "lake", all.x = T) %>%
    mutate(lake = factor(lake), #Treat as categorical
           effort = log(total_time+1) / log(surf_mean), #add one minute bc of zero times
           julian = as.integer(julian)) %>% 
    ## Assign visit (secondary sampling period) order for each year x lake
    group_by(year, lake) %>%
    arrange(julian) %>%
    mutate(visit = row_number()) %>%
    arrange(year, lake, visit) %>% 
    ungroup() 
  
  ## Spread observation data
  larv_obs_w <- select(larvae_obs, lake, year, visit, larvae) %>%
    spread(key = visit, value = larvae) %>%
    arrange(lake, year)
  
  ## Same for observation covariates
  julian <- select(larvae_obs, lake, year, visit, julian) %>%
    mutate(julian = scale(julian)) %>% #standardize for modeling
    spread(key = visit, value = julian)
  effort <- select(larvae_obs, lake, year, visit, effort) %>%
    mutate(effort = scale(effort)) %>% #standardize for modeling
    spread(key = visit, value = effort)
  stime <- select(larvae_obs, lake, year, visit, total_time) %>%
    mutate(stime = log(total_time + 1),
           stime = scale(stime)) %>%
    select(-total_time) %>% #standardize for modeling
    spread(key = visit, value = stime)
  
  ## line up site covs
  larv_sc <- select(larv_obs_w, lake, year) %>%
    merge(sc.stand, all.x = T) %>%
    arrange(lake, year) %>%
    mutate(year = as.factor(year))
  
  ## Make sure everything lines up still
  identical(select(larv_obs_w, lake, year), select(julian, lake, year))
  identical(select(larv_obs_w, lake, year), select(effort, lake, year))
  identical(as.data.frame(select(larv_obs_w, lake, year)), 
            select(larv_sc, lake, year))
  
  ## Put y and obsCovs in correct format
  larv_y <- select(larv_obs_w, -lake, -year)
  julian_s <- select(julian, -lake, -year) %>%
    as.matrix()
  julian_s2 <- julian_s * julian_s #Adding a quadratic term
  effort_s <- select(effort, -lake, -year) %>%
    as.matrix()
  stime_s <- select(stime, -lake, -year) %>%
    as.matrix()
  ## Put it into an unmarked dataframe for N-mixture
  larv_cnt_um <- unmarkedFramePCount(y = larv_y,
                                     siteCovs = larv_sc,
                                     obsCovs = list(julian = julian_s,
                                                    julian2 = julian_s2,
                                                    effort = effort_s,
                                                    stime = stime_s))
  ## Set up dataframe for occupancy
  larv_y_occ <- ifelse(larv_y > 0, 1, 0)
  larv_occ_um <- unmarkedFrameOccu(y = larv_y_occ,
                                   siteCovs = larv_sc,
                                   obsCovs = list(julian = julian_s,
                                                  julian2 = julian_s2,
                                                  effort = effort_s,
                                                  stime = stime_s))
  
  ## Save
  save(larv_occ_um, larv_cnt_um,
       larvae_obs, sc.orig, sc.stand, 
       file = "Data/Intermediate/larvae_umf.RData")
  
}