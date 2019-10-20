## Purpose: Create a 3D heat map with lake surface area, depth, and predicted abundance as teh X, Y and Z
## Author: Zack Steel
## Created: 8-26-18

LakePreds <- function() {
  library(tidyverse)
  library(unmarked)
  library(cowplot)
  
  ## Bring in models
  load("Models/egg_mod.RData")
  load("Models/larv_mod.RData")
  load("Models/frog_pc_mod.RData")
  
  ## And just predict for one year
  sc.stand <- select(sc.stand, year, lake, fish, #depth, 
                     surf_lm) %>%
    ## Get year factors to match
    mutate(year = 2000,
           year = factor(year, levels(factor(frog_pcm@data@siteCovs$year))),
           ## predictions for mean basin drying (i.e 33% original) 
           pbasin1_dry = 0) %>%
    unique()
  sc.s.egg <- mutate(sc.stand,
                     year = factor(year, levels(factor(egg_m@data@siteCovs$year)))) 
  
  ## Make predictions to actual lakes
  pred_real <- predict(egg_m, sc.s.egg, type = "state")
  pred_real2 <- predict(larv_m, sc.stand, type = "state")
  pred_real3 <- predict(frog_pcm, sc.stand, type = "state") 
  
  ## Get summary stats for observations
  mn_obs <- group_by(eggs_obs, lake, year) %>%
    summarise(max_cnt = max(eggs)) %>%
    group_by(lake) %>%
    summarise(mean_naive = mean(max_cnt),
              min_naive = min(max_cnt),
              max_naive = max(max_cnt))
  mn_obs2 <- group_by(larvae_obs, lake, year) %>%
    summarise(max_cnt = max(larvae)) %>%
    group_by(lake) %>%
    summarise(mean_naive = mean(max_cnt),
              min_naive = min(max_cnt),
              max_naive = max(max_cnt))
  mn_obs3 <- group_by(frogs_pc_obs, lake, year) %>%
    summarise(max_cnt = max(frogs)) %>%
    group_by(lake) %>%
    summarise(mean_naive = mean(max_cnt, na.rm = T), 
              min_naive = min(max_cnt, na.rm = T),
              max_naive = max(max_cnt, na.rm = T))
  
  plot_data0 <- select(sc.s.egg, lake, fish) %>%
    cbind(pred_real) %>%
    unique() %>%
    merge(mn_obs) %>%
    mutate(stage = "eggs",
           upper = ifelse(upper > max(pred_real$Predicted), 
                          max(pred_real$Predicted), upper))
  plot_data2 <- select(sc.stand, lake, fish) %>%
    cbind(pred_real2) %>%
    unique() %>%
    merge(mn_obs2) %>%
    mutate(stage = "larvae",
           upper = ifelse(upper > max(pred_real2$Predicted), 
                          max(pred_real2$Predicted), upper))
  plot_data3 <- select(sc.stand, lake, fish) %>%
    cbind(pred_real3) %>%
    unique() %>%
    merge(mn_obs3) %>%
    mutate(stage = "frogs",
           upper = ifelse(upper > max(pred_real3$Predicted), 
                          max(pred_real3$Predicted), upper))
  plot_data <- bind_rows(plot_data0, plot_data2, plot_data3) %>%
    mutate(lake = factor(lake),
           stage = factor(stage, levels = c("eggs", "larvae", "frogs"),
                          labels = c("Eggs", "Tadpoles", "Frogs")))
  
  p1 <- ggplot(plot_data, aes(lake, Predicted, color = factor(fish))) +
    facet_wrap(~factor(stage), ncol = 3, scales = "free") +
    geom_errorbar(aes(ymin=lower, ymax=upper), size = 2, width=0, alpha=0.5) +
    geom_point(shape=19, alpha=0.9, size=4) + 
    scale_color_manual(name = "Fish present",
                       breaks = c(0, 1),
                       labels = c("No", "Yes"),
                       values = c("steelblue", "grey70")) +
    theme_bw() +
    theme(#legend.position = "top", #c(0.1,-.15), 
          #legend.direction = "horizontal",
          legend.position = c(.99, .99),
          legend.justification = c(1,1),
          text = element_text(size = 12), title = element_text(size = 12),
          axis.text = element_text(size = 12)) +
    ylab("Predicted abundance") + xlab("Lake number") +
    coord_flip()
  
  ## Add surface area and depth values
  covs <- select(sc.orig, lake, proplake_dry, depth, surf_lm) %>%
    unique() %>%
    mutate(lake = factor(lake)) %>%
    gather(key = attribute, value = value, -lake) %>%
    mutate(attribute = factor(attribute, 
                              levels = c("surf_lm", "depth", "proplake_dry"),
                              labels = c("Area (log sq m)", "Depth (m)", "Prop Dry Yrs")))
  p2 <- ggplot(covs, aes(x = lake, y = value)) +
    facet_wrap(~factor(attribute), ncol = 3, scales = "free") +
    geom_bar(stat = "identity") + 
    xlab("Lake number") +
    ylab(NULL) +
    theme_bw() +
    theme(text = element_text(size = 12), title = element_text(size = 12),
          axis.text = element_text(size = 12)) + 
    coord_flip()
  
  # save_plot("Figures/LakePreds.png", p1, base_width = 8, base_height = 4)
  # save_plot("Figures/LakeChars.png", p2, base_width = 8, base_height = 4)
  
  pg <- plot_grid(p1, p2, ncol = 1, labels = c("A)", "B)"), rel_heights = c(1.1, 1))
  save_plot("Figures/Figure4.tiff", pg, base_width = 8, base_height = 6)
  
}
# }