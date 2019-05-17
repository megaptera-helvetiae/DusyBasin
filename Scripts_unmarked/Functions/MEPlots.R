## Purpose - Create marginal effects plots for parameter estimates
## Created - 8/15/18
## Author - Zack Steel

me_plots <- function() {
  library(unmarked)
  library(tidyverse)
  library(cowplot)
  library(ggrepel)
  
  ## Read in models and data
  load("Models/egg_mod.RData")
  load("Models/larv_mod.RData")
  load("Models/frog_pc_mod.RData")
  
  ## Set up regression plotting subfunction for each model
  f1_reg <- function(m, pred, type, ylab, xlab, ylim, lpos) {
    
    ## Pull out parameter names
    state_pars <- names(m@estimates@estimates$state@estimates)
    det_pars <- names(m@estimates@estimates$det@estimates)
    
    ## Make sequence on original scale to predict to
    seq_orig <- seq(min(sc.orig[,pred]), max(sc.orig[,pred]), length.out = 30)

    ## Standardize
    mp <- sc.orig[,pred] %>%
      unique() %>%
      mean()
    sdp <- sc.orig[,pred] %>%
      unique() %>%
      sd()
    seq_stand <- sapply(seq_orig, function(x) (x - mp) / sdp)

    ## Create new dataframe, where all other predictors are kept at their mean values
    new_data <- expand.grid(fish = c(0, 1), seq_stand = seq_stand) %>%
      ## Add original sequence
      merge(data.frame(seq_stand = seq_stand, seq_orig = seq_orig))
    ## insert appropriate column name for prediction
    names(new_data)[names(new_data) == "seq_stand"] <- pred
    
    ## Set up empty dataframe to add to new data
    new_pars <- c(state_pars, det_pars)
    new_pars <- new_pars[!(new_pars %in% c("(Intercept)", "julian2", pred))]
    empty_df <- matrix(ncol = length(new_pars), nrow = 0) %>%
      data.frame() %>%
      setNames(new_pars)
    
    new_data <- merge(new_data, empty_df, all.x = T)
    new_data[is.na(new_data)] <- 0
    new_data$julian2 <- new_data$julian^2
    
    ## Make some predictions
    preds <- predict(m, type = type, newdata = new_data) %>%
      bind_cols(new_data) %>%
      mutate(fish = factor(fish))
    
    ## Get z-matrix
    z_mat <- predict(m, type = type) %>%
      bind_cols(m@data@siteCovs) %>%
      select(Predicted:year) %>%
      mutate(year = as.integer(as.character(year))) %>%
      merge(sc.orig) %>%
      mutate_(pred_orig = pred) %>%
      mutate(fish = as.factor(fish),
             ## Get observed
             obs = apply(m@data@y, 1, max, na.rm = T),
             lakeid = paste0("Lake ", lake)) %>%
      select(lakeid, lake, Predicted, SE, lower, upper, fish, depth, pred_orig) %>%
      unique()

    
    ## Plot it
    ggplot(preds, aes(x = seq_orig, y = Predicted, fill = fish)) +
      geom_line(size = 2) +
      geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.4, color = NA) +
      scale_fill_manual(name = "Fish Present",
                         breaks = c(0, 1),
                         labels = c("No", "Yes"),
                         values = c("steelblue", "grey70")) +
      theme(legend.position=lpos,
            text = element_text(size = 10),
            axis.title = element_text(size = 12)) +
      xlab(xlab) +
      ylab(ylab) +
      coord_cartesian(ylim = c(0, z_mat$Predicted)) +
      ## Add predictions
      geom_point(data = z_mat,
                 mapping = aes(x = pred_orig, y = Predicted, color = fish),
                 size = 2, shape = 19, alpha = 0.9) +
      scale_color_manual(name = NULL,
                         breaks = NULL,
                         values = c("steelblue", "grey70")) +
      ## label the outlier
      geom_text_repel(data = filter(z_mat, lake == 1),
                      aes(x = pred_orig, y = Predicted, label = lakeid))
  }
  
  ## Build the plots
  ## Eggs
  p1 <- f1_reg(m = egg_m, pred = "surf_lm", type = "state", lpos = "none",
               ylab = "Egg Abundance", xlab = NULL)
  p2 <- f1_reg(m = egg_m, pred = "depth", type = "state", lpos = c(.45,.85), 
               ylab = NULL, xlab = NULL)
  ## Larvae
  p3 <- f1_reg(m = larv_m, pred = "surf_lm", type = "state", lpos = "none", 
               ylab = "Tadpole Abundance", xlab = NULL)
  p4 <- f1_reg(m = larv_m, pred = "depth", type = "state", lpos = "none", 
               ylab = NULL, xlab = NULL)
  ## Frogs
  p5 <- f1_reg(m = frog_pcm, pred = "surf_lm", type = "state", lpos = "none", 
               ylab = "Frog Abundance", xlab = "Lake Surface (log sq meters)")
  p6 <- f1_reg(m = frog_pcm, pred = "depth", type = "state", lpos = "none", 
               ylab = NULL, xlab = "Depth (meters)")
  
  p <- plot_grid(p1, p2, p3, p4, p5, p6, 
                 ncol = 2, align = "vh")
  
  save_plot("Figures/ME_abund.png", p, 
            base_height = 8, base_width = 6, dpi = 600)
  
}
