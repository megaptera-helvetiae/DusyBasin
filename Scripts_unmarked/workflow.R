## Purpose: Workflow script for unmarked frog models
## Author: Zack Steel
## Created: 8/14/18

## Set up unmarked dataframes for the three model types
source("Scripts/Functions/egg_umf.R")
source("Scripts/Functions/larvae_umf.R")
source("Scripts/Functions/frog_umf.R")
egg_umf()
larvae_umf()
frog_umf()

## Models of abundance
source("Scripts/Functions/Egg_model.R")
source("Scripts/Functions/Larvae_model.R")
source("Scripts/Functions/Frog_model.R")
egg_model()
larvae_model()
frog_model()

## Tabulate parameter estiamtes
source("Scripts/Functions/PCTabs.R")
PCTabs()

## Predictions for each lake and depth, size, dryness characteristics
source("Scripts/Functions/LakePreds.R")
LakePreds()

## Marginal Effects plots
source("Scripts/Functions/MEPlots.R")
me_plots()
