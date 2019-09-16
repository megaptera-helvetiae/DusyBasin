## Purpose: Workflow script for unmarked frog models
## Project: DusyBasin
## Author: Zack Steel

## Set up unmarked dataframes for the three model types
source("Scripts_unmarked/Functions/egg_umf.R")
source("Scripts_unmarked/Functions/larvae_umf.R")
source("Scripts_unmarked/Functions/frog_umf.R")
egg_umf()
larvae_umf()
frog_umf()

## Models of abundance
source("Scripts_unmarked/Functions/Egg_model.R")
source("Scripts_unmarked/Functions/Larvae_model.R")
source("Scripts_unmarked/Functions/Frog_model.R")
egg_model()
larvae_model()
frog_model()

## Tabulate parameter estiamtes
source("Scripts_unmarked/Functions/PCTabs.R")
PCTabs()

## Predictions for each lake and depth, size, dryness characteristics
source("Scripts_unmarked/Functions/LakePreds.R")
LakePreds()

## Marginal Effects plots
source("Scripts_unmarked/Functions/MEPlots.R")
me_plots()
