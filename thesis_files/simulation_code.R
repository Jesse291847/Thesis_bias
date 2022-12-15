library(parSim)
library(IsingSampler)
library(igraph)
library(tidyverse)
library(reshape2)
library(IsingFit)
library(glmnet)
source("comparison_functions.R")
source("IsingFit_correction.R")

# population <- readRDS("objects/population_simulation.RDS")
# true_network <- readRDS("objects/true_network.RDS")


### SIMULATION SETUP ###
results <- parSim(
  
    # Conditions
    sample_size = c(500, 1000, 2500, 5000),
    cutoff = c(5, 8), # only for Appendix


    # Setup:
    write = FALSE,
    reps = 100,
    debug = FALSE,
    progressbar = FALSE,
    
    nCores = 24,

    # The simulation code:
    expression = {
      
      library(parSim)
      library(IsingSampler)
      library(igraph)
      library(tidyverse)
      library(reshape2)
      library(IsingFit)
      library(glmnet)
      source("comparison_functions.R")
      source("IsingFit_correction.R")
      
      population <- readRDS("objects/large_population_sim.RDS")
      true_network <- readRDS("objects/true_network.RDS")

        # Select data:
        eligible <- population[rowSums(population) >= cutoff, ]
        selection <- eligible %>% slice_sample(n = sample_size)

        # network estimation
        regular <- IsingFit(selection, plot = FALSE, progressbar = FALSE)$weiadj
        correction <- IsingFit_correction(selection, progressbar = FALSE, plot = FALSE, min_sumscore = cutoff)$weiadj



        # Make a data frame with one row to return results:
        data.frame(
            Sensitivity_regular = calculate_sensitivity(regular, true_network),
            Specificity_regular = calculate_specificity(regular, true_network),
            Correlation_regular = calculate_correlation(regular, true_network),
            SpurNegEdges_regular = calculate_spur_neg_edges(regular, true_network),
            Estimation_error_regular = calculate_estimation_error(regular, true_network),
            Sensitivity_correction = calculate_sensitivity(correction, true_network),
            Specificity_correction = calculate_specificity(correction, true_network),
            Correlation_correction = calculate_correlation(correction, true_network),
            SpurNegEdges_correction = calculate_spur_neg_edges(correction, true_network),
            Estimation_error_correction = calculate_estimation_error(correction, true_network)
        )
    }
)

#save simulation results to external file
saveRDS(results, "results_100_runs.RDS")
# 
# #loading a large population all with at least a sum score of 5
# large_severe_pop <- readRDS("objects/large_severe_pop_sim.RDS")
# 
# #estimating the networks depends on the esimation method
# regular <- IsingFit(large_severe_pop, plot = FALSE, progressbar = FALSE)$weiadj
# correction <- IsingFit_sumscore_corr(large_severe_pop, sumscore = 5, plot = FALSE, progressbar = FALSE)$weiadj
# 
# results_single_run <- data.frame(
#   Sensitivity_regular = calculate_sensitivity(regular, true_network),
#   Specificity_regular = calculate_specificity(regular, true_network),
#   Correlation_regular = calculate_correlation(regular, true_network),
#   SpurNegEdges_regular = calculate_spur_neg_edges(regular, true_network),
#   Estimation_error_regular = calculate_estimation_error(regular, true_network),
#   Sensitivity_correction = calculate_sensitivity(correction, true_network),
#   Specificity_correction = calculate_specificity(correction, true_network),
#   Correlation_correction = calculate_correlation(correction, true_network),
#   SpurNegEdges_correction = calculate_spur_neg_edges(correction, true_network),
#   Estimation_error_correction = calculate_estimation_error(correction, true_network)
# )
# 
# #save to external file
# saveRDS(results_single_run, "results_large_pop.RDS")

