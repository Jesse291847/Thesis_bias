library(IsingSampler)
library(tidyverse)
library(IsingFit)
library(parSim)

source("comparison_functions.R")
source("IsingFit_correction.R")

#reading empirical 
#weights and thresholds from Cramer paper
weights <- read.delim("Cramer_par/EmpiricalWeightParameters.txt")
thresholds <- read.delim("Cramer_par/EmpiricalThresholdParameters.txt", header = FALSE)

# because I change the parameters I don't want to show the symptom names
rownames(weights) <- colnames(weights) <- paste0(rep("S", 14), 1:14)

# making changes to obtain enough variance
weights[-1, 1] <- round(weights[-1, 1] * 0.5, digits = 4)
weights[-c(1, 2), 2] <- round(weights[-c(1, 2), 2] * 0.5, digits = 4)


weights[upper.tri(weights)] <- t(weights)[upper.tri(weights)]
thresholds <- thresholds * 0.75

results <- parSim(
  
  # Conditions
  sample_size = 400000,
  
  
  
  # Setup:
  write = FALSE,
  reps = 10,
  debug = FALSE,
  
  # The simulation code:
  expression = {
    
    # Select data:
    population <- as.data.frame(IsingSampler(sample_size, as.matrix(weights), pull(thresholds)))
    
    # network estimation
    
    eLasso <- IsingFit(population, plot = FALSE, progressbar = FALSE)$weiadj
    unregularized <- univariate(population, 0)
   
    
    
    # Make a data frame with one row to return results:
    data.frame(
      Sensitivity_eLasso = calculate_sensitivity(eLasso, weights),
      Specificity_eLasso = calculate_specificity(eLasso, weights),
      Correlation_eLasso = calculate_correlation(eLasso, weights),
      SpurNegEdges_eLasso = calculate_spur_neg_edges(eLasso, weights),
      Estimation_error_eLasso = calculate_estimation_error(eLasso, weights),
      Estimation_error_unregularized = calculate_estimation_error(unregularized, weights),
      Correlation_unregularized  = calculate_correlation(unregularized , weights),
      SpurNegEdges_unregularized  = calculate_spur_neg_edges(unregularized , weights)
     
    )
  })
  
saveRDS(results, "Lasso_vs_unregularized.RDS") 


options(scipen = 999)
round(sapply(results, mean), digits = 3)
round(sapply(results, sd), digits = 3)


