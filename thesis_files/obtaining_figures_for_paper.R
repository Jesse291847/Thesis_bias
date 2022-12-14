library(qgraph)
library(ggplot2)
library(tidyverse)
library(IsingSampler)
library(ggdag)

# create collider plot
collider_plot <- collider_triangle(x = "Motivation", y = "Intelligence", "Honors") %>%
  ggdag_dseparated(controlling_for = "m", text = FALSE, use_labels = "label") +
  theme_dag()

pdf("Collider_triangle.RDS")
collider_plot
dev.off()

# create distribution plot
func_shaded <- function(x) {
  y <- dnorm(x, mean = 0.5, sd = 0.15)
  y[x < 0.7] <- NA
  return(y)
}

pdf("Illustration_selection.pdf")
ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(0.5, 0.15)) +
  stat_function(fun = func_shaded, geom = "area", fill = "red", alpha = 0.5) +
  theme_void()
dev.off()

### constructing plot for true network ###

# reading empirical weights and thresholds from Cramer
weights <- read.delim("Cramer_par/EmpiricalWeightParameters.txt")
thresholds <- read.delim("Cramer_par/EmpiricalThresholdParameters.txt", header = FALSE)

# because I change the parameters I don't want to show the symtpom names
rownames(weights) <- colnames(weights) <- paste0(rep("S", 14), 1:14)

# making changes to obtain enough variance
weights[-1, 1] <- round(weights[-1, 1] * 0.5, digits = 4)
weights[-c(1, 2), 2] <- round(weights[-c(1, 2), 2] * 0.5, digits = 4)


weights[upper.tri(weights)] <- t(weights)[upper.tri(weights)]
thresholds <- thresholds * 0.75

# getting plot true network plot for paper
pdf("true_network.pdf")
qgraph(weights, theme = "colorblind", layout = "spring")
dev.off()

# calculate densitiy and average edge strenght
all_edges <- weights[upper.tri(weights)]
non_zero <- all_edges[all_edges != 0]

average_strength <- mean(abs(non_zero))
density <- length(non_zero) / length(all_edges)


# Save to file:
saveRDS(weights, "objects/true_network.RDS")

### simulate datasets for simulation study ###
# # for multiple simulations not too large so easy to store in memory
# population <- as.data.frame(IsingSampler(400000, as.matrix(weights), pull(thresholds), method = "CFTP"))
# saveRDS(population, "population_simulation.RDS")

# SE: One large dataset for all simulations:
large_population <- as.data.frame(IsingSampler(10000000, as.matrix(weights), pull(thresholds), method = "CFTP"))

# Save the object:
saveRDS(large_population, "objects/large_population_sim.RDS")

# select only eligible cases
severe_pop_large <- large_population[rowSums(large_population) >= 5, ]#  %>% slice_sample(n = 400000)

#save to external file
saveRDS(severe_pop_large, "objects/large_severe_pop_sim.RDS")
