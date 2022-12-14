library(tidyverse)
library(reshape2)

results <- readRDS("../results_100_runs.RDS")
results <- results %>% select(-c(error, errorMessage))

#reshaping results to plot
results <- melt(results, id = c("rep", "id", "cutoff", "sample_size"))
results$condition <- ifelse(grepl("regular", results$variable), "Regular", "Correction")
results$outcome <- ifelse(grepl("Sensitivity", results$variable), "Sensitivity",
                          ifelse(grepl("Neg", results$variable), "Spurious negative edges",
                                 ifelse(grepl("error", results$variable), "Estimation Error",
                                        ifelse(grepl("Specificity", results$variable), "Specificity",
                                               ifelse(grepl("Cor", results$variable), "Correlation", "Unknown")))))

p <- results %>% filter(outcome == "Estimation Error" & condition == "Correction") %>%
  ggplot(mapping = aes(x = factor(sample_size), y = value, fill = factor(cutoff))) +
  geom_boxplot() +
  facet_wrap(~outcome) +
  theme_bw() +
  scale_fill_discrete(name = "Sum score") +
  labs(x = "Sample Size", y = "") +
  ylim(c(0,max(results$value))) 

pdf("plot_appendix.pdf")
p
dev.off()

  


