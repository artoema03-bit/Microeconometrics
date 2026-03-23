library(ggplot2)

obs_effect <- mean(jtrain2$re78[jtrain2$train == 1]) -
  mean(jtrain2$re78[jtrain2$train == 0])
print(paste("Observed Treatment Effect:", obs_effect))

# Permutation distribution
null_dist <- replicate(1000, {
  perm <- sample(jtrain2$train)
  mean(jtrain2$re78[perm == 1]) - mean(jtrain2$re78[perm == 0])
})

# Two-sided p-value
p_value <- mean(abs(null_dist) >= abs(obs_effect))
print(paste("Randomization Inference p-value:", round(p_value, 6)))

# Plot null distribution with observed effect
ggplot(data.frame(effect = null_dist), aes(x = effect)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 30, alpha = 0.7) +
  geom_vline(xintercept = obs_effect, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Randomization Inference Null Distribution",
       x = "Simulated Treatment Effects",
       y = "Frequency") +
  theme_classic()