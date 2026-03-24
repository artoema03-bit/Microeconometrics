# Describe Fisher's inference
# Per Heß(2017), "Fisherian randomization inference produces the distribution of
# a test statistic under a designated null hypothesis, allowing a researcher to 
# assess whether the actually observed realization of the statistic is “extreme” 
# and hence whether the null hypothesis has to be rejected".
# One of the most common statistics to test is the coefficient estimate, which we 
# found to be 1.794 in task 1. It is obtained by comparing the mean value of the
# outcome of interest across groups.
# The null hypothesis consists in a sharp hypothesis of a zero treatment effect, ie:
#
# y_i (D = 1) = y_i (D = 0) for all i = 1, 2, ...
#
# Note that it's  different from testing a null average treatment effect: 
# The sharp null is stronger: it requires every individual's treatment effect
# to be zero, i.e. Y_i(0) = Y_i(1) for all i, rather than just requiring
# individual effects to cancel out on average. Rejecting it means concluding
# that at least one unit's outcome was affected by treatment.
# In particular, to obtain this distribution, we ought to compute the same test
# statistic for each possible permutation for the treatment vector. If the 
# sharp null were true for every unit, then relabeling who got treated shouldn't
# matter; otherwise, the observed statistic should occupy an extreme position in the
# null distribution.


# Recreate p-value in Athey & Imbens (4.1)

# a) Using difference in means statistic

# To get the diff. in means estimate, We could equivalently do
# y <- regression <- lm(re78 ~ train, data = jtrain2))
# alpha <- coefficients(regression)["train"]

obs_effect <- mean(jtrain2$re78[jtrain2$train == 1]) -
  mean(jtrain2$re78[jtrain2$train == 0])
print(paste("Observed Treatment Effect:", obs_effect))


# Permutation distribution
set.seed(88888)
null_dist <- replicate(1000, {
  perm <- sample(jtrain2$train)
  mean(jtrain2$re78[perm == 1]) - mean(jtrain2$re78[perm == 0])
})

# Two-sided p-value
p_value <- mean(abs(null_dist) >= abs(obs_effect))
print(paste("Randomization Inference p-value (diff in means):", round(p_value, 6)))

# Athey & Imbens present a p-value of 0.0044, which differs from our own result 
# of 0.001.
# Such a difference is not necessarily problematic, as resampling the treatment 
# variable N times evidently produces a random realization of the distribution itself.
# Mechanically, this also depends on the choice of seed in the code.
# Then, different researchers will obtain different p-values simply due to sampling
# variation in the permutation draws.


# b) Using difference in ranks statistic

y <- jtrain2$re78
R <- numeric(length(y))

compute_R <- function(x) {
  for (i in 1:length(x)) {
    first_part <- sum(x < x[i])
    ties <- sum(x == x[i])
    R[i]= first_part + 0.5 * (1 + ties) - (length(x)+1)/2
  }
  return(R)
}

y_hat <- compute_R(y)
observed_effect <- mean(y_hat[jtrain2$train == 1] - y_hat[jtrain2$train == 0])


# new perm distro
set.seed(88888)
null_distro <- replicate(1000, {
  permy <- sample(jtrain2$train)
  mean(y_hat[permy == 1] - y_hat[permy == 0])
})

# two-sided p-value
new_p_value <- mean(abs(null_distro) >= abs(y_hat))
print(paste("Randomization Inference p-value (diff in ranks):", round(new_p_value, 6)))

# The rank statistic compresses outliers and extreme values, which, as established in
# task 2, are relevant in our data. Given the relevant paper's result of 0.01,
# our p-value is slightly different, 
# In our case, both results fall below the 5% significance level, allowing us to 
# determine that the slight difference does not influence the conclusion.