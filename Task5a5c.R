####### Task 5a

# A first condition is that for randomized experiments, treatment is assigned 
# independently from the potential outcomes W⊥(Y(0),Y(1)). This follows from 
# randomization and ensures that, on average, treated and control groups are 
# comparable. The Neyman framework assumes that randomness comes only from
# assignment of treatment and control groups. Whereby, variation in assignment,
# if an experiment is repeated, would change, while the individuals’ potential 
# outcomes would be fixed. 
# Additionally, SUTVA must hold, allowing for no interference between units
# whereby the potential outcome for any unit do not vary with the treatments
# assigned to other units.
# Under these conditions, the treatment and control groups are balanced in expectation
# in terms of potential outcomes, so the difference in sample means provides an unbiased
# estimator of the average treatment effect (ATE).

# Finally, knowing that in the Neyman framework, the true variance of 
# the difference-in-means estimator is: 
# V(τ_hat) = S1^2 / N_t + S0^2 / N_c - S_tau^2 / N
#
# where:
# S1^2    = variance of Y_i(1)
# S0^2    = variance of Y_i(0)
# S_tau^2 = variance of individual treatment effects (Y_i(1) - Y_i(0))

# We can say that, because we never observe both potential outcomes Y_i(0) and
# Y_i(1) for the same unit, the variance of individual treatment effects Yi(1)−Yi(0)
# cannot be estimated when we have heterogeneous treatment effects. As a result, 
# the Neyman variance estimator omits this term, leading to a  conservative 
# (upward-biased) estimate of the variance. This issue arises because, in the 
# finite-population framework, the sample is fixed
# and only treatment assignment varies. As a result, we cannot observe individual
# treatment effects, so heterogeneity across individuals appears as missing information.

# In contrast, under an infinite population framework, the sample is viewed
# as randomly drawn from a larger population. In this case, heterogeneity in treatment
# effects is reflected in observable variation across individuals and is captured
# by the sample variances. Or as Athey and Imbens put it  "If we view the sample
# as randomly drawn from an infinite population, then the variance estimator is
# unbiased when τ_hat is used to estimate the population average treatment effect
# E[Y_i(1) - Y_i(0)], rather than theaverage treatment effect for the specific 
# sample, (1/N) * sum_i (Y_i(1) - Y_i(0))."

# Changing from a finite- to a super-population perspective shifts uncertainty
# from missing information to sampling variation, which in turn changes how we
# interpret the variance estimator. Since the variance of τ_hat determines
# standard errors and confidence intervals, correctly specifying it is crucial
# for valid inference, even if the estimator of the ATE itself is unbiased.

####### Task 5c

# Fisherian randomisation requires that treatment reshuffling must be conducted  
# only in ways that were possible in the real experiment. Looking at the LaLonde paper,
# we can see that the experiment was conducted across multiple sites and target groups 
# (10 locations,AFDC women, ex-drug addicts, ex-offenders, and young dropouts).
# Furthermore the paper notes that treatment-control ratios differed across groups. 
# LaLonde explicitly mentions, for example, that "the young high school target 
# group there were by design more controls than treatments". This suggests that 
# assignment was not conducted on a single completely randomized draw over all individuals.
#
# Athey and Imbens perform their test by re-assigning treatment while simply 
# keeping the total number of treated and control units fixed (185 and 260, 
# respectively). Despite noting, "In order to calculate the exact p-value we need 
# to know the exact distribution of the assignment vector". More concretely,
# looking at their own definitions, they treat the experiment as a Completely 
# Randomized Experiment rather than a Stratified Randomized Experiment. This 
# assumes every unit in the dataset had an equal probability of being assigned 
# to treatment ignoring possible constraints from the original design. If 
# assignment was conducted within sites or groups, a correct Fisherian test 
# would need to restrict reshuffling within those strata.
#
# Further there is an issue of attrition, whereby Athey and Imbens apply randomisation 
# inference to a non-randomly selected sub-sample. More concretely, Athey and Imbens 
# 445 permute 185 treated labels and 260 control labels among a pool of individuals. 
# However, the original dataset from LaLonde consisted of two 
# randomly allocated groups: 297 treated and 425 controls. The 445 individuals 
# which Athey and Imbens use are a restricted subset of the original experiment.
# Additionally, since the Fisher test relies on reshuffling treatment
# according to the known assignment mechanism, this implies that, under attrition, the assignment 
# rule may no longer correctly describe the observed sample. More specifically,
# the way treatment is distributed in the observed data is no longer the result 
# of the original randomization alone, but also of a (self-)selection process.
# As a result, exchangeability between treated and control units may no longer hold
# and the randomisation distribution used for fisherian inference may no longer reflect
# the randomisation plan of the original experiment.
# Concretely, attrition may introduce selection that depends on treatment or outcomes. LaLonde documents that 
# “many participants failed to complete these interviews, and this sample 
# attrition potentially biases the experimental results.” Moreover,
# response rates differ between treatment and control groups (e.g., 72% vs 68%),
# indicating that attrition may not be purely random.
# If high-outcome treated individuals are more likely to remain in the sample,
# then the observed treatment effect will be inflated. As a result, the 
# reshuffled assignments are unlikely to generate differences as large as the observed one.  
# This may lead to a smaller p-value and possible over-rejection of the null hypothesis