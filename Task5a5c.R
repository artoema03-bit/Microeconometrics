####### Task 5a

# A first condition is that for randomized experiments, treatment is assigned 
# independently from the potential outcomes W⊥(Y(0),Y(1)). This follows from 
# randomization and ensures that, on average, treated and control groups are comparable

# In the Neyman framework, randomness is assumed to arise only that randomness comes only from
# assignment of treatment and control groups. Whereby, variation in assignment,
# if an experiment is repeated, would change, while the individuals’ potential 
# outcomes would be fixed. 

# Additionally, SUTVA must hold, allowing for no interference between units
# whereby the potential outcome for any unit do not vary with the treatments assigned to other units.
# If these hold, the treatment and control groups are (in expectation) also 
# balanced in terms of potential outcomes, allowing for the difference in means
# to recover the ATE.

# Finally, because we never observe both potential outcomes Y_i(0) and Y_i(1) for the same unit,
# their covariance cannot be estimated. As such in the formula for the variance for the ATE estimator
# researchers drop the population variance of the unit-level treatment effects Yi(1) − Yi(0)
# As a result, researchers using Neyman’s variance estimator omit this term and 
# it consequently is conservative (or upwardly biased).
# Assuming that the sample at hand can be viewed as a random sample
# from an infinite super-population we can get an alternative interpertation. 
# In a super- population framework, the uncertainty from sampling new units from 
# the infinite population accounts for the variation between individuals.


####### Task 5c

# Fisherian randomisation requires that treatment reshuffling must be conducted  
# only in ways that were possible in the real experiment.
# Looking at the LaLonde paper, we can see that the experiment was conducted 
# across multiple sites and target groups (10 locations,AFDC women, ex-drug addicts, 
# ex-offenders, and young dropouts ), further the paper notes that treatment–control 
# ratios differed across groups by design. This suggests that assignment was not 
# a single completely randomized draw over all individuals.
#
# Athey and Imbens perform their test by re-assigning treatment while simply 
# keeping the total number of treated and control units fixed (185 and 240, respectively)
# Despite noting "In order to calculate the exact p-value we need to know the exact distribution of the assignment vector" .
# More concretely,looking at their own definitions, they treat the experiment as 
# a Completely Randomized Experiment rather than a Stratified Randomized Experiment. 
# This assumes every unit in the dataset had an equal probability of being assigned to treatment
# ignoring possible constraints from the original design. If assignment was conducted 
# within sites or groups, a correct Fisherian test would need to restrict reshuffling 
# within those strata.
#
# Further there is an issue of attrition, whereby Athey and Imbens apply randomization 
# inference to a non-randomly selected sub-sample.
# More concretely, Athey and Imbens chage 185 treated labels and 260 control
# labels among a pool of 445 individuals. However, the original dataset from LaLonde 
# consisted of two randomly allocated groups: 297 treated and 425 controls
# The 445 individuals which Athey and Imbens use are a restricted subset of the original experiment.
# Since, many participants failed to complete the required interviews, the sample 
# attrition potentially biases the experimental results. As the attrition is probably not random.
# As such the probability of an individual remaining in this 445-person "survivor" pool is driven
# by unknown, unobservable factors rather than a controlled randomization process.




