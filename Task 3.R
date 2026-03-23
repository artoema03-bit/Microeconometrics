library(hdm)

# Set wd
user <- Sys.info()["user"]
output_dir <- switch(user,
  "ajnik"="G:/Mans disks/zObsidian/04 Courses/20295 Microeconometrics/Problem Sets/microeconometrics-ps",
  getwd()
)
setwd(output_dir)

# Common setup
source("./setup.R")

# Task 3

#a)

regression_lasso1 <- rlasso(re78 ~ age + educ + black + hisp + re74 + re75, data = jtrain2)

summary(regression_lasso1)

regression_postlasso <- lm(re78 ~ train, data = jtrain2)

summary(regression_postlasso)

# The Lasso selected 0 covariates. All coefficients are estimated as exactly zero. 
# This indicates that, given the penalty parameter, none of the covariates 
# (age, educ, black, hisp, re74, re75) have sufficient predictive power for earnings 
# (re78) to be included in the model.


# Key issues with inference based on this approach:
# This is simply the difference-in-means estimator, not a post-Lasso estimator
# since no covariates were selected because Lasso results showed that the chosen 
# covariates did not predict outcomes.
#
# Omitted Variable Bias might be present since the Lasso only selects variables
# that are strong predictors of the outcome. However, a variable might be a "weak"
# predictor of Y but strongly correlated with the treatment. If such a variable 
# is omitted because Lasso missed it, the treatment effect estimate will be biased 
# Another issue is that there may be Model Selection Mistakes. It is very hard 
# to guarantee that LASSO will pick the ideal set of variables.
# A single mistake in doing so may affect the final results



#b)
# (1)

# matrix of regressor variables

u <- as.matrix(jtrain2[, c("age", "educ", "black", "hisp", "re74", "re75")])

#double selection

regression_ds <- rlassoEffect(x = u, y = jtrain2$re78, d = jtrain2$train,
                                  method = "double selection" )

summary(regression_ds)

# Comments to be added:


#(2)
#Dummy Variable Creation
# 1. Age

for (i in 17:55) {
  jtrain2[[paste0("age_", i)]] <- ifelse(jtrain2$age == i, 1, 0)
}

# 2. Education

for (i in 3:16) {
  jtrain2[[paste0("educ_", i)]] <- ifelse(jtrain2$educ == i, 1, 0)
}


# OR

u2 <- model.matrix(~  factor(age)*factor(educ) + black + hisp + re74 + re75 - 1, data = jtrain2)

regression_ds2 <- rlassoEffect(x = u2, y = jtrain2$re78, d = jtrain2$train,
                              method = "double selection" )

summary(regression_ds2)



## INTERPERTATION NEEDED
