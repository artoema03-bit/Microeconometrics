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

lasso_1 <- rlasso(re78 ~ age + educ + black + hisp + re74 + re75, data = jtrain2)

names ( coef ( lasso_1 ))[ coef ( lasso_1 ) != 0]
summary(lasso_1)

regression_pl1 <- lm(re78 ~ train, data = jtrain2)

summary(regression_pl1)

# The estimated treatment effect is approximately 1.79 and is statistically
# significant at the 1% level. This suggests that participation in the training
# program is associated with higher earnings in 1978.

# However, since no covariates are included in the regression, this estimate
# corresponds to a simple difference-in-means between treated and control groups.

# The Lasso selected 0 covariates. All coefficients are shrunk to zero
# This indicates that, given the penalty parameter , none of the covariates 
# (age, educ, black, hisp, re74, re75) have sufficient predictive power for earnings 
# (re78) to be included in the model.

# The main issue with this approach is that Lasso performs variable selection
# based on predictive power for the outcome (Y), not on whether variables are
# confounders. A variable may be weakly related to re78 but strongly related
# to treatment assignment. Such variables are important for causal inference,
# but may be excluded by naive Lasso approach.

# If relevant confounders are omitted, the estimated treatment effect will be
# biased due to omitted variable bias. 

#b)
# (1)

# Matrix of regressor variables

u <- as.matrix(jtrain2[, c("age", "educ", "black", "hisp", "re74", "re75")])
u <- model.matrix(~  age + educ + black + hisp + re74 + re75 - 1, data = jtrain2)

#Double Selection

regression_ds <- rlassoEffect(x = u, y = jtrain2$re78, d = jtrain2$train,
                                  method = "double selection" )

summary(regression_ds)

lasso_2 <- rlasso(train ~ age + educ + black + hisp + re74 + re75, data = jtrain2)

names ( coef ( lasso_2 ))[ coef ( lasso_2 ) != 0]

summary(lasso_2)

# Leading to once again 

regression_pl1 <- lm(re78 ~ train, data = jtrain2) 

# The double selection estimator gives us a treatment effect of 1.794 with a 
# standard error of 0.670, which is statistically significant at the 1% level.
# The estimate is identical to the one obtained in part (a). 

# The reason for this is that Lasso selects no covariates in either the outcome equation
# or the treatment equation. Therefore, the double selection procedure includes
# no additional controls, so the final estimator reduces to the same regression
# of re78 on train only.

# This suggests that, within this linear specification, none of the observed characteristics
# are strong predictors of  participation in the training program. 
# This provides no evidence of substantial imbalance in
# these observables between the treatment and control groups.

#(2)
#Dummy Variable Creation
# 1. Age
# for (i in 17:55) {
#  jtrain2[[paste0("age_", i)]] <- ifelse(jtrain2$age == i, 1, 0)
# }
# 2. Education
#for (i in 3:16) {
#  jtrain2[[paste0("educ_", i)]] <- ifelse(jtrain2$educ == i, 1, 0)
#}
# OR

u2 <- model.matrix(~  factor(age)*factor(educ) + black + hisp + re74 + re75 - 1, data = jtrain2)

regression_ds2 <- rlassoEffect(x = u2, y = jtrain2$re78, d = jtrain2$train,
                              method = "double selection" )

summary(regression_ds2)
cat (" Selected :", colnames ( u2 )[ regression_ds2 $ selection.index ], "\n")

lasso_3 <- rlasso(jtrain2$re78 ~ u2)
S_Y <- which ( coef (lasso_3)[ -1] != 0)

names ( coef ( lasso_3 ))[ coef ( lasso_3 ) != 0]

lasso_4 <- rlasso(jtrain2$train ~ u2)
S_D <- which ( coef (lasso_4)[ -1] != 0)


names ( coef ( lasso_4 ))[ coef ( lasso_4 ) != 0]

Lasso_Union <- union(S_Y, S_D)
cat (" Union :", colnames (u2)[Lasso_Union ], "\n")
X_final <- cbind(jtrain2$train, u2 [, Lasso_Union ])
colnames(X_final) [1] <- " train "
summary (lm(jtrain2$re78 ~ X_final ))

# The coefficient falls from the original 1.79 to 1.59, and becomes significant 
# not at the 1% but at the 5% level.This time, Lasso selects several variables that were previously omitted, because 
# the greater level of controls allows for previously missed nonlinearity (expand on this)
# Further it suggests that the reduction in the treatment effect suggests that 
# part of the effect estimated in (a) and (b1) was driven by omitted variable bias.
# This helps validate that compared to the naive post-Lasso approach in part 
# (a), which selects controls only based on their predictive power for the outcome, 
# the double selection procedure ensures that variables relevant for treatment 
# assignment are also included. This makes the estimator more robust.
# Concretely, The selection of several age and age–education interaction terms in the treatment 
# equation indicates that participation in the training program depends on specific 
# demographic characteristics. In particular, certain combinations of age and education 
# significantly predict treatment assignment.For example, the interaction factor(age)31:factor(educ)10 is selected in the
# treatment equation, implying that individuals with this specific combination
# of age and education are more or less likely to participate in the program
# than others. This shows that treatment assignment is not fully random but varies
# across demographic subgroups. This shows that the treatment and control groups
# are not fully balanced once nonlinearities are taken into account. 
# As such failing to control for this would lead to biased estimates of the treatment effect. 