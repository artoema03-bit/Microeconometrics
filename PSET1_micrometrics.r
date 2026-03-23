library(stargazer)

library(kableExtra)
library(knitr)

library(RCT)


# Set wd
user <- Sys.info()["user"]
output_dir <- switch(user,
                    "ajnik"="G:/Mans disks/zObsidian/04 Courses/20295 Microeconometrics/Problem Sets/microeconometrics-ps",
                    getwd()
)
setwd(output_dir)

# Common setup
source("./setup.R")

# Task 1

# a)
# Create table to check balance across treatment groups for some covariates
vars <- c("age", "educ", "black", "hisp", "nodegree", "re74", "re75")
TABLE_1 <- datasummary_balance(~ train , fmt = 3, data = jtrain2[, c("train", vars)], dinm_statistic = "p.value", title = "Balance table")
TABLE_1[[1]][[1]] <- "jtrain2"

TABLE_1

# Comment:
# How many variables are balanced? Are you surprised?
# At a 5% significance level, only the 13 pp difference in `nodegree` is statistically significant.
# At a 10% significance level, also the difference in `hisp` is statistically significant.
# At these significance levels we would expect to find only 1 in 20 and 1 in 10 significance differences, respectively, so there is some cause for concern on the sample being balanced.
# It is possible for this to be due to random imbalance, especially so as this a smaller subset of a randomized sample, and the nature of the experimental design eases the concern from the standpoint of validity.
# ??? Given the limited representativeness of hispanics in the sampled population, it is unsurprising that the smaller control group halves their numbers.
# This also does not constitute a validity concern with respect to the randomization procedure.

# b)
# regress re78 on train, save estimate and SE of the coefficient as scalars

regression <- lm(re78 ~ train, data = jtrain2)
summary(regression)

alpha <- coefficients(regression)["train"]
alpha_se  <- summary(regression)$coefficients["train", "Std. Error"]
alpha_se


# Comment:
# Interpret the coefficient
# The coefficient from regressing real earnings (1978) on the treatment variable is 1.794, with associated standard error equal to 0.633,
# i.e. the coefficient is statistically significant at a 1% significance level.
# This means that being randomly assigned to the treatment group is associated with an increase in participant's real earnings by $1,794 by the year 1978, ceteris paribus.
# ??? Naturally, it is important to note that the regression does not include any controls, so that, while the validity of the result is not in question, its precision is not certain.

# c)
# create new table to test covariate sensitivity

regs <- list(
  regression_1 = lm(re78 ~ train, data = jtrain2),
  regression_2 = lm(re78  ~ train + age + educ + black + hisp, data = jtrain2),
  regression_3 = lm(re78  ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2)
)

rows <- get_group_counts(regs)

TABLE_2 <- modelsummary(
  regs,
  add_rows = rows,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1c"
)

TABLE_2

# Comment:
# Are your results sensitive to the introduction of covariates?
# The introduction of covariates slightly reduced the treatment coefficient from 1.79 to 1.68 thousand 1982 dollars, but it is pretty stable overall.
# Similarly, the SE marginally decreased, but remains essentially unchanged after adding the covariates.
# So, the statistical significance of the results remains intact.
# All of these point to the fact that our baseline estimate may not have been severy biased by ommitted variables.

# d)
# redoing reg_3 with dfbeta
inflm_reg <- influence.measures(regs[[3]]) # first subtask
influence_train <- inflm_reg$infmat[,"dfb.tran"] # no idea why its called that

# second subtask (rerun regression)
subsample_maker <- function( dc, x = influence_train) {
  if (!dc %in% c(3, 5, 10)) {
    stop("insert one of 3, 5, or 10")
  }
  highest <- order(x, decreasing = TRUE)[1:dc]
  lowest <- order(x)[1:dc]
  unique(c(highest, lowest))
}

t1 <- subsample_maker(dc=3)
t2 <- subsample_maker(dc=5)
t3 <- subsample_maker(dc=10)

regs_dfb <- list(
  reg_3_generic = regs[[3]],
  reg_3_drop3 = lm(re78 ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2[-t1,]),
  reg_3_drop5 = lm(re78 ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2[-t2,]),
  reg_3_drop10 = lm(re78 ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2[-t3,])
)

rows_dfb <- get_group_counts(regs_dfb)

modelsummary(
  regs_dfb,
  add_rows = rows_dfb,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1d"
)

# Comment
# The treatment coefficient changes markedly after dropping the most influential observations.
# Given the baseline coefficient of 1.68, dropping the top and bottom 10 most influential observations reduces the coefficient to 1.02,
# amounting to a loss of $658 (in 1982 dollars) in the associated change in earnings.
# ???We can also note that, as the sample size shrinks,the standard errors also decrease, as expected. This helps p-values staying low.???
# ???In conclusion, while the loss does not impinge on the statistical significance of the results,
# ???we can safely state that the regression results are indeed sensitive to influential observations.

# Likewise, the SEs for the estimates were also reduced, but to a lesser extent than the treatment coefficients as the significance dropped to only the 5% significance level.

# Task 2

# a)
vars_j3 <- c("age", "educ", "black", "hisp", "re74", "re75")

TABLE_1j3 <- datasummary_balance(~ train , fmt = 3, data = jtrain3[, c("train", vars_j3)], dinm_statistic = "p.value", title = "Balance table for jtrain3 (using train)")
TABLE_1j3[[1]][[1]] <- "jtrain3 (train)"

TABLE_1_2a <- rbind(TABLE_1, TABLE_1j3)

TABLE_1_2a

# Comment
# Table four includes, side by side, the contents of Table 1 and the
# data from running the same analysis on jtrain3.
# Note that all jtrain3 covariates are less balanced than the ones in the RCT sample.
# For example, there is a 9 years difference in the average age of units across treatment
# groups; blacks still constitute 85% of the treatment group, though the new control group
# has only 25% black units.
# The new control units also tend to have much higher baseline real earnings in
# years 1974 and 1975 (roughly $19,000 compared to $1,267-$2,107). This is especially stark,
# suggesting that the treated and control populations are fundamentally different.
# For this reason, we can state that jtrain3's control group is far less balanced than
# the RCT control group, as non-experimental controls look nothing like the treated group
# on observables and probably unobservable, too.

# b)
# new nonsense treatment (called treated)

set.seed(88888)

jtrain3$random <- runif(nrow(jtrain3))
jtrain3$random_order <- rank(jtrain3$random, ties.method = "first")
jtrain3$treated <- ifelse(jtrain3$random_order <= floor(nrow(jtrain3)/2), 0, 1)

# c)
# alternative treatment assignment method

jtrain3$key_id <- 1:nrow(jtrain3)

treatment_rand <- treatment_assign(
  jtrain3,
  key = "key_id",
  strata_varlist = NULL,
  share_control = 0.5,
  n_t = 1,
  missfits = "global",
  seed = "88888",
)

jtrain3$treated_2 <- treatment_rand$data$treat

# Testing the correlation
cor.test(jtrain3$treated, jtrain3$treated_2)

# Comment:
# Is the correlation between 'treated' and 'treated_2' statistically significant?
# The correlation is -0.0228, which is unsurprisingly low, given that both treatments
# are assigned randomly and independently of both data and each other.
# As expected, fail to reject the null of zero correlation.

# d)
# appending Table_1

vars_j3 <- c("age", "educ", "black", "hisp", "re74", "re75")
TABLE_1_2d <- datasummary_balance(~ treated , fmt = 3, data = jtrain3[, c("treated", vars_j3)], dinm_statistic = "p.value")
TABLE_1_2d[[1]][[1]] <- "jtrain3 (treated)"

TABLE_1_2d <- rbind(TABLE_1_2a, TABLE_1_2d)

TABLE_1_2d

## Only the difference for one variable `age` is statistically significant at a 10% significance level.
## This is expected as `treated` is a randomly assigned variable, so it is plausible that one variable out of 6 is unbalanced.
## With more variables, we would expect 1 out of 10 to be unbalanced.

# Comment
# What do you find corresponds to your expectations?
# The issue with the jtrain3 dataset, as stated earlier, is that
# the non-experimental, jtrain3 control group has no relation with the units
# in the control group, and that the original treatment ( variable 'train')
# is not randomly assigned.
# By randomly re-assigning treatment to the jtrain3 dataset, we obtain, as
# expected, a much more balanced set of covariates.
# For example, the age gap, the ethnicity composition gap and the real earnings
# gap in both baseline years are now gone (see table).
# The downside, naturally, is that the new treatment group has no economic interpretation,
# it is merely a coin-flip.

# e)
# appending Table_2 using "treated" treatment variable
regs_2e <- list(
  regression_e1 = lm(re78  ~ treated, data = jtrain3),
  regression_e2 = lm(re78  ~ treated + age + educ + black + hisp, data = jtrain3),
  regression_e3 = lm(re78  ~ treated + age + educ + black + hisp + re74 + re75, data = jtrain3)
)

rows_2e <- get_group_counts(regs_2e, treat = "treated")

TABLE_2_2e <- modelsummary(
  c(regs, regs_2e),
  add_rows = cbind(rows, rows_2e[,-1]),
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1c"
)

TABLE_2_2e

# Comment:
# Comment on what you find. Is it what you expected?
# The treated/control ratio has no relation at all with the actual treatment;
# the coefficients obtained by the new regressions are, expectedly, statistically
# insignificant and unstable.

# f)
# appending Table_2 using "train" treatment variable

regs_2f <- list(
  regression_f1 = lm(re78  ~ train, data = jtrain3),
  regression_f2 = lm(re78  ~ train + age + educ + black + hisp, data = jtrain3),
  regression_f3 = lm(re78  ~ train + age + educ + black + hisp + re74 + re75, data = jtrain3)
)

rows_2f <- get_group_counts(regs_2e, treat = "treated")

TABLE_2_2f <- modelsummary(
  c(regs, regs_2e, regs_2f),
  add_rows = cbind(rows, rows_2e[,-1], rows_2f[,-1]),
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1c"
)

TABLE_2_2f

# For the dataset with non-experimental controls, using `train` as the treatment dummy, the coefficient in the univariate
# model is negative: participation in the training programme is associated with a 15.2 thousand 1982 dollar drop in 1978 real earnings, ceteris paribus.
# The coefficient is also statistically significant at any conventional significance level.
# Adding covariates, the coefficient becomes less negative (1978 real earnings lower by 8.45 thousand 1982 dollars) and remains statistically significant.
# Adding past outcomes, the point estimate inverts (213 hundred 1982 dollar increase in '78 real earnings) and becomes statistically insignificant.
# These results are contrary to those of the experimental control group, both in point estimates and stability with respect to covariates.

# Comment:
# Comment on what you find. Is it what you expected? Are your results sensitive to the introduction of covariates?
# The interpretation of each new regression is interesting:
# Regressing re78 on train (Reg_ff1) yields a strongly negative coefficient. This is
# entirely a byproduct of the fact that individuals in the jtrain3 control group
# earn significantly more than those in the original RCT treatment group. Then, there
# is no causal content, though the p-value indicates strong significance.
# Regressing re78 on treatment, given the first set of controls (Reg_ff2)
# does not eliminate the bias.
# Further adding re74 and re75 further reduces the bias, but the analysis is still
# completely unable to recover the RCT coefficient (1.794). Also note that the
# associated p-value is 0.8.
# In conclusion, the non-experimental dataset is unable to recover the original
# effects, and incapable of offering causal interpretations of any measure.
