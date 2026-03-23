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

# At a 5% significance level, only the difference in `nodegree` is statistically significant.
# At a 10% significance level, also the difference in `hisp` is statistically significant.
# At these significance levels we would expect to find only 1 in 20 and 1 in 10 significance differences, respectively, so there is some cause for concern on the sample being balanced.
# However, given that this a smaller subset of a randomized sample, these imbalances could have also arisen due to bad luck.

# b)
# regress re78 on train, save estimate and SE of the coefficient as scalars

regression <- lm(re78 ~ train, data = jtrain2)
summary(regression)

alpha <- coefficients(regression)["train"]
beta  <- summary(regression)$coefficients["train", "Std. Error"]

# The coefficient implies that participation in the training program is associated with an increase of 1.79 thousand 1982 dollars in 1978 real earnings , ceteris paribus.
# The coefficient is significant at a 1% significance level.

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

# The removal of the top/bottom 3, 5, and 10 influential observations reduced the treatment coefficient from 1.68 to 1.35 to 1.22 to 1.02 thousand 1982 dollars, respectively.
# This amounts to a 40% decrease from the baseline to the 10 observation case.
# Likewise, the SEs for the estimated was also reduced, but to a lesser extent than the treatment coefficients as the significance dropped to only the 5% significance level.

# Task 2

# a)
vars_j3 <- c("age", "educ", "black", "hisp", "re74", "re75")

TABLE_1j3 <- datasummary_balance(~ train , fmt = 3, data = jtrain3[, c("train", vars_j3)], dinm_statistic = "p.value", title = "Balance table for jtrain3 (using train)")
TABLE_1j3[[1]][[1]] <- "jtrain3 (train)"

TABLE_1_2a <- rbind(TABLE_1, TABLE_1j3)

TABLE_1_2a

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

# Testing correlation
cor.test(jtrain3$treated, jtrain3$treated_2)

# As expected, fail to reject the null of zero correlation

# d)
# appending Table_1

vars_j3 <- c("age", "educ", "black", "hisp", "re74", "re75")
TABLE_1_2d <- datasummary_balance(~ treated , fmt = 3, data = jtrain3[, c("treated", vars_j3)], dinm_statistic = "p.value")
TABLE_1_2d[[1]][[1]] <- "jtrain3 (treated)"

TABLE_1_2d <- rbind(TABLE_1_2a, TABLE_1_2d)

TABLE_1_2d

# Only the difference for one variable `age` is statistically significant at a 10% significance level.
# This is expected as `treated` is a randomly assigned variable, so it is plausible that one variable out of 6 is unbalanced.
# With more variables, we would expect 1 out of 10 to be unbalanced.

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

# The coefficient on treated is very unstable and statistically insignificant.
# This is expected as `treated` is random.

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
