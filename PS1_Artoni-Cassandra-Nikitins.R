# R script for Problem Set 1
# Group composition: Emanuele Artoni (3199617), Pedro Cassandra (3387647), and Arturs Janis Nikitins (3342806)

library(tidyverse)
library(RCT)
library(hdm)
library(grf)
library(sandwich)
library(lmtest)
library(modelsummary)
library(huxtable)
library(openxlsx)

options("modelsummary_factory_default" = "huxtable")

# Set wd
user <- Sys.info()["user"]
output_dir <- switch(
  user,
  "ajnik"="G:/Mans disks/zObsidian/04 Courses/20295 Microeconometrics/Problem Sets/microeconometrics-ps",
  "erick"="/home/erick/TEMP/",
  getwd()
)
setwd(output_dir)

# Common setup

# Load data
jtrain2 <- read_delim(
  "files/jtrain2.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

jtrain3 <- read_delim(
  "files/jtrain3.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

# Seed
set.seed(88888)

# Common functions
get_group_counts <- \(models, treat = "train") {

  res <- map_dfr(models, \(model){
    mf <- model.frame(model)
    if (!treat %in% names(mf)) stop("treatment variable not found in model frame")

    n1 <- sum(mf[[treat]] == 1, na.rm = TRUE)
    n0 <- sum(mf[[treat]] == 0, na.rm = TRUE)
    c(as.character(n1), as.character(n0))
  })

  res

  data.frame(
    "term" = c("N Treated ", "N Control "),
    res
  )
}

get_pvals <- \(data, vars, treat = "train") {
  res <- map_dbl(vars, \(var) {
    t.test(data[[var]][data[[treat]]==1], data[[var]][data[[treat]]==0])$p.value
  })
}

# Task 1

# a)
# Create table to check balance across treatment groups for some covariates
vars <- c("age", "educ", "black", "hisp", "nodegree", "re74", "re75")

pvals <- get_pvals(jtrain2, vars)

TABLE_1 <- datasummary_balance(~ train , fmt = 3, data = jtrain2[, c("train", vars)], add_columns = tibble(`P value` = pvals), title = "TABLE 1: Balance")
TABLE_1[[1]][[1]] <- "jtrain2"

TABLE_1

saveWorkbook(as_Workbook(TABLE_1), "out/PS1_Q1a.xlsx", overwrite = TRUE)

# Comment:
# How many variables are balanced? Are you surprised?
# At a 5% significance level, only the 13 pp difference in `nodegree` is statistically significant.
# At a 10% significance level, also the 5 pp difference in `hisp` is statistically significant.
# However, it is possible for this to be due to random imbalance, especially so as this a restricted subset of a randomized sample.
# Likewise, the randomized nature of the experimental design eases the concern from the standpoint of validity.

# b)
# regress re78 on train, save estimate and SE of the coefficient as scalars

regression <- lm(re78 ~ train, data = jtrain2)
summary(regression)

alpha <- coefficients(regression)["train"]
alpha
alpha_se  <- summary(regression)$coefficients["train", "Std. Error"]
alpha_se


# Comment:
# Interpret the coefficient
# The coefficient from regressing real earnings (1978) on the treatment variable is 1.794, with associated standard error equal to 0.633,
# i.e. the coefficient is statistically significant at a 1% significance level.
# This means that being assigned to the treatment increased participant's real earnings by on average $1,794 in 1978, measured in 1982 dollars, compared to the control group.
# Because treatment was randomly assigned in jtrain2, this difference can be interpreted causally.

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
  title = "TABLE 2: Task 1c"
)

TABLE_2

saveWorkbook(as_Workbook(TABLE_2), "out/PS1_Q1c.xlsx", overwrite = TRUE)

# Comment:
# Are your results sensitive to the introduction of covariates?
# The introduction of covariates slightly reduced the treatment coefficient from 1.79 to 1.68 thousand 1982 dollars, but it is pretty stable overall.
# Similarly, the SE marginally decreased, but remains essentially unchanged after adding the covariates.
# So, the statistical significance of the results remains intact.

# d)
# redoing reg_3 with dfbeta
influence_train <- dfbeta(regs[[3]])[,"train"]

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

TABLE_2_1d <- modelsummary(
  regs_dfb,
  add_rows = rows_dfb,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "TABLE 2: Task 1d"
)

TABLE_2_1d

saveWorkbook(as_Workbook(TABLE_2_1d), "out/PS1_Q1d.xlsx", overwrite = TRUE)

# Comment
# The treatment coefficient changes markedly after dropping the most influential observations.
# Given the baseline coefficient of 1.68, dropping the top and bottom 10 most influential observations (as determined by dfbeta) reduces the coefficient to 1.02,
# amounting to a loss of $658 (in 1982 dollars) in the associated change in earnings.
# Likewise, the SEs for the estimates were also reduced, but to a lesser extent than the treatment coefficients as the significance dropped to only the 5% significance level.
# So, while the regression results are economically sensitive to influential observations, the loss of influential observations does not impinge on the sign and statistical significance of the results.


# Task 2

# a)
vars_j3 <- c("age", "educ", "black", "hisp", "re74", "re75")

pvals_1j3 <- get_pvals(jtrain3, vars_j3)

TABLE_1j3 <- datasummary_balance(~ train , fmt = 3, data = jtrain3[, c("train", vars_j3)], add_columns = tibble(`P value` = pvals_1j3), title = "Balance table for jtrain3 (using train)")
TABLE_1j3[[1]][[1]] <- "jtrain3 (train)"

TABLE_1_2a <- rbind(TABLE_1, TABLE_1j3)

TABLE_1_2a

saveWorkbook(as_Workbook(TABLE_1_2a), "out/PS1_Q2a.xlsx", overwrite = TRUE)

# b)
# new random treatment (called treated)

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
  seed = "88888"
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

pvals_1_2d <- get_pvals(jtrain3, vars_j3, "treated")

TABLE_1_2d <- datasummary_balance(~ treated , fmt = 3, data = jtrain3[, c("treated", vars_j3)], add_columns = tibble(`P value` = pvals_1_2d))
TABLE_1_2d[[1]][[1]] <- "jtrain3 (treated)"

TABLE_1_2d <- rbind(TABLE_1_2a, TABLE_1_2d)

TABLE_1_2d

saveWorkbook(as_Workbook(TABLE_1_2d), "out/PS1_Q2d.xlsx", overwrite = TRUE)


# Comment
# What do you find corresponds to your expectations?
# The issue with the jtrain3 dataset, as stated earlier, is that the non-experimental jtrain3 control group has no relation with the units in the treatment group, and so the original treatment variable ('train') is not randomly assigned anymore.
# By randomly re-assigning treatment to the jtrain3 dataset, we obtain, as expected, a much more balanced set of covariates.
# Only the difference for one variable `age` is statistically significant at a 10% significance level.
# This is expected as `treated` is a randomly assigned variable, so it is plausible that 1 variable out of 6 is unbalanced.
# However, the new assignment has no economic interpretation.

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
  title = "TABLE 2: Task 2e"
)

TABLE_2_2e

saveWorkbook(as_Workbook(TABLE_2_2e), "out/PS1_Q2e.xlsx", overwrite = TRUE)

# Comment:
# Comment on what you find. Is it what you expected?
# The treated/control ratio has no relation at all with the actual treatment;
# the coefficients obtained by the new regressions are, expectedly, all close to zero and statistically insignificant.

# f)
# appending Table_2 using "train" treatment variable

regs_2f <- list(
  regression_f1 = lm(re78  ~ train, data = jtrain3),
  regression_f2 = lm(re78  ~ train + age + educ + black + hisp, data = jtrain3),
  regression_f3 = lm(re78  ~ train + age + educ + black + hisp + re74 + re75, data = jtrain3)
)

rows_2f <- get_group_counts(regs_2f, treat = "train")

TABLE_2_2f <- modelsummary(
  c(regs, regs_2e, regs_2f),
  add_rows = cbind(rows, rows_2e[,-1], rows_2f[,-1]),
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "TABLE 2: Task 2f"
)

TABLE_2_2f

saveWorkbook(as_Workbook(TABLE_2_2f), "out/PS1_Q2f.xlsx", overwrite = TRUE)

# Comment:
# Comment on what you find. Is it what you expected? Are your results sensitive to the introduction of covariates?
# The interpretation of each new regression is interesting:
# Regressing re78 on train (regression_f1) yields a strongly negative coefficient: participation in the training programme is associated with 15.2 thousand 1982 dollar lower 1978 real earnings.
# Adding covariates (regression_f2), the coefficient becomes less negative (8.45 thousand 1982 dollar decrease in 1978 real earnings, ceteris paribus) and remains statistically significant.
# Adding past outcomes (regression_f3), the point estimate inverts (213 '82 dollar increase in '78 real earnings, ceteris paribus) and becomes statistically insignificant.
# These results are contrary to those of the experimental control group, both in point estimates and stability with respect to covariates.
# The non-experimental dataset is unable to recover the original effects, and these estimates lack a causal interpretation without an unconfoundedness argument.

# Task 3

#a)

lasso_1 <- rlasso(re78 ~ age + educ + black + hisp + re74 + re75, data = jtrain2)


names ( coef ( lasso_1 ))[ coef ( lasso_1 ) != 0]
summary(lasso_1)

regression_pl1 <- lm(re78 ~ train, data = jtrain2)

summary(regression_pl1)

# The estimated treatment effect is approximately 1.79 and is statistically significant at the 1% level.
# This suggests a positive estimated treatment effect on 1978 earnings.

# However, since no covariates are included in the regression, this estimate
# corresponds to a simple difference-in-means between treated and control groups.

# The Lasso selected 0 covariates in the outcome equation; all coefficients were shrunk to zero.
# This indicates that, in this specification and given the penalty parameter, none of the covariates
# (age, educ, black, hisp, re74, re75) were selected as sufficient predictors for earnings (re78).

# The main issue with this approach is that Lasso performs variable selection only in the outcome equation, which can fail to provide valid inference.
# A variable may be weakly related to re78, and be excluded by the naive Lasso approach, but be strongly related to treatment assignment.
# Although generally omitting relevant controls may bias estimates, for us causal identification is not threatened as treatment is randomized.
# However, naive post-Lasso inference does become invalid as it ignores model-selection mistakes and treats the selected model as fixed.

#b)
# (1)

# Matrix of regressor variables

u <- model.matrix(~  age + educ + black + hisp + re74 + re75 - 1, data = jtrain2)

#Double Selection

regression_ds <- rlassoEffect(x = u, y = jtrain2$re78, d = jtrain2$train,
                                  method = "double selection" )

summary(regression_ds)

lasso_2 <- rlasso(train ~ age + educ + black + hisp + re74 + re75, data = jtrain2)
summary(lasso_2)

# Leading to once again
regression_pl1 <- lm(re78 ~ train, data = jtrain2)
summary(regression_pl1)

# OR
S_D1 <- which ( coef (lasso_2)[ -1] != 0)
names ( coef ( lasso_2 ))[ coef ( lasso_2 ) != 0]

S_Y1 <- which ( coef (lasso_1)[ -1] != 0)
names ( coef ( lasso_1))[ coef ( lasso_1 ) != 0]

Lasso_Union1 <- union(S_Y1, S_D1)
cat (" Union :", colnames (u)[Lasso_Union1 ], "\n")
X_final1 <- cbind(jtrain2$train, u [, Lasso_Union1 ])

colnames(X_final1) [1] <- " train "
summary (lm(jtrain2$re78 ~ X_final1 ))

# The double selection estimator gives us a treatment effect of 1.794 with a standard error of 0.670,
# which is statistically significant at the 1% level.
# The estimate is identical to the one obtained in part (a).

# The reason for this is that Lasso selects no covariates in either the outcome equation or the treatment equation.
# Therefore, the double selection procedure includes no additional controls,
# so the final estimator reduces to the same regression of re78 on train only.

# This suggests that, within this linear specification and penalty, the observed characteristics
# are weak predictors of participation in the training program and the outcome.

#(2)
#Dummy Variable Creation

u2 <- model.matrix(~  factor(age)*factor(educ) + black + hisp + re74 + re75 - 1, data = jtrain2)

regression_ds2 <- rlassoEffect(x = u2, y = jtrain2$re78, d = jtrain2$train, method = "double selection" )

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

# The coefficient falls from the original 1.79 to 1.59, and becomes significant not at the 1% but at the 5% level.
# This time, Lasso selects several variables that were previously omitted, because the richer set of controls allows
# for previously excluded nonlinearities and interactions.
# Further, it suggests that the reduction in the treatment effect estimated in (a) and (b1) did not fully account for relevant nonlinear functions of observables,
# which lowers the estimated treatment effect after inclusion.
# Moreover, the standard error increases relative to the naive OLS after selection (e.g. 0.656 vs 0.610),
# reflecting the fact that the double selection procedure is designed to deliver valid inference after model selection.
# In contrast, regular OLS treats the selected controls as fixed and ignores that they were chosen from the data,
# leading to overly optimistic (downward biased) standard errors.

# This helps validate that compared to the naive post-Lasso approach in part (a), which selects controls only based on their predictive power for the outcome,
# the double selection procedure helps ensure that variables relevant for treatment assignment are also included.
# This makes the estimator more robust to imperfect model selection.
# Concretely, the selection of several age and age-education interaction terms in the treatment equation indicates that treatment assignment,
# in this sample, is statistically related to specific demographic characteristics.
# These imbalances between treatment and control units could be considered to be due to chance as the underlying assignment was randomized.
# Failing to control for this does not invalidate causal identification but can make post-selection inference unreliable.

# Task 4

jtrain3_q4 <- jtrain3

# a)

## 1) Logit

### Model
mod_logit <- glm(train ~ age + educ + black + hisp + re74, family = binomial(link = "logit"), data = jtrain3_q4)
summary(mod_logit)

### Propensity score
jtrain3_q4$prop_logit <- predict(mod_logit, type = "response")

## 2) Random forest

### Setup variables
mod_rf_vars <- as.matrix(jtrain3_q4[ , c("age", "educ", "black", "hisp", "re74")])

### Model
mod_rf <- probability_forest(
  X = mod_rf_vars , Y = as.factor(jtrain3_q4$train),
  num.trees = 4000,
  seed= 88888
)

### Propensity score = Out-of-bag predicted probabilities
jtrain3_q4$prop_rf <- predict(mod_rf)$predictions[, 2]

## Summary stats

### Logit
summary(jtrain3_q4$prop_logit[jtrain3_q4$train == 1]) # Treated
summary(jtrain3_q4$prop_logit[jtrain3_q4$train == 0]) # Control

### RF
summary(jtrain3_q4$prop_rf[jtrain3_q4$train == 1]) # Treated
summary(jtrain3_q4$prop_rf[jtrain3_q4$train == 0]) # Control

## Overlap plot
jtrain3_q4 %>%
  mutate(group = factor(jtrain3_q4$train, labels = c("Control", "Treatment"))) %>%
  pivot_longer(c(prop_rf, prop_logit), names_to = c(".value", "model"), names_sep = "_") %>%
  ggplot(aes(x = prop, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(x = "P-score", y = "Density") +
  facet_wrap(~ model, scales = "free_x")

ggsave("out/PS1_Q4.png", width = 12)

jtrain3_q4 %>%
  mutate(group = factor(jtrain3_q4$train, labels = c("Control", "Treatment"))) %>%
  pivot_longer(c(prop_rf, prop_logit), names_to = c(".value", "model"), names_sep = "_") %>%
  mutate(prop = log(prop / (1- prop))) %>%
  ggplot(aes(x = prop, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(x = "P-score (log-odds)", y = "Density") +
  facet_wrap(~ model, scales = "free_x")

ggsave("out/PS1_Q4_logodds.png", width = 12)

## 3) Trimming

jtrain3_q4 <- jtrain3_q4 %>%
  mutate(across(c(prop_logit, prop_rf), ~ . <= 0.8, .names = "{sub('prop', 'trim', .col)}"))

### Trimmed obs. and implied cuttoff stats
jtrain3_q4 %>%
  pivot_longer(c(prop_rf, prop_logit, trim_rf, trim_logit), names_to = c(".value", "model"), names_sep = "_") %>%
  summarize(
    across(trim, list(`Kept obs.` = sum, `Dropped obs.` = ~ sum(1 - .)), .names = "{.fn}"),
    `Implied cutoff` = if_else(cur_group()$train != 0, NA, max(prop)),
    .by = c(model, train)
  ) %>%
  mutate(
    `Kept %` = `Kept obs.` / (`Kept obs.` + `Dropped obs.`) * 100,
    `Dropped %` = 100 - `Kept %`,
    .after = `Dropped obs.`
  ) %>%
  arrange(model, train)

### Coviariate means for treated kept & trimmed
jtrain3_q4 %>%
  pivot_longer(c(prop_rf, prop_logit, trim_rf, trim_logit), names_to = c(".value", "model"), names_sep = "_") %>%
    filter(train == 1) %>%
    summarize(
      n = n(),
      across(c(age, educ, black, hisp, re74), mean, .names = "Mean of {.col}"),
      .by = c(model, trim)
    ) %>%
  arrange(model, trim)

## 4) TABLE 3

### Regressions
regs_4 <- list(
  reg_78_full  = lm(re78 ~ train + age + educ + black + hisp + re74, data = jtrain3_q4),
  reg_78_logit = lm(re78 ~ train + age + educ + black + hisp + re74, data = jtrain3_q4[jtrain3_q4$trim_logit,]),
  reg_78_rf    = lm(re78 ~ train + age + educ + black + hisp + re74, data = jtrain3_q4[jtrain3_q4$trim_rf,]),
  reg_75_full  = lm(re75 ~ train + age + educ + black + hisp + re74, data = jtrain3_q4),
  reg_75_logit = lm(re75 ~ train + age + educ + black + hisp + re74, data = jtrain3_q4[jtrain3_q4$trim_logit,]),
  reg_75_rf    = lm(re75 ~ train + age + educ + black + hisp + re74, data = jtrain3_q4[jtrain3_q4$trim_rf,])
)

rows_4 <- get_group_counts(regs_4)

TABLE_3 <- modelsummary(
  regs_4,
  add_rows = rows_4,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "TABLE 3: Task 4"
)

TABLE_3

saveWorkbook(as_Workbook(TABLE_3), "out/PS1_Q4.xlsx", overwrite = TRUE)

## 5)

# Flexibility
# RF is more flexible because it captures nonlinearities and interactions automatically, while logit imposes a more restrictive functional form, linearity in log-odds,
# where transformations and interactions must be added manually.

# Tail behavior/calibration & trimming
# For this data, RF produces a heavier upper tail for the treated units: median is 0.71 for RF, 0.66 for logit, 3rd quartile is 0.83 vs 0.77, max is 0.95 vs 0.89.
# This results in more treated units exceeding the 0.8 threshold (59 (31.9%) trimmed treated observations for RF vs 37 (20%) for logit),
# so the trimmed sample depends on the choice of estimator.
# Overall, logit produces smoother propensity scores, given the restricted functional form, while RF scores can be more extreme and irregular.
# RF does assign a higher implied cutoff for controls, but the effect is isolated to a few cases.
# Overall, trimming for controls was barely affected (4 vs 2 dropped).
# For calibration, the key question is how similar predicted treatment probabilities are to the actual treatment frequencies.
# However, we do not test calibration in this exercise, so no strong conclusions can be made other than the heavier upper tail for RF.

# Interpretability/reproducibility
# Logit is easier to interpret: effects have a clear sign interpretation and a size interpretation on the log-odds scale.
# Likewise, reproducibility is exact with the same specification.
# As RF is a composition of many different trees, its inner workings are essentially a black box compared to logit.
# RF is still reproducible, given a seed and tuning parameters, but the process is less transparent.

# Overlap diagnostics & discarded observations
# Both logit and RF-based propensity scores imply weak overlap, with controls tending to have much lower propensity scores than treated units.
# With RF, treated units are assigned higher propensity scores where there are fewer controls, leading to more trimming.
# The discarded observations systematically differ from the untrimmed units.
# Discarded units are younger, less educated, more likely to be black, less likely to be hispanic and with lower '74 real earnings.
# There are also differences between RF and logit: RF trimmed units are slightly older, marginally more educated, all black, and with no '74 real earnings.
# So, the choice of estimator also affects the sample used for treatment effect estimation.
# As evidenced by TABLE3, the treatment coefficient for the RF trimmed sample is more negative (-1.69) than the full (-0.93) or logit-trimmed (-0.98) sample.

# b)

# Dehejia and Wahba argue that the use of propensity scores and matching to ensure overlap allowed them to obtain statistical estimates for
# the training effect that matched the experimental benchmark.
# However, this should not be seen as evidence that observational methods are able to recover causal effects.

# First, Imbens and Xu showed that with a different control group, the observational estimates no longer matched the experimental results, and,
# going deeper into conditional average treatment effects, the discrepancy between observational and experimental estimates increased significantly.
# So, the Dehejia Wahba results are not representative of the general performance of nonexperimental methods.

# Second, while Imbens and Xu concur that modern methods and better overlap can make observational estimates more stable/robust
# (similar estimated covariate-adjusted treated-control differences), a causal interpretation still requires the fundamentally untestable unconfoundedness assumption.
# However, the plausibility of unconfoundedness can be examined via placebo tests.
# In this case, the placebo estimates generally did not support unconfoundedness, as the estimated treatment effects on pre-treatment outcomes
# were significant even for modern estimation methods and after improving overlap.

# Our results agree with this assessment.
# All three specifications give a negative estimated coefficient on re78 (-0.93 in the full sample, -0.98 after logit trimming,
# and -1.69 after RF trimming) and none were statistically significant.
# Moreover, the treatment estimate is sensitive to the model used for propensity score estimation: RF trims 59 treated observations,
# logit trims 37, and the estimated re78 coefficient becomes substantially more negative under RF trimming.
# Furthermore, the placebo regressions for re75 are also negative and statistically significant (-2.0 to -2.4).
# These results argue against a causal interpretation: treatment by construction cannot affect past earnings,
# so the placebo result indicates remaining selection bias / unobserved differences between treated and controls.
# Trimming improves overlap, but it does not solve failure of unconfoundedness.

# Task 5

# a)

# For the unbiasedness of the Neyman difference-in-means estimator, the key condition is that, in a completely randomized experiment,
# treatment assignment is independent from the potential outcomes D⊥(Y(0),Y(1)).
# This follows from randomization and ensures that, on average, treated and control groups are comparable.
# The Neyman framework assumes that randomness comes only from the assignment of treatment and control groups.
# Thus, if an experiment were repeated, the assignment would change, while the individuals’ potential outcomes would remain fixed.
# Additionally, SUTVA must hold, requiring no interference between units whereby the potential outcome for any unit does not vary
# with the treatments assigned to other units, and also no hidden treatments.
# Under these conditions, the treatment and control groups are balanced in expectation in terms of potential outcomes over repeated assignments,
# so the difference in sample means provides an unbiased estimator of the sample average treatment effect.

# On the unbiasedness of the Neyman variance estimator, the true variance of the difference-in-means estimator in the Neyman framework is:
# V(τ_hat) = S1^2 / N_t + S0^2 / N_c - S_tau^2 / N
#
# where:
# S1^2    = variance of Y_i(1)
# S0^2    = variance of Y_i(0)
# S_tau^2 = variance of individual treatment effects (Y_i(1) - Y_i(0))

# We can say that, because we never observe both potential outcomes Y_i(0) and Y_i(1) for the same unit,
# the variance of individual treatment effects Yi(1)−Yi(0) (S_tau^2) cannot be estimated when we have heterogeneous treatment effects.
# As a result, the Neyman variance estimator omits this term, leading to a conservative (weakly upward-biased) estimate of the variance.
# The issue arises because, in the finite-population framework, we never observe both potential outcomes for the same individuals and therefore
# cannot identify the variance of individual treatment effects.
# So, heterogeneity in individual treatment effects appears as missing information.

# In contrast, under a superpopulation framework, the sample is viewed as randomly drawn from a larger population.
# So, the becomes the sampling variation across draws from the population becomes an additional source of uncertainty.
# Focus shifts towards the population ATE instead of the sample ATE.
# Under the superpopulation interpretation, the usual Neyman variance estimator can be unbiased for the variance of the PATE estimator.
# As Athey and Imbens put it "If we view the sample as randomly drawn from an infinite population,
# then the variance estimator is unbiased when τ_hat is used to estimate the population average treatment effect
# E[Y_i(1) - Y_i(0)], rather than the average treatment effect for the specific sample, (1/N) * sum_i (Y_i(1) - Y_i(0))."

# Changing from a finite sample to a super-population perspective shifts uncertainty from missing information to sampling variation,
# which in turn changes how we interpret the variance estimator.
# Since the variance of τ_hat determines standard errors and confidence intervals,
# correctly specifying it is crucial for valid inference, even if the estimator of the ATE itself is unbiased.

# b)

# Describe Fisher's inference
# Per Heß(2017), "Fisherian randomization inference produces the distribution of a test statistic under a designated null hypothesis,
# allowing a researcher to assess whether the actually observed realization of the statistic is “extreme”
# and hence whether the null hypothesis has to be rejected".
# One of the most common statistics to test is the coefficient estimate, which we found to be 1.794 in task 1.
# It is obtained by comparing the mean value of the outcome of interest across groups.
# The null hypothesis consists in a sharp hypothesis of a zero treatment effect, ie:

# y_i (D = 1) = y_i (D = 0) for all i = 1, 2, ...

# Note that it's different from testing a null average treatment effect as the sharp null is stronger:
# it requires every individual's treatment effect to be zero, i.e. Y_i(0) = Y_i(1) for all i,
# rather than just requiring individual effects to cancel out on average.
# Rejecting it means concluding that at least one unit's outcome was affected by treatment.
# In particular, to obtain this distribution, we ought to compute the same test statistic for each possible permutation for the treatment vector.
# If the sharp null were true for every unit, then relabeling who got treated should not matter;
# otherwise, the observed statistic should occupy an extreme position in the null distribution.


# Recreate p-value in Athey & Imbens (4.1)

# 1) Using difference in means statistic

# To get the difference-in-means estimate, we could equivalently do
# y <- regression <- lm(re78 ~ train, data = jtrain2))
# alpha <- coefficients(regression)["train"]

obs_effect <- mean(jtrain2$re78[jtrain2$train == 1]) -
  mean(jtrain2$re78[jtrain2$train == 0])
print(paste("Observed Treatment Effect:", obs_effect))


# Permutation distribution
set.seed(88888)
null_dist <- replicate(100000, {
  perm <- sample(jtrain2$train)
  mean(jtrain2$re78[perm == 1]) - mean(jtrain2$re78[perm == 0])
})

# Two-sided p-value
p_value <- mean(abs(null_dist) >= abs(obs_effect))
print(paste("Randomization Inference p-value (diff in means):", round(p_value, 6)))

# Athey & Imbens present a p-value of 0.0044, which is similar to our result of 0.00407.
# The slight difference is not necessarily problematic, and may have arise due to variation from randomly sampled assignments.
# Increasing the number of permutations even more would be likely to bring the results closer.
# Mechanically, there is also dependence on the choice of the seed.


# 2) Using difference in ranks statistic

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
observed_effect <- mean(y_hat[jtrain2$train == 1]) - mean(y_hat[jtrain2$train == 0])

# new perm distro
set.seed(88888)
null_distro <- replicate(100000, {
  permy <- sample(jtrain2$train)
  mean(y_hat[permy == 1]) - mean(y_hat[permy == 0])
})

# two-sided p-value
new_p_value <- mean(abs(null_distro) >= abs(observed_effect))
print(paste("Randomization Inference p-value (diff in ranks):", round(new_p_value, 6)))

# The rank statistic compresses outliers and extreme values, which, as established in task 2, are relevant in our data.
# Given the relevant paper's result of 0.01, our p-value of 0.01132 is slightly different.
# In our case, both results fall below the 5% significance level, allowing us to determine that the slight difference does not influence the conclusion.

# c)

# Fisherian randomization requires that treatment reshuffling must be conducted only in ways that were possible in the real experiment.
# Looking at the LaLonde paper, we can see that the experiment was conducted across multiple sites and target groups
# (10 locations, AFDC women, ex-drug addicts, ex-offenders, and young dropouts).
# Furthermore the paper notes that treatment-control ratios differed across groups.
# LaLonde explicitly mentions, for example, that for "the young high school target group there were by design more controls than treatments".
# This suggests that the original assignment may not have necessarily been equivalent to a single completely randomized draw over all individuals.

# Athey and Imbens perform their test by re-assigning treatment while simply keeping the total number of treated and control units fixed
# (185 and 260, respectively).
# Despite noting, "In order to calculate the exact p-value we need to know the exact distribution of the assignment vector".
# More concretely, looking at their own definitions, they treat the experiment as if coming from a completely
# randomized experiment rather than a design with site- and group-specific assignment.
# This assumes every unit in the dataset had an equal probability of being assigned to treatment ignoring possible constraints from the original design.
# If assignment was conducted within sites or groups, a correct Fisherian test should restrict reshuffling within those strata.

# Further there is the potential issue of attrition.
# Athey and Imbens apply randomization inference to a restricted subsample and not the full originally randomized sample.
# More concretely, Athey and Imbens permute 185 treated labels and 260 control labels among a pool of individuals.
# However, the original dataset from LaLonde consisted of two randomly allocated groups: 297 treated and 425 controls.
# Additionally, since the Fisher test relies on reshuffling treatment according to the known assignment mechanism, this implies that, under attrition,
# the assignment rule may no longer correctly describe the observed sample.
# More specifically, the way treatment is distributed in the observed data is no longer the result of the original randomization alone,
# but also of a (self-)selection process.
# As a result, exchangeability between treated and control units may no longer hold and
# the randomization distribution used for Fisherian inference may no longer reflect the randomization plan of the original experiment.

# Concretely, attrition may introduce selection that depends on treatment or outcomes.
# LaLonde documents that "many participants failed to complete these interviews, and this sample attrition potentially biases the experimental results."
# Moreover, response rates differ between treatment and control groups (e.g., 72% vs 68%), indicating that attrition may not be purely random.
# For example, if high-outcome treated individuals are more likely to remain in the sample, then the observed treatment effect could be inflated.
# As a result, the reshuffled assignments are unlikely to generate differences as large as the observed one.
# This may lead to a smaller p-value and possible over-rejection of the null hypothesis.
# More generally, however, the sign of the distortion is ambiguous, depending on how attrition relates to treatment assignment and outcomes.

# d)

## 1)

# Both HC1 and HC3 are heteroskedasticity-robust estimators for the OLS variance-covariance matrix.
# Both build on White's estimator (HC0), HC1 makes an overall degrees-of-freedom adjustment (n/(n-k)) to the whole estimator,
# while HC3 adjusts each squared residual by the squared unity complement of the respective observation's leverage (1/(1-lev)^2).
# So, HC3 increases the residual contribution of influential observations and is usually more conservative.
# HC3 often gives larger standard errors than HC1, especially so in smaller samples.


## 2)

### First regressions
TABLE_2_5d2_1c <- modelsummary(
  regs,
  add_rows = rows,
  vcov = "HC3",
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "TABLE 2: Task 1c (HC3)"
)

TABLE_2_5d2_1c

saveWorkbook(as_Workbook(TABLE_2_5d2_1c), "out/PS1_Q5d2_1c.xlsx", overwrite = TRUE)

# redoing reg_3 with dfbeta

TABLE_2_5d2_1d <- modelsummary(
  regs_dfb,
  add_rows = rows_dfb,
  vcov = "HC3",
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "TABLE 2: Task 1d (HC3)"
)

TABLE_2_5d2_1d

saveWorkbook(as_Workbook(TABLE_2_5d2_1d), "out/PS1_Q5d2_1d.xlsx", overwrite = TRUE)

## 3)

# Bootstrap standard errors are obtained by repeatedly resampling the data with replacement, re-estimating the regression in each resample,
# and then computing the standard deviation of the estimated coefficients.
# So, bootstrap approximates the sampling distribution of the estimator directly from repeated resamples.
# It is easier to do with `sandwich`, the type "xy" is equivalent to calculating SEs from resampled and repeated regressions.
vcovBS_adj <- \(...) vcovBS(R = 2000, type = "xy", ...)

set.seed(88888)
TABLE_2_5d3_1c <- modelsummary(
  regs,
  add_rows = rows,
  vcov = vcovBS_adj,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "TABLE 2: Task 1c (bootstrap)"
)

TABLE_2_5d3_1c

saveWorkbook(as_Workbook(TABLE_2_5d3_1c), "out/PS1_Q5d3_1c.xlsx", overwrite = TRUE)

set.seed(88888)
TABLE_2_5d3_1d <- modelsummary(
  regs_dfb,
  add_rows = rows_dfb,
  vcov = vcovBS_adj,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "TABLE 2: Task 1d (bootstrap)"
)

TABLE_2_5d3_1d

saveWorkbook(as_Workbook(TABLE_2_5d3_1d), "out/PS1_Q5d3_1d.xlsx", overwrite = TRUE)

## 4)

# Different standard errors do not qualitatively change our results.
# Across the default, HC3, and bootstrap standard errors, the estimated effect of train remains positive and statistically significant.
# What changes is mainly the estimated precision, but HC3 does not change the conclusions much here because the correction relative to conventional OLS SEs is modest.
# It increases the standard errors somewhat, but not enough to overturn inference.
# Likewise, bootstrapped SEs are inbetween the default and HC3 ones.

# The main point of the Data Colada post is that HC1 can perform poorly relative to HC3, especially in smaller samples.
# But this is not our case exactly, because the default lm() standard errors in R are the usual OLS standard errors, not HC1.
# In addition, our sample has 445 observations, which is well above the small-sample range emphasized in the post (250 obs.).
# HC3 can still matter when there are influential or high-leverage observations, because it penalizes those observations more strongly.
# And the existance of influential observations is something we see in the dfbeta exercise.
# So, HC3 could be expected to increase the standard errors at least somewhat, and that is exactly what we find:
# the estimated effect of train remains positive and statistically significant, and only the precision changes slightly.
