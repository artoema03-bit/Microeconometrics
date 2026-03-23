library(sandwich)
library(lmtest)

# Set wd
user <- Sys.info()["user"]
output_dir <- switch(user,
  "ajnik"="G:/Mans disks/zObsidian/04 Courses/20295 Microeconometrics/Problem Sets/microeconometrics-ps",
  getwd()
)
setwd(output_dir)

# Common setup
source("./setup.R")

# d)

## 1)

# Both HC1 and HC3 are heteroskedasticity-robust estimators for the OLS variance-covariance matrix.
# Both build on White's estimator (HC0), HC1 makes an overall degrees-of-freedom adjustment (n/(n-k)) to the whole estimator,
# while HC3 adjusts each squared residual by the squared unity complement of the respective observation's leverage (1/(1-lev)^2).
# So, HC3 gives more weight to influential observations and is usually more conservative.
# HC3 often gives larger standard errors than HC1, especially so in smaller samples.


## 2)

### First regressions
regs <- list(
  regression_1 = lm(re78 ~ train, data = jtrain2),
  regression_2 = lm(re78  ~ train + age + educ + black + hisp, data = jtrain2),
  regression_3 = lm(re78  ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2)
)

rows <- data.frame(
  "term" = c("N Treated ", "N Control "),
  "Model 1" = get_group_counts(regs[[1]]),
  "Model 2" = get_group_counts(regs[[2]]),
  "Model 3" = get_group_counts(regs[[3]])
)

modelsummary(
  regs,
  add_rows = rows,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1c (no adjustment)"
)

modelsummary(
  regs,
  add_rows = rows,
  vcov = "HC3",
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1c (HC3)"
)

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

rows_dfb <- data.frame(
  "term" = c("N Treated ", "N Control "),
  "Model 1" = get_group_counts(regs_dfb[[1]]),
  "Model 2" = get_group_counts(regs_dfb[[2]]),
  "Model 3" = get_group_counts(regs_dfb[[3]]),
  "Model 4" = get_group_counts(regs_dfb[[4]])
)

modelsummary(
  regs_dfb,
  add_rows = rows_dfb,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1d (no adjustment)"
)

modelsummary(
  regs_dfb,
  add_rows = rows_dfb,
  vcov = "HC3",
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1d (HC3)"
)

## 3)

# Bootstrap standard errors are obtained by repeatedly resampling the data with replacement, re-estimating the regression in each resample, and then computing the standard deviation of the estimated coefficients.
# So, bootstrap approximates the sampling distribution of the estimator directly from repeated resamples.
# It is easier to do with `sandwich`, the type "xy" is equivalent to calculating SEs from resampled and repeated regressions.
vcovBS_adj <- \(...) vcovBS(R = 2000, type = "xy", ...)

set.seed(88888)
modelsummary(
  regs,
  add_rows = rows,
  vcov = vcovBS_adj,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1c (bootstrap)"
)

set.seed(88888)
modelsummary(
  regs_dfb,
  add_rows = rows_dfb,
  vcov = vcovBS_adj,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1d (bootstrap)"
)

## 4)

# Different standard errors do not qualitatively change our results.
# Across the default, HC3, and bootstrap standard errors, the estimated effect of train remains positive and statistically significant.
# What changes is mainly the estimated precision, but HC3 does not change the conclusions much here because the correction relative to raw standard errors is modest.
# It increases the standard errors somewhat, but not enough to overturn inference.
# Likewise, bootstrapped SEs are inbetween the default and HC3 ones.

# The main point of the Data Colada post is that HC1 can perform poorly relative to HC3, especially in smaller samples.
# But this is not our case exactly, because the default lm() standard errors in R are the usual OLS standard errors, not HC1.
# In addition, our sample has 445 observations, which is well above the small-sample range emphasized in the post (250 obs.).
# HC3 can still matter when there are influential or high-leverage observations, because it penalizes those observations more strongly.
# And the existance of influential observations is something we see in the dfbeta exercise.
# So, HC3 could be expected to increase the standard errors at least somewhat, and that is exactly what we find: the estimated effect of train remains positive and statistically significant, and only the precision changes slightly.
