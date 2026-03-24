library(grf)

# SETUP
## Set wd
user <- Sys.info()["user"]
output_dir <- switch(user,
  "ajnik"="G:/Mans disks/zObsidian/04 Courses/20295 Microeconometrics/Problem Sets/microeconometrics-ps",
  getwd()
)
setwd(output_dir)

## Common setup
source("./setup.R")

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

jtrain3_q4 %>%
  mutate(group = factor(jtrain3_q4$train, labels = c("Control", "Treatment"))) %>%
  pivot_longer(c(prop_rf, prop_logit), names_to = c(".value", "model"), names_sep = "_") %>%
  mutate(prop = log(prop / (1- prop))) %>%
  ggplot(aes(x = prop, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(x = "P-score (log-odds)", y = "Density") +
  facet_wrap(~ model, scales = "free_x")

## 3) Trimming

jtrain3_q4 <- jtrain3_q4 %>%
  mutate(across(c(prop_logit, prop_rf), ~ . <= 0.8, .names = "{sub('prop', 'trim', .col)}"))

### Trimmed obs. and implied cuttoff stats
jtrain3_q4 %>%
  pivot_longer(c(prop_rf, prop_logit, trim_rf, trim_logit), names_to = c(".value", "model"), names_sep = "_") %>%
  summarize(
    across(trim, list(`Kept obs.` = sum, `Dropped obs.` = ~ sum(1 - .)), .names = "{.fn}"),
    `Implied cutoff` = max(prop),
    .by = c(model, train)
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

TABLE_3_4 <- modelsummary(
  regs_4,
  add_rows = rows_4,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01)
)

TABLE_3_4

## 5)

# Flexibility
# RF is more flexible because it captures nonlinearities and interactions automatically, while logit imposes a more restrictive functional form, linearity in log-odds, where transformations and interactions must be added manually.

# Tail behavior & trimming
# For this data, RF produces a heavier upper tail for the treated units: median is 0.71 for RF, 0.66 for logit, 3rd quartile is 0.83 vs 0.77, max is 0.95 vs 0.89.
# This results in more treated units exceeding the 0.8 threshold (59 trimmed treated observations for RF vs 37 for logit), so the trimmed sample depends on the choice of estimator.
# Overall, logit produces smoother propensity scores, given that it builds on a logistic distribution, while RF scores can be more extreme and irregular.
# RF does assign a higher implied cutoff for controls, but the effect is isolated to a few cases.
# Overall, trimming for controls was barely affected (4 vs 2 dropped).

# Interpretability/reproducibility
# Logit is easier to interpret: effects have a clear sign interpretation and a size interpretation on the log-odds scale.
# Likewise, reproducibility is exact with the same specification.
# As RF is a composition of many different trees, its inner workings are essentially a black box compared to logit.
# RF is still reproducible, given a seed and tuning parameters, but the process is less transparent.

# Overlap diagnostics & discarded observations
# Both logit and RF-based propensity scores imply weak overlap, with controls tending to have much lower propensity scores than treated units.
# RF could be said to worsen overlap, as treated units are assigned higher propensity scores where there is fewer controls, leading to more trimming.
# The discarded observations systematically differ from the untrimmed units.
# Discarded units are younger, less educated, more likely to be black, less likely to be hispanic and with lower '74 real earnings.
# There are also differences between RF and logit: RF trimmed units are slightly older, marginally more educated, all black, and with no '74 real earnings.
# So, the choice of estimator also affects the sample used for treatment effect estimation.
# As evidenced by TABLE3, the treatment coefficient for the RF trimmed sample is more negative (-1.69) than the full (-0.93) or logit-trimmed (-0.98) sample.

# b)

# Dehejia and Wahba argue that the use of propensity scores and matching to ensure overlap allowed them to obtain statistical estimates for the training effect that matched the experimental benchmark.
# However, this should not be seen as evidence that observational methods are able to recover causal effects.

# First, Imbens and Xu showed that with a different control group, the observational estimates no longer matched the experimental results, and, going deeper into conditional average treatment effects, the discrepancy between observational and experimental estimates increased significantly.
# So, the Dehejia Wahba results are not representative of the general performance of nonexperimental methods.

# Second, while Imbens and Xu concur that modern methods and better overlap can make observational estimates more stable/robust (similar estimated covariate-adjusted treated-control differences), a causal interpretation still requires the fundamentally untestable unconfoundedness assumption.
# However, the plausibility of unconfoundedness can be examined via placebo tests.
# In this case, the placebo estimates generally did not support unconfoundedness, as the estimated treatment effects on pre-treatment outcomes were significant even for modern estimation methods and after improving overlap.

# Our results agree with this assessment.
# All three specifications give a negative estimated coefficient on re78 (-0.93 in the full sample, -0.98 after logit trimming, and -1.69 after RF trimming) and none were statistically significant.
# Moreover, the treatment estimate is sensitive to the model used for propensity score estimation: RF trims 59 treated observations, logit trims 37, and the estimated re78 coefficient becomes substantially more negative under RF trimming.
# Furthermore, the placebo regressions for re75 are also negative and statistically significant (-2.0 to -2.4).
# These results argue against a causal interpretation: treatment by construction cannot affect past earnings, so the placebo result indicates remaining selection bias / unobserved differences between treated and controls.
# Trimming improves overlap, but it does not solve failure of unconfoundedness.
