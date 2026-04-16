library(grf)

# Set wd
user <- Sys.info()["user"]
output_dir <- switch(user,
  "ajnik"="G:/Mans disks/zObsidian/04 Courses/20295 Microeconometrics/Problem Sets/microeconometrics-ps",
  "erick"="/home/erick/TEMP/",
  getwd()
)
setwd(output_dir)

# Common setup
source("./setup.R")

# Load data

data_base <- read_delim("files/expanded_data.csv") %>%
  mutate(st = factor(st), urbanization = if_else(urbanization == "Urban", 1, 0))

# a

data_exp <- data_base %>%
  filter(between(lfdivlaw, 1969, 1973) | lfdivlaw == 2000) %>%
  mutate(treated = if_else(lfdivlaw == 2000, 0, 1)) %>%
  pivot_wider(names_from = year, values_from = -c(treated, st, county_id, year, lfdivlaw)) %>%
  mutate(outcome = div_rate_sim_1978 - div_rate_sim_1968)

data_exp_covar <- data_exp %>%
  select(-outcome, -treated, -county_id, -st, -starts_with("lfdivlaw"), -starts_with("div_rate_sim"), -ends_with("_1978")) %>%
  model.matrix(~ . - 1, data = .)

mod_base <- causal_forest(data_exp_covar, Y = data_exp$outcome, W = data_exp$treated, num.trees = 10000, seed = 88888, clusters = data_exp$st)
mod_base

average_treatment_effect(mod_base)

# For the causal forest estimation, we use the 1968–1978 difference in county divorce rates as outcomes, conditioned on 1968 controls.
# The estimation is also clustered at the state level, as that is the treatment assignment level.
# The causal forest estimates an average treatment effect of about 0.034 with standard error 0.068,
# so the average effect is small and not statistically different from zero.
# This is consistent with our conclusion for 1.c.
# Therefore, on average, unilateral divorce laws do not appear to have a clear effect on divorce rates in this restricted sample.
# However, a near-zero ATE does not rule out TE heterogeneity as it may have arisen due to positive and negative effects offsetting each other in the average.

# b

## Variable importance
diagn_imp <- tibble(
  variable = colnames(data_exp_covar),
  importance = as.double(variable_importance(mod_base))
) %>%
  arrange(desc(importance))
diagn_imp


# The most important variable in terms of splitting frequency is religious adherence, so heterogeneity is strongly associated with it.
# Women labor force participation is the second most important variable, followed by education rate and domestic violence rate.
# Urbanization, however, has almost zero importance, so it contributes very little to the forest's treatment effect heterogeneity.
# Anyways, variable importance is only a heuristic and should not be interpreted as a causal ranking of covariates.

## Best linear projection
diagn_blp <- best_linear_projection(mod_base, data_exp_covar)
diagn_blp

# It shows which observables

# The best linear projection is a doubly robust linear model of the estimated CATEs on covariates.
# As a summary, it descibes which observables are associated with more or less positive predicted treatment effects.
# The BLP also shows that heterogeneity is mainly related to religious adherence, women's labor force participation, and, more weakly, domestic violence.
# The coefficient on religious_adherence_1968 is negative and highly significant:
# counties with higher religious adherence tend to have less positive (more negative) estimated treatment effects.
# The coefficient on women_labor_force_participation_1968 is positive and significant:
# counties with higher female labor force participation tend to have more positive estimated treatment effects.
# The coefficient on domestic_violence_rate_1968 is positive and only weakly significant, its role is less precisely estimated.
# Other covariates are not statistically significant, so there is little evidence that they are linearly related to the CATE.

## TOC
set.seed(88888)
state_assign <- data_exp %>%
  distinct(st, treated)

# stratified state-level split so both treated and control states appear in both halves
train_states <- state_assign %>%
  group_split(treated) %>%
  lapply(function(df) sample(df$st, ceiling(nrow(df) / 2))) %>%
  unlist() %>%
  unname()

idx_train <- data_exp$st %in% train_states
idx_eval  <- !idx_train

mod_toc_train <- causal_forest(
  X = data_exp_covar[idx_train, ],
  Y = data_exp$outcome[idx_train],
  W = data_exp$treated[idx_train],
  clusters = data_exp$st[idx_train],
  num.trees = 10000,
  seed = 88888
)

mod_toc_eval <- causal_forest(
  X = data_exp_covar[idx_eval, ],
  Y = data_exp$outcome[idx_eval],
  W = data_exp$treated[idx_eval],
  clusters = data_exp$st[idx_eval],
  num.trees = 10000,
  seed = 88888
)

diagn_rate <- rank_average_treatment_effect(
  mod_toc_eval,
  predict(mod_toc_train, newdata = data_exp_covar[idx_eval, ])$predictions
)

paste("AUTOC:", round(diagn_rate$estimate, 2), "±", round(1.96 * diagn_rate$std.err, 2))

plot(diagn_rate)

png("out/Q2_b_1.png")
plot(diagn_rate)
dev.off()


# The TOC checks whether the causal forest is actually good at ranking units by treatment effect.
# For a given fraction of treated units, it compares the ATE among the top-ranked units to the overall ATE.
# The AUTOC is the area under this curve: a positive and statistically significant AUTOC indicates the ranking contains useful heterogeneity information.

# The TOC is evaluated out of sample, splitting observations at the state-level as treatment is assigned at the state level.
# The TOC curve is mostly above zero for low and intermediate treated fractions, which means that the observations ranked by the forest as having high
# treatment effects do tend to have higher-than-average effects.
# The AUTOC is about 0.28 with a fairly wide confidence interval (+/- 0.29), so this evidence is rather suggestive than conclusive.
# Overall, the TOC supports the idea that the forest captures some real heterogeneity and is able to rank observations by treatment effect better than chance.

## CATEs
mod_cate <- predict(mod_base)$predictions

hist(mod_cate)

png("out/Q2_b_2.png")
hist(mod_cate)
dev.off()

# The histogram of estimated CATEs shows substantial dispersion around zero, suggesting that treatment effects are not constant across observations.
# There is a cluster of observations with negative predicted effects and a cluster with positive ones, consistent with the near-zero ATE hiding heterogeneity.

data_exp_covar %>%
  as_tibble() %>%
  mutate(cate = mod_cate) %>%
  mutate(`Religious adherence` = if_else(religious_adherence_1968 < 50, "Low", "High")) %>%
  ggplot(aes(x = cate, fill = `Religious adherence`)) + geom_density(alpha = 0.3)

ggsave("out/Q2_b_3.png")

data_exp_covar %>%
  as_tibble() %>%
  mutate(cate = mod_cate) %>%
  mutate(group = if_else(religious_adherence_1968 < 50, "Low", "High")) %>%
  pivot_longer(-c(cate, group), names_to = "variable", values_to = "value") %>%
  left_join(diagn_imp, by = "variable") %>%
  mutate(variable = fct_reorder(variable, importance, .desc = TRUE)) %>%
  filter(importance >= 0.04) %>%
  ggplot(aes(x = value, y = cate, )) +
  geom_point(aes(color = group)) +
  geom_smooth(aes(group = group)) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(color = "Religious\nadherence")

ggsave("out/Q2_b_4.png")

# We plot the estimated CATEs with respect to the 8 most important covariates.
# The CATE plots are consistent with the other diagnostics and suggests that the clearest source of treatment effect heterogeneity is religious_adherence_1968.
# There is a non-linear relation between CATEs and religious adherence:
# counties with low religious adherence tend to have positive estimated treatment effects,
# while counties with high religious adherence tend to have negative or near-zero estimated treatment effects.
# This pattern is much stronger than for the other covariates and is consistent with both the variable-importance ranking and the BLP results.
# So, for ease of vizualization, we split the sample into two groups: counties with religious adherence below and above 50.
# Women's labor force participation and domestic violence also show some positive, but weaker, association with the CATE.
# Other variables appear to play almost no role.

# c

# As a disclaimer, because treatment varies at the state level while the causal forest is estimated on county-level observations,
# the source of independent treatment variation is the state, not the county.
# So, the county-level heterogeneity patterns should be interpreted cautiously.

# Our results provide some suggestive evidence of heterogeneous treatment effects, even though the overall ATE is close to zero.
# First, the BLP finds significant relationships between the CATE and some pre-treatment covariates, especially religious adherence and women's labor force participation.
# Second, the TOC/AUTOC is positive, suggesting that the forest's ranking of units by predicted treatment effect contains useful information.
# Third, the CATE plots show a clear contrast between low- and high-religious-adherence counties.
# Therefore, the average effect is small not because treatment has no effect for everyone,
# but because positive and negative effects appear to offset each other across different counties.

# d

mod_dishonest <- causal_forest(data_exp_covar, Y = data_exp$outcome, W = data_exp$treated, num.trees = 10000, seed = 88888, honesty = FALSE, clusters = data_exp$st)
mod_dishonest

average_treatment_effect(mod_dishonest)

diagn_dishonest_imp <- tibble(
  variable = colnames(data_exp_covar),
  importance = as.double(variable_importance(mod_dishonest))
) %>%
  arrange(desc(importance))
diagn_dishonest_imp

mod_dishonest_cate <- predict(mod_dishonest)$predictions

hist(mod_dishonest_cate)

png("out/Q2_d_1.png")
hist(mod_dishonest_cate)
dev.off()

data_exp_covar %>%
  as_tibble() %>%
  mutate(cate = mod_dishonest_cate) %>%
  mutate(group = if_else(religious_adherence_1968 < 50, "Low", "High")) %>%
  pivot_longer(-c(cate, group), names_to = "variable", values_to = "value") %>%
  left_join(diagn_dishonest_imp, by = "variable") %>%
  mutate(variable = fct_reorder(variable, importance, .desc = TRUE)) %>%
  ggplot(aes(x = value, y = cate, )) +
  geom_point(aes(color = group)) +
  geom_smooth(aes(group = group)) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  labs(color = "Religious\nadherence")

ggsave("out/Q2_d_2.png")

# Honest trees use one part of the sample to construct the tree by choosing splits and another part to estimate treatment effects within terminal leaves.
# This reduces overfitting, makes CATE estimation more credible and allows for asymptotic inference.
# In our results, disabling honesty barely changes the estimated ATE: the honest forest gives about 0.0343 and the dishonest forest about 0.0339.
# So, honesty is not very important for the average treatment effect in this sample, but it does have an effect on treatment effect heterogeneity.
# Without honesty, variable importance becomes even more concentrated on religious adherence
# and the distribution of predicted CATEs becomes much more spread out.
# This suggests that the dishonest forest is fitting stronger and potentially noisier heterogeneity patterns.
# Therefore, honest forests are more thrustworthy for interpreting CATEs, even though both approaches give almost the same ATE.

# One could expect the difference between honest and dishonest forests to be larger in smaller samples, with more varying outcomes, weaker overlap, deeper trees, or many relatively covariates.
# In these cases, using the same data both for model construction and effect estimation raises the risk of overfitting and a biased ATE.
