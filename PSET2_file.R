# R script for Problem Set 2
# Group composition: Emanuele Artoni (3199617), Pedro Cassandra (3387647), and Arturs Janis Nikitins (3342806)

# Set wd
user <- Sys.info()["user"]
output_dir <- switch(
  user,
  "ajnik" = "G:/Mans disks/zObsidian/04 Courses/20295 Microeconometrics/Problem Sets/microeconometrics-ps",
  "erick" = "/home/erick/TEMP/",
  getwd()
)
setwd(output_dir)

library(tidyverse)
library(writexl)
library(fixest)

data <- read.csv("files/pset_2.csv", sep = ";", )

# summary(data)

#Exercise 1

# (a)
# We must use analytic weights
# We do so because the dependent variable (divorce rate per 1,000 people) is a mean
# computed from state-level populations (stpop) of different sizes.

# Larger states may provide estimates with lower variance, so weighting
# by population accounts for heteroskedasticity and ensures that estimates
# are representative of the U.S. population.
# Likewise, when aggregating divorce rates for vizualizations, analytic weights
# allow to construct population-weighted average divorce rates.

# Frequency weights are not relevant, since the divorce rates are state-year specific.
# Using them would imply div_rate would be observed as if each observation were
# repeated stpop times.

# Probability weights are necessary when when data come from a random
# sample rather than the full population, this is not relevant in this scenario.

# (b)
# (i)

data_1 <- data %>%
  mutate(
    TREATED = case_when(
      lfdivlaw >= 1968 & lfdivlaw <= 1988 ~ 1,
      lfdivlaw > 1988 | lfdivlaw == 2000 ~ 0,
      TRUE ~ NA
    )
  ) %>%
  filter(!is.na(TREATED))

data_1 <- data_1 %>%
  group_by(year, TREATED) %>%
  summarise(
    Y = weighted.mean(div_rate, w = stpop, na.rm = TRUE),
    .groups = "drop"
  )

data_differences_1 <- data_1 %>%
  pivot_wider(names_from = TREATED, values_from = Y) %>%
  mutate(differences = `1` - `0`)

ggplot(data_1, aes(x = year, y = Y, color = factor(TREATED))) +
  geom_line() +
  geom_line(
    data = data_differences_1,
    aes(x = year, y = differences, color = "Difference"),
    inherit.aes = FALSE
  ) +
  geom_vline(xintercept = c(1968, 1988), linetype = "dashed") +
  scale_color_manual(
    name = "Series",
    values = c("0" = "red", "1" = "blue", "Difference" = "black"),
    labels = c(
      "0" = "Control Group",
      "1" = "Treated Group",
      "Difference" = "Difference (Treated - Control)"
    )
  ) +
  labs(y = "Divorce Rate per 1000 People", title = "Outcome Trends") +
  theme_minimal()

ggsave("out/Q1_b_1.png")

# States that adopted the law prior to the 1968-1988 period were removed from the
# sample as they were "always treated", making them unviable for comparison as
# members of the control group.

# (ii)

data_2 <- data %>%
  filter(
    (lfdivlaw == 2000 | (lfdivlaw >= 1969 & lfdivlaw <= 1973)) & year <= 1978
  ) %>%
  mutate(
    year = as.numeric(year),
    TREATED = as.numeric(lfdivlaw >= 1969 & lfdivlaw <= 1973)
  ) %>%
  group_by(year, TREATED) %>%
  summarise(
    Y = weighted.mean(div_rate, w = stpop, na.rm = TRUE),
    .groups = "drop"
  )

data_differences_2 <- data_2 %>%
  pivot_wider(names_from = TREATED, values_from = Y) %>%
  mutate(differences = `1` - `0`)


ggplot(data_2, aes(x = year, y = Y, color = factor(TREATED))) +
  geom_line() +
  geom_line(
    data = data_differences_2,
    aes(x = year, y = differences, color = "Difference"),
    inherit.aes = FALSE
  ) +
  geom_vline(xintercept = c(1968.5), linetype = "dashed") +
  scale_color_manual(
    name = "Series",
    values = c("0" = "red", "1" = "blue", "Difference" = "black"),
    labels = c(
      "0" = "Control Group",
      "1" = "Treated Group",
      "Difference" = "Difference (Treated - Control)"
    )
  ) +
  labs(y = "Divorce Rate per 1000 People", title = "Outcome Trends") +
  theme_minimal()

ggsave("out/Q1_b_2.png")

# The graphs give mixed visual support for the parallel trends assumption.
# Concretely, treated states have consistently higher divorce rates, but that is
# not a problem per se as a DiD setting controls for baseline differences.
# However, the pre-reform (1956–1968) evolution of the difference in divorce rates
# in treated states compared to the control group has a mild upward trend.
# In particular, the difference between the two groups prior to the reform period
# in the first (second) graph goes from 1.22 (1.24) to 1.55 (1.71) divorces per 1,000 people.
# From the graphs alone, we cannot assess whether these increases are statistically significant.
# Therefore, the graphs do not serve as clear visual support for the parallel trends assumption.

# (c)

data_3 <- data %>%
  filter(
    (lfdivlaw == 2000 | (lfdivlaw >= 1969 & lfdivlaw <= 1973)) &
      year %in% c(1968, 1978)
  ) %>%
  mutate(
    year = as.numeric(year),
    UNILATERAL = as.numeric(lfdivlaw >= 1969 & lfdivlaw <= 1973),
    POST = as.numeric(year == 1978),
    POST_UNILATERAL = (POST * UNILATERAL)
  )

#
# (i)

regression_1 <- feols(div_rate ~ POST_UNILATERAL + POST, data = data_3, weights = ~stpop, cluster = ~st)
summary(regression_1)

# (ii)

did_1 <- feols(div_rate ~ POST * UNILATERAL, data = data_3, weights = ~stpop, cluster = ~st)
summary(did_1)

# For regressions, where possible, we cluster standard errors by state.
# The pooled OLS regression (i) shows that, in 1978, treated states are associated with 1.70
# more divorces per 1,000 people compared to the control states, controlling for
# the average change in divorce rates common for both groups. This is statistically
# significant at the 0.1% level. The estimate lacks a causal DiD interpretation
# as the regression does not control for pre-treatment differences in average treatment
# and control group divorce rates.
# In the full DiD specification (ii), the interaction term is the DiD estimate for
# the effect of treatment on divorce rates. It is is both economically and statistically
# insignificant: -0.005 (SE: 0.202).
# The earlier graphs support this idea, with the control and treatment groups following
# similar trends pre-treatment. Further, after the reforms, both groups continued
# to see increases in divorce rates, but the gap between the two did not meaningfully
# widen. Instead, the two moved in parallel, explaining the small and insignificant
# DiD estimate in the second specification. Furthermore, the positive and statistically
# significant estimate for POST_UNILATERAL in regression (i) is corroborated by
# the treated group having higher divorce rates on average, as the estimated coefficient
# did not account for these baseline differences.

# (d)

data_3 <- na.omit(data_3)

means2 <- data_3 %>%
  group_by(UNILATERAL, POST) %>%
  summarise(avg = weighted.mean(div_rate, w = stpop), .groups = "drop")

m00 <- means2$avg[means2$UNILATERAL == 0 & means2$POST == 0]
m01 <- means2$avg[means2$UNILATERAL == 0 & means2$POST == 1]
m10 <- means2$avg[means2$UNILATERAL == 1 & means2$POST == 0]
m11 <- means2$avg[means2$UNILATERAL == 1 & means2$POST == 1]

diff_treated <- m11 - m10
diff_control <- m01 - m00
diff_post <- m11 - m01
diff_pre <- m10 - m00
did_3 <- (m11 - m10) - (m01 - m00)

did_mat <- matrix(
  c(m11, m01, diff_post, m10, m00, diff_pre, diff_treated, diff_control, did_3),
  nrow = 3,
  byrow = TRUE
)
rownames(did_mat) <- c("POST=1", "POST=0", "Difference 1")
colnames(did_mat) <- c("UNILATERAL=1", "UNILATERAL=0", "Difference 2")

print(did_mat)

did_mat_xlsx <- as.data.frame(did_mat)
did_mat_xlsx <- cbind(Group = rownames(did_mat_xlsx), did_mat_xlsx)

write_xlsx(did_mat_xlsx, "TABLE_1.xlsx")

# (e)

data_4 <- data %>%
  mutate(
    year = as.numeric(year),
    IMP_UNILATERAL = as.numeric(year >= lfdivlaw),
  ) %>%
  filter(year >= 1956 & year <= 1988)

# (i)

regression_2 <- feols(
  div_rate ~ IMP_UNILATERAL | st + year, cluster = ~ st, data = data_4, weights = ~ stpop
)

summary(regression_2)

# (ii)

regression_3 <- feols(
  div_rate ~ IMP_UNILATERAL | st + year + st[year], cluster = ~ st, data = data_4, weights = ~ stpop
)

summary(regression_3)

# (iii)

regression_4 <- feols(
  div_rate ~ IMP_UNILATERAL | st + year + st[year] + st[year^2], cluster = ~ st, data = data_4, weights = ~ stpop
)
summary(regression_4)

# All regressions match with Table A1 of the appendix in Wolfers (2006).

# Regression (i), which assumes common trends for the control and treatment groups,
# after controlling for state and year fixed effects, has a statistically insignificant
# coefficient of -0.055.
# Regressions (ii) and (iii), which include state-specific linear and quadratic trends,
# have coefficients of 0.477 and 0.334, respectively, both significant at the 5% level.

# The differences in coefficients across regressions suggest two interpretations.
# First, the treated and control groups could have different underlying state-specific
# trends correlated with treatment timing. Here, regressions (ii) and (iii) test
# whether the baseline results are robust to differential trajectories across states.
# The differences would indicate that the parallel trends assumption might be violated
# and the baseline regression may be biased by omitted state-specific trend heterogeneity.

# On the other hand, even under parallel trends, the treatment effect itself might
# be dynamic, changing over time. In this case, a single-coefficient design implicitly
# averages the underlying evolution of the treatment effect. Hence, the null coefficient
# in (i) might represent an actual effect that later declines. So, including state-specific
# trends would not correct for omitted heterogeneous trends, but could instead absorb
# the post-reform reversal.

# So, under the parallel trends assumption, i.e., with no omitted diverging state
# trends, and with a time-invariant treatment effect, the estimated coefficients
# should be similar across all specifications.

################################################################################
# (f) Illustration of naivete of TWFE estimator
################################################################################

library(dplyr)
library(fixest)
set.seed(55555)

df <- tibble(obs = 1:6) %>%
  mutate(
    state = floor(0.9 + obs/3)
  ) %>%
  group_by(state) %>%
  mutate(year = row_number()) %>%
  ungroup() %>%
  mutate(
    D = as.numeric((state == 1 & year == 3 ) | (state == 2 & year %in% c(2, 3))),
    # Create simulated outcomes
    Y = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + + runif(n())/100,
    Y2 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.3 * (state == 2 & year == 3 ) + runif(n())/100,
    Y3 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.4 * (state == 2 & year == 3 ) + runif(n())/100,
    Y4 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.5 * (state == 2 & year == 3 ) + runif(n())/100
  )

print(df)

# Regressions
summary(feols(sw(Y, Y2, Y3, Y4)  ~ D | state + year, data = df))

# Evidently, as the true treatment effect is 0.05, the treatment coefficient can
# only be consistently estimated for Y, where treatment effects are homogeneous.
# In Y2, Y3, Y4, the treatment effect has a post-treatment dynamic path, as the
# effect for state 2 in period 3 (i.e. for the already-treated state) increases
# from the baseline 0.05 by 0.3, 0.4, 0.5 respectively.
# Thus, the TWFE estimator can no longer recover the true treatment effect for these
# new outcome variables.

# Per de Chaisemartin and d'Haultfoeuille's contribution, with staggered treatment adoption,
# the TWFE coefficient is a weighted average of many DiD comparisons, including some
# where already-treated units serve as controls for newly treated ones in subsequent
# periods.
# From the output, we note that the coefficients for Y2, Y3, Y4 are -0.10, -0.15
# and -0.20 respectively, clearly different (including by sign) from the true effect.
# Since the individual ATT are trivially positive, the sign switch must be determined
# by the weights.

# In fact, the de Chaisemartin decomposition computes weights on each treated cell
# by projecting treatment on the space spanned by unit and time effects, residualizing
# it and then dividing it by the sum of the residualized treatment over all treated cells.
# As unit and time effects can be large for an early adopter, projecting out, or,
# in this additive setup, demeaning unit and time effects causes the numerator
# to be negative.

# Unlike Bacon weights (which are variance-based and always positive), de Chaisemartin
# weights can be directly negative.
# As heterogeneity grows from Y2 to Y4, the negatively weighted ATT becomes increasingly influential,
# eventually dragging the overall coefficient below zero.

################################################################################
# (g)
################################################################################

library(TwoWayFEWeights)

fun <- function(mod) {
  TwoWayFEWeights::twowayfeweights(
    data = df,
    Y = mod,
    G = "state",
    T = "year",
    D = "D"
  )
}

obj1 <- fun("Y")
obj4 <- fun("Y4")

obj1
obj4

# Under the common trends assumption, as obtained in part (f),
# the TWFE coefficient beta obtained from regressing Y and Y4 on D is, respectively,
# equal to 0.0445 and to -0.2003.

# The twowayfeweights output shows that the TWFE coefficient does not actually estimate
# the true treatment effect, but rather, as mentioned in (f), a sum of 2 positively
# weighted ATTs and one negatively weighted ATT (state 2 at time 3).
# The weights are equal for all 4 regressions because the underlying treatment design
# is the same.
# The estimated effect's sign switches when regressing Y4 on D instead of Y because
# the negatively weighted ATT gets larger, while the weight itself remains unchanged.

################################################################################
# (h)
################################################################################

## (i)

data_new <- data %>%
  group_by(st)%>%
  mutate(year = as.numeric(year),
          init_stpop = stpop[year == 1956])%>%   # Adding init_stpop variable
  ungroup()%>%
  mutate(IMP_UNILATERAL = as.numeric(year >= lfdivlaw))%>%
  filter(year>=1956 & year<=1988)

## (ii)

summary(feols(div_rate ~ IMP_UNILATERAL | st + year, cluster = ~ st, data = data_new, weights = ~ init_stpop))

## (iii)

library(bacondecomp)

# new dataset
data_new_squared <- data %>%
  group_by(st)%>%
  mutate(year = as.numeric(year),
          init_stpop = stpop[year == 1956][1],
          IMP_UNILATERAL = as.numeric(year >= lfdivlaw))%>%
  ungroup()%>%
  filter(year>=1956 & year<=1988)%>%
  filter(!is.na(div_rate))%>%
  group_by(st) %>%
  filter(n() == 33) %>% # Reason being that there are 52 NAs for div_rate. This way, we make sure that all 33 years are present
  ungroup()

# Bacon decomposition
bacon_output <- bacon(
  formula = div_rate ~ IMP_UNILATERAL,
  data = data_new_squared,
  id_var = "st",
  time_var = "year"
)
bacon_output

ggplot(bacon_output, aes(x = weight, y = estimate, color = type)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Weight",
    y = "2x2 DD estimate",
    color = "Comparison type",
    title = "Goodman-Bacon decomposition"
  ) +
  geom_hline(yintercept = -0.029861, color = "red") +
  annotate("text", x = .23, y = -.1, label = "DD estimate = -0.0299", color = "red") +
  coord_cartesian(xlim = c(0, 0.25)) +
  theme_classic()

ggsave("out/Q1_h.png", width = 9)

# The weighted mean of these estimates should be exactly the same as our earlier
# (naive) TWFE coefficient estimate. This is not exactly the case because of two
# reasons: first, the bacon model is unweighted; second, it uses a balanced sample.

# Goodman-Bacon (2021) show that "the TWFE DiD estimator is a weighted average of
# all possible 2x2 estimators that compare timing groups to each other. Some use
# units treated at a particular time as the treatment group and untreated units
# as the control group.
# Some compare units treated at two different times, using the later group as a control
# before its treatment begins and then the earlier group as a control after its treatment begins."

# The plot shows that the DD's with the highest weights, and thus the most influential,
# all belong to the Treated vs Untreated comparison type. This is somewhat reassuring, as
# these are the cleanest comparisons (never-treated units as controls).
# However, there is, in general, a wide dispersion in treatment effects
# across comparisons, especially so for the more problematic Later vs Earlier Treated
# and Later vs Always treated comparisons with ~17% of the weight.
# This means that the TWFE estimate is averaging opposing underlying comparisons.

# Note that all Bacon weights are positive by construction — the Bacon decomposition
# assigns weights proportional to subsample size and variance of treatment in that
# subsample, both of which are non-negative.
# Negativity in the Bacon decomposition shows up not in the weights but in the 2x2
# estimates themselves: the Later vs Earlier Treated dots are the contaminated
# comparisons where already-treated units act as controls.

# Negativity in the Bacon decomposition shows up in the 2x2 DiD themselves, and is not
# inherently problematic, as a negative 2x2 DiD could reflect a true negative effect.
# However, the fact that  "Later vs Earlier Treated" and "Later vs Always Treated"
# comparisons use already-treated units as controls make them invalid DiDs under
# treatment effect heterogeneity.

# The de Chaisemartin decomposition reframes this: instead of positive weights on
# negative estimates, it gives potentially negative weights directly on (positive) ATTs.

################################################################################
# (i)
################################################################################

dyn_values <- c(-9:-2, 0:14) # 9 leading and 16 lagging dummies, except extreme ones
dummy_maker <- function(tau) {
  as.numeric(tau == data$year - data$lfdivlaw)
}


names_list <- ifelse(dyn_values < 0,
                      paste0("dummy_m", abs(dyn_values)),
                      paste0("dummy_", dyn_values))

# Adding dummies to the dataset
data <- data %>%
  bind_cols(
    setNames(
      lapply(dyn_values, dummy_maker),
      names_list
    )
  )%>%
  mutate(
    dummy_lead10 = as.numeric(-10 >= year - lfdivlaw),
    dummy_lag15 = as.numeric(15 <= year - lfdivlaw)
  )%>%
  filter(year>=1956 & year<=1988)

## (i) group + time FE only
dummy_cols <- c(names_list, "dummy_lead10", "dummy_lag15")

mod1 <- feols(as.formula(paste("div_rate ~", paste(dummy_cols, collapse = " + "), "| st + year")),
                weights = ~stpop, cluster = ~st, data = data)

## (ii) linear trends
mod2 <- feols(as.formula(paste("div_rate ~", paste(dummy_cols, collapse = " + "), "| st + year + st[year]")),
                 weights = ~stpop, cluster = ~st, data = data)

## (iii) quadratic trends
mod3 <- feols(as.formula(paste("div_rate ~", paste(dummy_cols, collapse = " + "), "| st + year + st[year] + st[year^2]")),
                  weights = ~stpop, cluster = ~st, data = data)

summary(mod1)
summary(mod2)
summary(mod3)

# Adding leads and lags of the treatment dummy is useful because it allows one to
# investigate how treatment effect changes over time. While standard DiD assumes
# that the treatment effect is constant over time and units, we can thus confirm
# whether there were pre-trends and anticipation effect prior to the reform taking
# place, and whether the post-reform effect is temporary, delayed, growing in time,
# fading or reversing.
# Including state-specific linear/quadratic trends in the model means assuming that
# untreated outcomes differ by a linear/quadratic drift across states.
# We continue to use state populations as weights and to cluster SEs at the state level.

# The first regression, i.e. the basic event-study specification with state and
# year fixed effects, shows that the first two post-treatment dummies, i.e. D_1, D_2, equal
# 0.33 and 0.3 respectively, are statistically significant at the 5% level; no
# leading coefficients are significant; the output also shows that D_5 and the most distant
# lagging coefficients, from D_11 to D_15, are significant at the same level. Since this
# result seems hard to interpret, it is best to first observe the other two, more
# flexible models to see if it is persistent.

# The second regression with the smooth, state-specific linear drift shows that only
# the coefficients on D_1, D_2, D_3, D_5, D_6 are statistically significant at the 5% level.

# For the third regression, only the first two lagging coefficients appear significant.
# D_1, D_2 cluster around 0.25-0.4 across all three specifications, suggesting
# robustness for an immediate transitory treatment effect.

# While the inclusion of state-specific trends may control for omitted slow-moving
# confounders correlated  with treatment timing, Wolfers (2003) shows that
# when the treatment effect follows a non-monotonic dynamic path, as is the case here,
# with divorce rates spiking then declining, then state-specific trends can
# partially absorb the treatment effect itself.
# Thus the sensitivity of our results to including state-specific trends
# is itself informative: it does not necessarily imply that some omitted variable bias
# from state-specific sources was present in the basic model; it may instead signal that
# trends are soaking up genuine treatment variation.

# Evidently, performing this analysis allows us note that: firstly, there
# is no strong evidence of pre-trends and anticipation effects; secondly, the
# post-reform dynamic pattern gives some evidence that state-level divorce rates were
# impacted by the reforms only in the immediate years after implementation;
# lastly, this effect likely slowed down within the first ten years, with rates
# gradually returning to baseline levels.

# Since the significance of D_11-D_15 displayed by the first model is not robust
# to the inclusion of state-specific drifts, we could interpret it as a byproduct
# of failing to control for omitted state-specific trends and ignore it.
# An alternative interpretation of the fading and eventual reversal of the lagging
# coefficients is a mechanical compositional effect on the marriage pool, as discussed
# in Wolfers (2006). Unilateral divorce law facilitates the dissolution of "bad"
# marriages, that is, those in which at least one spouse preferred divorce but
# could not obtain consent. As these marriages are resolved in court, the remaining
# stock of marriages is of relatively better "quality", lowering the aggregate desire
# of divorce and mechanically reducing divorce rates a decade or more after reform.
# Under this reading, the insignificance of the most remote lagging coefficients does not
# necessarily imply that the treatment effect was temporary, but rather that the
# treatment had the secondary effect of lowering divorce "risk" via the mechanism
# of improving aggregate marriage quality.

# Nevertheless, as will be discussed in point (l), the staggered implementation setting
# remains a methodological problem: event with separate lead/lag dummies, the individual
# coefficients are a weighted average of different cohort effects, i.e., mixing
# the treatment effects for diffent relative periods.
# Thus, these results – no pre-trends and an immediate but transitory effect – could
# still be spurious.

################################################################################
# (j)
################################################################################

library(broom)
library(stringr)
library(readr)

extract_eventstudy <- function(model, model_name) {
  tidy(model, conf.int = TRUE) %>%
    filter(str_detect(term, "^dummy_")) %>%
    mutate(
      event_time = case_when(
        term == "dummy_lead10" ~ -10,
        term == "dummy_lag15"  ~ 15,
        str_detect(term, "^dummy_m") ~ -parse_number(term),
        str_detect(term, "^dummy_\\d+$") ~ parse_number(term),
        TRUE ~ NA
      ),
      model = model_name
    ) %>%
    select(model, term, event_time, estimate, conf.low, conf.high) %>%
    group_by(model) %>%
    group_modify(~ add_row(., term = "dummy_m1", event_time = -1, estimate = 0, conf.low = 0, conf.high = 0))
}

plot_data <- bind_rows(
  extract_eventstudy(mod1, "FE only"),
  extract_eventstudy(mod2, "FE + state linear trends"),
  extract_eventstudy(mod3, "FE + state quadratic trends")
) %>%
  arrange(model, event_time) %>%
  mutate(model = factor(model, c("FE only", "FE + state linear trends", "FE + state quadratic trends")))

ggplot(plot_data, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -1, linetype = "dotted") +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  facet_wrap(~ model, ncol = 1) +
  labs(
    x = "Years relative to unilateral divorce law",
    y = "Coefficient estimate",
    title = "Event-study coefficients with 95% confidence intervals"
  ) +
  theme_classic()

ggsave("out/Q1_j.png")

# k

library(grf)

data_l <- data %>%
  mutate (year = as.numeric (year))%>%
  filter(year>=1956 & year<=1988)

# k

# Once considering state-specific linear or quadratic trends, Friedberg (1998) concludes that unilateral divorce laws substantially and persistently raised divorce rates,
# accounting for a meaningful share of the rise in divorce rates after the late 1960s.
# Wolfers (2006), in contrast, finds that the effect is mainly temporary: divorce rates spiked after reforms, remained high for roughly a decade,
# and then returned to the pre-reform baseline,
# implying little long-run contribution of unilateral divorce laws to the overall rise in the divorce rate.
# Wolfers explains the difference by arguing that Friedberg’s single-treatment-dummy DiD with state-specific trends forces the treatment effect to look like a level shift,
# and, because the true effect is not constant over time, those trend controls partly absorb the post-reform decline after the initial spike,
# resulting in an overstated and overly persistent treatment effect.
# Wolfers's preferred interpretation is that unilateral divorce most likely shifted some divorces forward in time, with no robust evidence of a permanent increase in the total number of divorces.

# l

data_event <- data_l %>%
  mutate(
    time = year - lfdivlaw,
    time = case_when(
      time > 15 ~ 15,
      time < -10 ~ -10,
      .default = time
    ),
    year2 = year^2
  )

mod_sunab <- data_event %>%
  feols(
    div_rate ~ sunab(lfdivlaw, year, bin.rel = list("-10" = ~ x <= -10, "15" = ~ x >= 15), ref.c = 2000)
      | st + year + csw0(st[year], st[year2]),
    cluster = ~ st, weight = ~ stpop)

mod_sunab %>%
  summary()

iplot(mod_sunab)

# The Sun–Abraham correction addresses the contamination problem of standard TWFE event studies under staggered adoption:
# TWFE lead and lag coefficients can mix effects from different cohorts and relative periods, while Sun–Abraham estimates cohort-specific event-time effects and then aggregates them using appropriate controls.
# Using the Sun–Abraham estimator, we find little evidence of pre-trends in the baseline specification with state and year fixed effects.
# The introduction of unilateral divorce laws raises divorce rates in the short run, with positive and statistically significant effects in the first 2–3 years after reform.
# The effect then fades and eventually turns negative in later years.
# This pattern is qualitatively consistent with Wolfers (2006), who argues that unilateral divorce laws caused a temporary spike in divorce rates that largely reversed over time.
# The specification with state-specific linear trends gives a similar result but without the statistically significant long term decrease in divorce rates.
# By contrast, the quadratic-trend specification is problematic: it shows strong positive pre-treatment coefficients and large negative post-treatment effects.
# This, most likely, is a consequence of over-controling the counterfactual trend with the state-specific quadratic trend partly absorbing the effect of unilateral divorce laws, which is itself highly dynamic and curved over time.

# Exercise 2

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
# As a summary, it describes which observables are associated with more or less positive predicted treatment effects.
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
