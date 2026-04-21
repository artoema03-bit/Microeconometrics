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
# We must use analytic weights.
# We do so because the dependent variable (divorce rate per 1,000 people) is a mean
# computed from state-level populations (stpop) of different sizes.

# Larger states may provide estimates with lower variance, so weighting by population
# accounts for heteroskedasticity and ensures that estimates are representative
# of the U.S. population.
# Likewise, when aggregating divorce rates for vizualizations, analytic weights
# allow to construct population-weighted average divorce rates.

# Frequency weights are not relevant, since the divorce rates are state-year specific.
# Using them would imply div_rate would be observed as if each observation were
# repeated stpop times.

# Probability weights are necessary when when data come from a random sample rather
# than the full population, this is not relevant in this scenario.

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
# The pooled OLS regression (i) shows that, in 1978, treated states are associated
# with 1.70 more divorces per 1,000 people compared to the control states, controlling for
# the average change in divorce rates common for both groups. This is statistically
# significant at the 0.1% level. The estimate lacks a causal DiD interpretation
# as the regression does not control for pre-treatment differences in average treatment
# and control group divorce rates.
# In the full DiD specification (ii), the interaction term is the DiD estimate for
# the effect of treatment on divorce rates. It is is both economically and statistically
# insignificant: -0.005 (SE: 0.202).
# From the earlier graphs we see that, the positive and statistically significant
# estimate for POST_UNILATERAL in regression (i) is picking up the treated group
# having higher divorce rates on average, as the estimated coefficient did not account
# for these baseline differences. For the second regression, after the reforms,
# both groups continued to see increases in divorce rates, but the gap between the
# two did not meaningfully widen, explaining the small and insignificant DiD estimate.

################################################################################
# (d)
################################################################################

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
# (f)
################################################################################

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
    Y = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + runif(n())/100,
    Y2 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.3 * (state == 2 & year == 3 ) + runif(n())/100,
    Y3 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.4 * (state == 2 & year == 3 ) + runif(n())/100,
    Y4 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.5 * (state == 2 & year == 3 ) + runif(n())/100
  )

print(df)

# Regressions
summary(feols(sw(Y, Y2, Y3, Y4)  ~ D | state + year, data = df))

# With a true treatment effect of 0.05, the treatment effect is consistently estimated
# only for Y (0.045 due to noise), where treatment effects are homogeneous.
# For Y2, Y3, Y4, treatment effect heterogeneity is introduced: the effect for state 2
# in period 3 (i.e. for the already-treated state) increases from the baseline 0.05
# by 0.3, 0.4, 0.5 respectively.
# In this case, the TWFE estimator has no clean causal interpretation as a common
# treatment effect. The coefficients for Y2, Y3, Y4 are -0.10, -0.15 and -0.20,
# respectively, clearly different in magnitude and sign from the common 0.05 effect.

# Per de Chaisemartin and d'Haultfoeuille, with staggered treatment adoption, the
# TWFE coefficient is a weighted average of group-time ATTs. Nothing prevents those
# weights from being negative, so even with positive ATTs, the sign on the TWFE
# estimate can be negative if negative-weighted ATTs overpower the rest.

# In fact, the de Chaisemartin decomposition computes weights on each treated cell
# by projecting treatment on the unit and time effects, residualizing it and then
# dividing it by the sum of the residualized treatment over all treated cells.
# This means that some group-time cells, like the already-treated state 2 in period 3,
# are assigned negative residualized treatment, which then leads to negative weights.
# As Y2, Y3, Y4 make the treatment effect in that negatively weighted ATT increasingly
# large, it drags the overall coefficient below zero.

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

# As obtained in (f), the TWFE coefficient from regressing Y and Y4 on D is, respectively,
# 0.045 and -0.200.

# The twowayfeweights output shows that the TWFE coefficient represents, as mentioned
# in (f), a weighted average of two positively-weighted group-time ATTs and one
# negatively-weighted ATT (the already-treated state 2 at time 3). The ATT weights
# remain equal for all 4 regressions as the underlying treatment design is the same.
# Comparing Y4 to Y, the negatively-weighted ATT becomes large enough to turn the
# estimated effect negative.

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

weighted.mean(bacon_output$estimate, bacon_output$weight)

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
geom_hline(yintercept = -0.583842, color = "red") +
  annotate("text", x = .225, y = -.65, label = "Bacon estimate = -0.5838", color = "red") +
  coord_cartesian(xlim = c(0, 0.25)) +
  theme_classic()

ggsave("out/Q1_h.png", width = 9)

# Goodman-Bacon (2021) show that "the TWFE DiD estimator is a weighted average of
# all possible 2x2 estimators that compare timing groups to each other. Some use
# units treated at a particular time as the treatment group and untreated units
# as the control group.
# Some compare units treated at two different times, using the later group as a
# control before its treatment begins and then the earlier group as a control after
# its treatment begins."
# Bacon weights are positive by construction – the Bacon decomposition assigns weights
# proportional to subsample size and variance of treatment in that subsample, both
# of which are non-negative. Thus, there is no negative weight problem in the Bacon decomposition itself.
# Heterogenous/dynamic treatment effect issues show up in the Bacon decomposition
# not in the weights but in contaminated 2x2 comparisons – Later vs Earlier Treated
# and Later vs Always Treated –, where already-treated units act as controls.

# The weighted mean of these Bacon DD estimates differs from our earlier (naive)
# TWFE coefficient because the Bacon decomposition is run on an unweighted, balanced sample.
# The plot shows that the DD's with the highest weights, the most influential ones,
# all belong to the Treated vs Untreated comparison type, which are the cleanest
# comparisons (never-treated units as controls).
# However, there is, in general, a wide dispersion in DD estimates across comparisons,
# especially so for the more problematic Later vs Earlier Treated and Later vs Always
# Treated comparisons with ~17% of the weight.
# This means that the TWFE estimate is partly averaging opposing underlying comparisons.

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

## (i), (ii), (iii)
dummy_cols <- c(names_list, "dummy_lead10", "dummy_lag15")

mod1 <- feols(as.formula(paste("div_rate ~", paste(dummy_cols, collapse = " + "), "| st + year + csw0(st[year], st[year^2])")),
                  weights = ~stpop, cluster = ~st, data = data)

summary(mod1)

# Adding leads and lags of the treatment dummy is useful because it allows one to
# investigate how treatment effect changes over time. As standard DiD assumes
# a common baseline trend for the treatment and control groups, we can thus test
# whether there were pre-trends and anticipation effect prior to the reform taking
# place, and whether the post-reform effect is temporary, delayed, growing in time,
# fading or reversing.

# For all three regressions, pre-reform leading coefficients are all individually
# insignificant, providing no obvious evidence of pre-trends or anticipation effects.

# For the post-reform path, the first regression shows that two immediate post-treatment
# effects, D_1, D_2, and the 5-periods-out effect, D_5, are positive and statistically
# significant at the 5% level (0.34, 0.3, 0.25, respectively). Also, the most distant
# post-treatment coefficients, from D_11 to D_15, are negative and significant at
# the 5% level.
# The second regression, with state-specific linear drift, shows that only immediate
# post-treatment effects, D_1, D_2, D_3, D_5, D_6, are positive and statistically
# significant at the 5% level.
# For the third regression, only the first two post-treatment coefficients are significant
# at the 5% level and positive.

# D_1, D_2 remain statistically significant and cluster around 0.25-0.4 across all
# three specifications, giving robust evidence of an immediate short-run treatment effect.

# Left unclear is the interpretation of the apparent reduction of divorce rates
# a decade after the reforms, which is statistically significant only for the basic
# TWFE regression.
# Reiterating our discussion in (e), while the inclusion of state-specific trends
# may control for omitted slow-moving confounders correlated with treatment timing,
# Wolfers (2006) shows that when the treatment effect follows a non-monotonic dynamic
# path, as might also be the case here, then state-specific trends could partially
# absorb the treatment effect itself.
# Thus, the sensitivity of our results to including state-specific trends remains
# unexplained: it does not necessarily imply that omitted variable bias from state-specific
# trends is present and that the reversal results should be discarded; it may instead
# signal that the trends are soaking up genuine treatment dynamics.

# Nevertheless, as will be discussed in (l), Sun and Abraham conclude that the staggered
# implementation setting remains a methodological problem by itself: even with separate
# lead/lag dummies, the individual coefficients are a weighted average of different
# cohort effects, mixing treatment effects for diffent relative periods.
# Thus, these results – no obvious pre-trends and a short-run positive effect –
# should still be interpreted cautiously.

################################################################################
# (j)
################################################################################

library(broom)

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

# Once considering state-specific linear or quadratic trends, Friedberg (1998) concludes
# that unilateral divorce laws substantially and persistently raised divorce rates,
# accounting for a meaningful share of the rise in divorce rates after the late 1960s.
# Wolfers (2006), in contrast, finds that the effect is mainly temporary: divorce
# rates spiked after reforms, remained high for roughly a decade, and then returned
# to the pre-reform baseline, implying little long-run contribution of unilateral
# divorce laws to the overall rise in the divorce rate.
# Wolfers explains the difference by arguing that Friedberg’s single-treatment-dummy
# DiD with state-specific trends forces the treatment effect to look like a level
# shift, and, because the true effect is not constant over time, those trend controls
# partly absorb the post-reform decline after the initial spike, resulting in an
# overstated and overly persistent treatment effect.
# Wolfers's preferred interpretation is that unilateral divorce most likely shifted
# some divorces forward in time, with no robust evidence of a permanent increase
# in the total number of divorces.

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

# The Sun–Abraham correction addresses the contamination problem of standard TWFE
# event studies under staggered adoption: TWFE lead and lag coefficients can mix
# effects from different cohorts and relative periods, while Sun–Abraham estimates
# cohort-specific event-time effects and then aggregates them using valid comparison groups.
# Using the Sun–Abraham estimator, we find little evidence of pre-trends in the
# baseline specification with state and year fixed effects. The introduction of
# unilateral divorce laws raises divorce rates in the short run, with positive and
# statistically significant effects in the first 2–3 years post-reform. The effect
# then fades and eventually turns negative in later years.
# This pattern is qualitatively consistent with Wolfers (2006), who argues that
# unilateral divorce laws caused a temporary spike in divorce rates that largely
# reversed over time.
# The specification with state-specific linear trends gives a similar result but
# without the statistically significant long term decrease in divorce rates.
# By contrast, the quadratic-trend specification is problematic: it shows strong
# positive pre-treatment coefficients and large negative post-treatment effects.
# This, most likely, is a consequence of over-controling the counterfactual trend
# with the state-specific quadratic trend partly absorbing the effect of unilateral
# divorce laws, which is itself highly dynamic and curved over time.
# Overall, at least for base and linear-trend cases, the Sun–Abraham estimator adds
# to the evidence of no apparent pre-trends and also affirms that the post-reform
# estimate dynamics are not artifacts of staggered contamination. The question of
# whether the base case's reversal is a true effect or caused by omitted trends
# remains open.

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

# For the causal forest estimation, we use the 1968–1978 difference in county divorce
# rates as outcomes, conditioned on 1968 controls.
# The estimation is also clustered at the state level, as that is the treatment
# assignment level.
# The causal forest estimates an average treatment effect of about 0.034 with a
# standard error 0.068, so the average effect is small and not statistically different
# from zero, aligned with our conclusion for 1.c.
# Therefore, on average, unilateral divorce laws do not appear to have a clear effect
# on divorce rates in this restricted sample. However, a near-zero ATE does not
# rule out TE heterogeneity as it may have arisen due to positive and negative effects
# offsetting each other in the average.

# b

## Variable importance
diagn_imp <- tibble(
  variable = colnames(data_exp_covar),
  importance = as.double(variable_importance(mod_base))
) %>%
  arrange(desc(importance))
diagn_imp


# The most important variable in terms of splitting frequency is religious adherence,
# so heterogeneity is strongly associated with it. Women labor force participation
# is the second most important variable, followed by education rate and domestic
# violence rate. Urbanization, however, has almost zero importance, so it contributes
# little to the forest's splits.
# Anyways, variable importance is only a heuristic and should not be interpreted
# as a causal ranking of covariates.

## Best linear projection
diagn_blp <- best_linear_projection(mod_base, data_exp_covar)
diagn_blp

# The best linear projection is a doubly robust linear projection of the estimated
# CATEs on covariates. As a summary, it describes which observables are associated
# with more or less positive predicted treatment effects.
# The BLP shows that heterogeneity is linearly related to religious adherence, women's
# labor force participation, and, more weakly, domestic violence. The coefficient
# on religious_adherence_1968 is negative and highly significant: counties with
# higher religious adherence tend to have less positive (more negative) estimated
# treatment effects. The coefficient on women_labor_force_participation_1968 is
# positive and significant: counties with higher female labor force participation
# tend to have more positive estimated treatment effects. The coefficient on domestic_violence_rate_1968
# is positive and only weakly significant, its role is less precisely estimated.
# Other covariates are not statistically significant, so there is little evidence
# that they are linearly related to the CATEs.

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


# The TOC checks whether the causal forest is actually good at ranking units by
# treatment effect. For a given fraction of top-ranked treated units, it compares
# the ATE among these units to the overall ATE. The AUTOC is the area under this
# curve: a positive and statistically significant AUTOC indicates the ranking contains
# useful heterogeneity information.

# We evaluate the TOC out of sample, splitting observations at the state-level as
# treatment is assigned at the state level. The TOC curve is mostly above zero for
# low and intermediate treated fractions, which means that the observations ranked
# by the forest as having high treatment effects do tend to have higher-than-average effects.
# However, the AUTOC is about 0.28 with a wide confidence interval (+/- 0.29), so
# this evidence is only suggestive of the idea that the forest captures real heterogeneity
# and is able to rank observations by treatment effect better than chance.

## CATEs
mod_cate <- predict(mod_base)$predictions

hist(mod_cate)

png("out/Q2_b_2.png")
hist(mod_cate)
dev.off()

# The histogram of estimated CATEs shows substantial dispersion around zero, consistent
# with treatment effect heterogeneity.

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

ggsave("out/Q2_b_3.png")

# We plot the estimated CATEs with respect to the 8 most important covariates.
# The CATE plots are consistent with the other diagnostics and suggests that the
# clearest source of treatment effect heterogeneity is religious_adherence_1968.
# There appears to be a non-linear relation between CATEs and religious adherence:
# counties with low religious adherence tend to have positive estimated treatment
# effects, while counties with high religious adherence tend to have negative or
# near-zero estimated treatment effects.
# This pattern is much stronger than for the other covariates and is consistent
# with both the variable-importance ranking and the BLP results. So, for ease of
# vizualization, we split the sample into two groups: counties with religious adherence
# below and above 50.
# Women's labor force participation and domestic violence also show some positive,
# but weaker, association with the CATE. Other variables show weaker and less stable patterns.

# c

# It should be noted that because treatment varies at the state level while the
# causal forest is estimated on county-level observations, the source of independent
# treatment variation is the state, not the county. So, the county-level heterogeneity
# patterns should be interpreted cautiously.

# Taking that into account, our results provide some suggestive evidence of heterogeneous
# treatment effects, even though the overall ATE is close to zero.
# First, the BLP indicates some statistically significant linear associations between
# the CATEs and pre-treatment covariates, especially religious adherence and women's
# labor force participation.
# Second, the TOC curve is mostly positive, suggesting that the forest's ranking of units
# by predicted treatment effect might contain useful information, but the AUTOC
# is imprecise for a strong conclusion.
# Third, the CATE plots show a contrast between low- and high-religious-adherence
# counties.
# Therefore, there is some evidence that the ATE might be small not because the
# treatment is ineffective, but because positive and negative effects offset each
# other across different counties.

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

# Honest trees use one part of the sample to construct the tree by choosing splits
# and another part to estimate treatment effects within terminal leaves. This reduces
# overfitting, makes CATE estimation more credible and allows for valid asymptotic
# inference. Without splitting the sample, the same potentially noisy outcomes are
# used to choose the tree splits and then to estimate leaf treatment effects, exaggerating
# estimated heterogeneity.
# In our results, disabling honesty barely changes the estimated ATE: the honest
# forest gives about 0.0343 and the dishonest forest about 0.0339. So, honesty is
# not very important for the average treatment effect in this sample, but it does
# have an effect on treatment effect heterogeneity.
# Without honesty, variable importance becomes even more concentrated on religious
# adherence and the distribution of predicted CATEs becomes much more spread out.
# This suggests that the dishonest forest is fitting stronger and potentially noisier
# heterogeneity patterns. Therefore, honest forests are more thrustworthy for interpreting
# CATEs, even though both approaches give almost the same ATE.
# One could expect the difference between honest and dishonest forests to be larger
# in smaller samples, with noisier outcomes, weaker overlap, deeper trees,
# or relatively many covariates. In these cases, using the same data both for model
# construction and effect estimation makes the risk of overfitting noise worse,
# potentially changing the ATE.
