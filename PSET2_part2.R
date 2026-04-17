#Exercise 1 

################################################################################
# (f) Illustration of naivete of TWFE estimator
################################################################################

# Copy-pasting from pset pdf
library(dplyr)
df <- tibble (obs = 1 : 6) %>%
  mutate(
    state = floor ( 0.9 + obs/3)
  ) %>%
  group_by(state) %>%
  mutate(year = row_number()) %>%
  ungroup() %>%
  mutate(
    D = as.numeric((state == 1 & year == 3 ) | ( state == 2 & year %in% c (2, 3))),
    # Create simulated outcomes
    Y = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + + runif(n())/100,
    Y2 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.3 * ( state == 2 & year == 3 ) + runif(n())/100,
    Y3 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.4 * ( state == 2 & year == 3 ) + runif(n())/100,
    Y4 = 0.1 + 0.02 * (year==2) + 0.05 * (D==1) + 0.5 * ( state == 2 & year == 3 ) + runif(n())/100
  )
print(df)

# Regressions

summary(feols(Y  ~ D | factor(state) + factor(year), data = df))
summary(feols(Y2 ~ D | factor(state) + factor(year), data = df))
summary(feols(Y3 ~ D | factor(state) + factor(year), data = df))
summary(feols(Y4 ~ D | factor(state) + factor(year), data = df))


# Evidently, as the true treatment effect is 0.05, the treatment coefficient can 
# only be estimated efficiently for Y, where the TWFE, equal to 0.054, is homogeneous 
# across treatment observations.
# In Y2, Y3, Y4, the effect for state 2 in period 3 (i.e. for the already-treated
# state) is larger than for the other. In each new dependent variable, the 
# heterogeneous treatment bias increases, and the TWFE estimators with an absorbing
# treatment dummy can no longer recover the true effect.
# Per de Chaisemartin and d'Haultfeuille's contribution, with staggered timing, 
# the TWFE coefficient is a weighted average of many DiD comparisons, including some 
# where already-treated units serve as controls for newly treated ones in subsequent
# periods. Then, increasing treatment effect heterogeneity induces the coefficient to break.
# From the output, we note that the coefficients for Y2, Y3, Y4 are -0.01, -0.015
# and -0.02 respectively, clearly different (including by sign) from the true effect.

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

# Under the common trends assumption, as obtained in part (f),
# the TWFE coefficient beta obtained from regressing Y and Y4 on D is, respectively, 
# equal to 0.0473 and to -0.2034.

obj4

# The twowayfeweights output shows that the TWFE coefficient does not actually estimate 
# the true treatment effect, but rather, as mentioned in (f), a sum of 2 
# positively weighted ATTs and one negatively weighted ATT.
# The sign switches when regressing Y4 on D instead of Y, because 
# the negatively weighted ATT becomes more influential as the heterogeneous 
# treatment bias increases.

################################################################################
# (h)
################################################################################

data <- read.csv("~/Documents/R_main/R_econometrics/files/pset_2.csv", sep = ";",)

## (i)

data_new <- data %>%
  group_by(st)%>%
  mutate (year = as.numeric(year),
          init_stpop = stpop[year == 1956])%>%   # Adding init_stpop variable
  ungroup()%>%
  filter((lfdivlaw == 2000 | (lfdivlaw >= 1968 & lfdivlaw <= 1988)) & year %in% c(1968, 1978)) %>%
  mutate( UNILATERAL = as.numeric(lfdivlaw >= 1969 & lfdivlaw <= 1973),
          POST = as.numeric(year == 1978),
          POST_UNILATERAL = (POST*UNILATERAL),
          IMP_UNILATERAL = as.numeric(year >= lfdivlaw))%>%
  filter(year>=1956 & year<=1988)

## (ii)

summary(feols(div_rate ~ IMP_UNILATERAL | st + year,
              cluster = "st", data = data_new, weights = data_new$init_stpop)
)

## (iii)

library(bacondecomp)
library(ggplot2)

# new dataset
data_new_squared <- data %>% 
  group_by(st)%>%
  mutate (year = as.numeric(year),
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


ggplot(bacon_output, aes(x = weight, y = estimate, color = type)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Weight",
    y = "2x2 DID estimate",
    color = "Comparison type",
    title = "Goodman-Bacon decomposition"
  ) +
  theme_classic()

# The weighted mean of these estimates would be exactly the same as our earlier (naive) 
# TWFE coefficient estimate, recalling that the regression must be unweighted and 
# run on the same balanced sample that bacon runs on.

# Discussion on Goodman-Bacon




################################################################################
# (i)
################################################################################

library(fixest)

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
    dummy_lead10 = as.numeric( -10 >= year - lfdivlaw),
    dummy_lag15 = as.numeric( 15 <= year - lfdivlaw)
  )

## (i)

dummy_cols <- c(names_list, "dummy_lead10", "dummy_lag15")

mod1 <- feols(as.formula(paste("div_rate ~", paste(dummy_cols, collapse = " + "), "| st + year")),
      weights = ~stpop, cluster = ~st, data = data)

## (ii)

mod2 <- feols(as.formula(paste("div_rate ~", paste(dummy_cols, collapse = " + "), "| st + year + st[year]")),
      weights = ~stpop, cluster = ~st, data = data)

## (iii)

mod3 <- feols(as.formula(paste("div_rate ~", paste(dummy_cols, collapse = " + "), "| st + year + st[year] + st[year^2]")),
      weights = ~stpop, cluster = ~st, data = data)

# comment
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
# untreated outcomes differ by a linear/quadratic drift across states

# The first regression, i.e. the basic event-study specification with state and 
# year fixed effects, shows that the first two lagging dummies, i.e. D_1, D_2, equal
# to 0.32 and 0.3 respectively, are statistically significant at the 5% level; no leading dummies are
# significant; the output also shows that the most distant lagging dummies, from D_11
# to D_15, are significant at the same level. Since this result seems hard to interpret,
# it is best to first observe the other two, more flexible models to see if it is 
# persistent.

# The second regression with the smooth, state-specific linear drift is mostly consistent,
# showing that the only dummies to be statistically significant at the 5% level are the 
# first seven lagging dummies. Since the significance of D_11-D_15 is not robust to 
# the inclusion of state-specific drifts, we can assume it is a byproduct of omitted
# state-specific trends, and ignore it.

# The third regression is entirely consistent with the other models' results.
# D_1, D_2 cluster around 0.3-0.38 across all three specifications, suggesting 
# robustness.
# We have also decided to use state populations as weights and to cluster SEs at 
# the state level in order to align our analysis to Wolfers'.

# Evidently, performing this analysis now allows us to say that, firstly, there is no evidence 
# of pre-trends and anticipation effects, as we expect from the analysis of a new law's
# effect; secondly, that the post-reform dynamic pattern shows that divorce rates were
# impacted by the reform, as it went into effect in each state, mostly in the 
# immediate years after the reform, that this effect slowed down substantially within 
# the first ten years and allowed rates to return to baseline levels after that.
# According to our most flexible model with quadratic state-specific trends, the window
# for the effect to persist is seven years out.


################################################################################
# (j)
################################################################################

library(broom)
library(stringr)

extract_eventstudy <- function(model, model_name) {
  tidy(model, conf.int = TRUE) %>%
    filter(str_detect(term, "^dummy_")) %>%
    mutate(
      event_time = case_when(
        term == "dummy_lead10" ~ -10,
        term == "dummy_lag15"  ~ 15,
        str_detect(term, "^dummy_m") ~ -as.numeric(str_remove(term, "^dummy_m")),
        str_detect(term, "^dummy_\\d+$") ~ as.numeric(str_remove(term, "^dummy_")),
        TRUE ~ NA_real_
      ),
      model = model_name
    ) %>%
    select(model, term, event_time, estimate, conf.low, conf.high)
}

plot_data <- bind_rows(
  extract_eventstudy(mod1, "FE only"),
  extract_eventstudy(mod2, "FE + state linear trends"),
  extract_eventstudy(mod3, "FE + state quadratic trends")
) %>%
  arrange(model, event_time)

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
  theme_minimal()
