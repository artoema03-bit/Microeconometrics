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


ggplot(bacon_output, aes(x = estimate, y = weight, color = type)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "2x2 DID estimate",
    y = "Weight",
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
