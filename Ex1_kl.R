library(grf)
library(fixest)

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

data_l <- read.csv("files/pset_2.csv", sep = ";",) %>%
  mutate (year = as.numeric (year))%>%
  filter(year>=1956 & year<=1988)

# i

# Setup dummies

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

# Exactly as in Wolfers

data_event %>%
  mutate(
    time = case_when(
      time < 0 ~ -1,
      time %in% c(0, 1) ~ 1,
      time %in% c(2, 3) ~ 3,
      time %in% c(4, 5) ~ 5,
      time %in% c(6, 7) ~ 7,
      time %in% c(8, 9) ~ 9,
      time %in% c(10, 11) ~ 11,
      time %in% c(12, 13) ~ 13,
      time %in% c(14, 15) ~ 15
    )
  ) %>%
  feols(div_rate ~ i(time, ref = -1) | st + year + csw0(st[year], st[year2]), weight = ~ stpop) %>%
  etable()

# Our version

mod_event <- data_event %>%
  feols(div_rate ~ i(time, ref = -1) | st + year + csw0(st[year], st[year2]), weight = ~ stpop)

mod_event %>%
  summary()

iplot(mod_event)

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
