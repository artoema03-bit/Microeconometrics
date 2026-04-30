# Set wd
user <- Sys.info()["user"]
output_dir <- switch(
  user,
  "ajnik" = "G:/Mans disks/zObsidian/04 Courses/20295 Microeconometrics/Problem Sets/microeconometrics-ps",
  "erick" = "/home/erick/TEMP/",
  "emanuele" = "/Users/emanueleartoni/Documents/R_main/R_econometrics",
  getwd()
)
setwd(output_dir)

# Load packages
library(tidyverse)
library(rdrobust)
library(rdss)
library(modelsummary)

glance.rdrobust <- function(object, ...){
    ret <- data.frame(nobs.left = object$N[1],
                      nobs.right = object$N[2],
                      nobs.effective.left = object$N_h[1],
                      nobs.effective.right = object$N_h[2],
                      cutoff = object$c,
                      order.regression = object$p,
                      order.bias = object$q,
                      kernel = object$kernel,
                      bwselect = object$bwselect)
        ret
}

################################################################################
# Exercise 2
################################################################################

data_fraud <- read.csv("files/fraud_pcenter_final.csv", sep = ";") %>%
  # Following Gonzalez, drop one-sided segments
  filter(ind_seg50 == 1) %>%
  mutate(X_dist_aligned = if_else(cov == 0, -X_dist, X_dist))

################################################################################
# (a)
################################################################################

# Gonzalez uses dist/distance, we use X_dist

rdplot(data_fraud$cov, data_fraud$X_dist_aligned, p = 1)

summary(rdrobust(data_fraud$cov, data_fraud$X_dist_aligned, p = 1, all = TRUE))

# We plot the signed version of _dist, where uncovered polling centers have their
# distance inverted. The noisy measurement of longitude means that there are some
# covered centers with a negative distance, implying they are measured as if outside
# the coverage boundary, and some uncovered centers with a positive distance, implying
# they are measured as if inside the boundary.
# This means that coverage is no longer a deterministic function of distance: having
# a positive distance, being inside the coverage boundary, no longer definitively means
# that the center has coverage, but instead increases the probability of being covered.
# In the RD regression of coverage on distance, we see that the estimated jump in
# coverage probability at the cutoff is about 12pp, but the estimate is imprecise.
# Thus, given noisy distance measurements, the design must be treated as a fuzzy RD.

# On assumptions, for Gonzalez's sharp design we need
# - Continuity of potential outcomes at the boundary
# - No precise manipulation of the running variable around the boundary
# - Treated and control observations on both sides of boundary segments
# For the fuzzy RDD reinterpretation, we need
# - First-stage relevance: the probability of treatment increases at the boundary
# - Exclusion: crossing the boundary affects outcomes only through treatment
# - Monotonicity (no defiers) for the interpretation of the fuzzy RDD Wald ratio as a LATE


################################################################################
# (b)
################################################################################

# Having only a proxy for longitude would not require changing the design if the
# relevant coverage boundary was horizontal (East-West), i.e., determined only by
# latitude and not longitude. As latitude is measured correctly, the running variable
# would still be correctly constructed, so treatment assignment around the cutoff
# would remain sharp, as in Gonzalez (2021).
# The problem arises for vertical (North-South) or diagonal boundaries, because
# then distance to the boundary depends on longitude, which is measured with error here.


################################################################################
# (c)
################################################################################

run_rdd <- \(y, x, b, reg, fuzzy = FALSE, orig = FALSE) {
  data_mod <- data_fraud %>%
    filter(ind_seg50 == 1) %>%
    filter(reg == 0 | (reg == 1 & region2 == "East") | (reg == 2 & region2 == "North"))

  data_mod_fe <- model.matrix(~ factor(segment50) - 1, data = data_mod)

  if (missing(b)) {
    mod_bw <- rdbwselect(
      data_mod[[y]], data_mod[[x]], kernel = "tri", cluster = data_mod[["segment50"]],
      vce = if(orig) "hc0" else "nn",
      masspoints = if (orig) "off" else "adjust"
    )

    b <- mod_bw$bws[1]
  }

  mod <- data_mod %>%
    fixest::feols(
      as.formula(paste0(y, " ~ ", "cov*", x, " | segment50")),
      fixef.rm = "none", cluster = ~ segment50,
      subset = as.formula(paste0("~ abs( ", x, ") <= ", b))
    )

  mod
}

run_rdd_fuzzy <- \(y, x, b, reg, orig = FALSE) {
  data_mod <- data_fraud %>%
  filter(ind_seg50 == 1) %>%
  filter(reg == 0 | (reg == 1 & region2 == "East") | (reg == 2 & region2 == "North"))

  data_mod_fe <- model.matrix(~ factor(segment50) - 1, data = data_mod)

  if (missing(b)) {
    mod_bw <- rdbwselect(
      data_mod[[y]], data_mod[[x]], fuzzy = data_mod[["cov"]], kernel = "tri", cluster = data_mod[["segment50"]],
      vce = if(orig) "hc0" else "nn",
      masspoints = if (orig) "off" else "adjust"
    )

    b <- mod_bw$bws[1]
  }

  mod <- rdrobust(
    data_mod[[y]], data_mod[[x]],fuzzy = data_mod[["cov"]], covs = data_mod_fe, kernel = "uni",
    all = TRUE, h = b, cluster = data_mod[["segment50"]]
  )

  mod
}

get_rdd_stats <- \(model) {
  nneighb <- model$fixef_size[[1]]
  avg_nocov <- mean(model.matrix(model, type = "lhs")[model.matrix(model)[, 2] < 0])
  bw <- as.double(str_extract(model$model_info$subset, "(?<=<= ).*"))

  tibble(avg_nocov, bw, nneighb)
}

get_rdd_stats_fuzzy <- \(model) {
  nobs <- sum(model$N_h)
  bw <- model$bws[[1]]

  tibble(nobs, bw)
}

rdd_stat_fmt <- tribble(
  ~ raw, ~ clean, ~ fmt,
  "nobs", "Num.Obs.", 0,
  "avg_nocov", "Mean outside coverage", 3,
  "bw", "Bandwidth (km)", 3,
  "nneighb", "Neighborhoods", 0
)

# Perfect replication
mod_perf <- expand_grid(y = c("vote_comb_ind", "vote_comb"), x = "distance", reg = c(0, 1, 2)) %>%
  rowwise() %>%
  mutate(
    mod = list(run_rdd(y, x, reg = reg, orig = TRUE)),
    mod = setNames(list(mod), paste0(y, " (", case_match(reg, 0 ~ "All", 1 ~ "Southeast", 2 ~ "Northwest"), ")"))
  )

# map(mod_perf$mod, summary)

modelsummary::modelsummary(
  mod_perf$mod,
  output = "huxtable", coef_omit = "dist",
  gof_map = rdd_stat_fmt,
  gof_function = get_rdd_stats
)

# Imperfect replication
mod_imperf <- expand_grid(y = c("vote_comb_ind", "vote_comb"), x = "X_dist_aligned", reg = c(0, 1, 2)) %>%
  rowwise() %>%
  mutate(
    mod = list(run_rdd_fuzzy(y, x, reg = reg, orig = TRUE)),
    mod = setNames(list(mod), paste0(y, " (", case_match(reg, 0 ~ "All", 1 ~ "Southeast", 2 ~ "Northwest"), ")"))
  ) %>%
  ungroup()

map(mod_imperf$mod, summary)

modelsummary(
  mod_imperf$mod,
  output = "huxtable",
  coef_omit = "^(?!Con)",
  gof_map = rdd_stat_fmt,
  gof_function = get_rdd_stats_fuzzy
)

# We attempt to replicate Gonzalez (2021) as closely as possible. Their estimation
# slightly differs from current default settings: they use a triangular kernel for
# the optimal bandwidth and a equal-weighted local linear regression with segment
# FEs for the actual RDD effect (similar to using a uniform kernel and including
# segment dummies). Also, they compute neighborhood-clustered standard errors.
# For estimating the fuzzy RDD, we mimic this specification, but instead of an explicit
# 2SLS estimation procedure, we use `rdrobust` with the `fuzzy` option.

# Under the previously listed assumptions, the fuzzy-RD estimates should be considered
# as LATEs for compliers at the boundary.
# These effects are qualitatively aligned with the original ones, i.e., that coverage
# reduces fraud at near-boundary polling centers. On average, the fuzzy-RD point
# estimates imply a 13.8pp reduction in the probability of at least one fraudulent
# station and a 12pp lower share of fraudulent votes. This effect is also stronger
# for polling centers in the Southeastern region, while for centers in the Northwestern
# region, the effects are close to zero.
# The greater noisiness and larger magnitudes in the fuzzy RDD estimates is partly
# due to the scaling by the first stage RDD of treatment probability being less
# than one. So, these results should be interpreted with some caution.
