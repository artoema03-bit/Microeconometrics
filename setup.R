# Common packages
library(tidyverse)
library(stargazer)

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

# clearly tidier to use a function
reg_extractor <- function(linmod, treat = "train") {
  tau <- summary(linmod)
  coeffs_gen <- summary(linmod)$coefficients
  mf <- model.frame(linmod)
  if (! treat %in% names(mf)) stop("treatment variable not found in model frame")

  n1_new <- sum(mf[[treat]] == 1, na.rm = TRUE)
  n0_new <- sum(mf[[treat]] == 0, na.rm = TRUE)
  coef_mod <- coeffs_gen[treat, "Estimate"]
  se_mod <- coeffs_gen[treat, "Std. Error"]
  p_value_mod <- coeffs_gen[treat, "Pr(>|t|)"]
  c(n1_new = n1_new, n0_new = n0_new, Estimate = coef_mod, SE = se_mod, p_value = p_value_mod)
}
