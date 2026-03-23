# Common packages
library(tidyverse)
library(stargazer)
library(modelsummary)

options("modelsummary_factory_default" = "huxtable")

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

Table_original_function <- function(data, vars, treat_var) {
  tvar <- data[[treat_var]]

  n1 <- sum(tvar == 1)
  n0 <- sum(tvar ==0)

  table_m <- matrix(NA, nrow = 7, ncol = length(vars))
  rownames(table_m) <- c(
    "Mean treated",
    "Mean control",
    "SD treated",
    "SD control",
    "Diff. in means",
    "SE",
    "p-value"
  )
  colnames(table_m) <- vars

  for (i in seq_along(vars)) {
    target <- data[[vars[i]]]

    treated <- target[tvar==1]
    control <- target[tvar==0]
    treated_mean <- mean(treated)
    control_mean <- mean(control)
    treated_sd <- sd(treated)
    control_sd <- sd(control)
    diff_in_means <- treated_mean - control_mean
    SE_diff_in_means <- sqrt(treated_sd^2/n1+control_sd^2/n0)
    p_val <- t.test(x = treated, y = control, alternative = "two.sided", var.equal = FALSE)$p.value

    table_m[ ,i] <- c(treated_mean,
                      control_mean,
                      treated_sd,
                      control_sd,
                      diff_in_means,
                      SE_diff_in_means,
                      p_val
    )
  }
  table_m <- as.data.frame(table_m)
  table_m[] <- round(table_m, 3)

  return(table_m)
}
