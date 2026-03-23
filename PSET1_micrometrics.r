library(stargazer)

library(kableExtra)
library(knitr)

library(RCT)

# Set wd
user <- Sys.info()["user"]
output_dir <- switch(user,
  "ajnik"="G:/Mans disks/zObsidian/04 Courses/20295 Microeconometrics/Problem Sets/microeconometrics-ps",
  getwd()
)
setwd(output_dir)

# Common setup
source("./setup.R")

# Task 1

# a)
# Create table to check balance across treatment groups for some covariates
vars <- c("age", "educ", "black", "hisp", "nodegree", "re74", "re75")
datasummary_balance(~ train , fmt = 3, data = jtrain2[, c("train", vars)], dinm_statistic = "p.value")

# At a 5% significance level, only the difference in `nodegree` is statistically significant.
# At a 10% significance level, also the difference in `hisp` is statistically significant.
# At these significance levels we would expect to find only 1 in 20 and 1 in 10 significance differences, respectively, so there is some cause for concern on the sample being balanced.
# However, given that this a smaller subset of a randomized sample, these imbalances could have also arisen due to bad luck.

# b)
# regress re78 on train, save estimate and SE of the coefficient as scalars

regression <- lm(re78 ~ train, data = jtrain2)
summary(regression)

alpha <- coefficients(regression)["train"]
beta  <- summary(regression)$coefficients["train", "Std. Error"]

# The coefficient implies that participation in the training program is associated with an increase of 1.79 thousand 1982 dollars in 1978 real earnings , ceteris paribus.
# The coefficient is significant at a 1% significance level.

# c)
# create new table to test covariate sensitivity

regs <- list(
  regression_1 = lm(re78 ~ train, data = jtrain2),
  regression_2 = lm(re78  ~ train + age + educ + black + hisp, data = jtrain2),
  regression_3 = lm(re78  ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2)
)

get_group_counts <- \(model, treat = "train") {
  mf <- model.frame(model)
  if (!treat %in% names(mf)) stop("treatment variable not found in model frame")

  n1 <- sum(mf[[treat]] == 1, na.rm = TRUE)
  n0 <- sum(mf[[treat]] == 0, na.rm = TRUE)
  c(as.character(n1), as.character(n0))
}

rows <- data.frame(
  "term" = c("N Treated ", "N Control "),
  "Model 1" = get_group_counts(regs[[1]]),
  "Model 2" = get_group_counts(regs[[2]]),
  "Model 3" = get_group_counts(regs[[3]])
)

modelsummary(
  regs,
  add_rows = rows,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1c"
)

# Are your results sensitive to the introduction of covariates?
# The introduction of covariates slightly reduced the treatment coefficient from 1.79 to 1.68 thousand 1982 dollars, but it is pretty stable overall.
# Similarly, the SE marginally decreased, but remains essentially unchanged after adding the covariates.
# So, the statistical significance of the results remains intact.
# All of these point to the fact that our baseline estimate may not have been severy biased by ommitted variables.

# d)
# redoing reg_3 with dfbeta

inflm_reg <- influence.measures(regs[[3]]) # first subtask
influence_train <- inflm_reg$infmat[,"dfb.tran"] # no idea why its called that

# second subtask (rerun regression)
subsample_maker <- function( dc, x = influence_train) {
  if (!dc %in% c(3, 5, 10)) {
    stop("insert one of 3, 5, or 10")
  }
  highest <- order(x, decreasing = TRUE)[1:dc]
  lowest <- order(x)[1:dc]
  unique(c(highest, lowest))
}

t1 <- subsample_maker(dc=3)
t2 <- subsample_maker(dc=5)
t3 <- subsample_maker(dc=10)

regs_dfb <- list(
  reg_3_generic = regs[[3]],
  reg_3_drop3 = lm(re78 ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2[-t1,]),
  reg_3_drop5 = lm(re78 ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2[-t2,]),
  reg_3_drop10 = lm(re78 ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2[-t3,])
)

rows_dfb <- data.frame(
  "term" = c("N Treated ", "N Control "),
  "Model 1" = get_group_counts(regs_dfb[[1]]),
  "Model 2" = get_group_counts(regs_dfb[[2]]),
  "Model 3" = get_group_counts(regs_dfb[[3]]),
  "Model 4" = get_group_counts(regs_dfb[[4]])
)

modelsummary(
  regs_dfb,
  add_rows = rows_dfb,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  title = "Task 1d"
)

# The removal of the top/bottom 3, 5, and 10 influential observations reduced the treatment coefficient from 1.68 to 1.35 to 1.22 to 1.02 thousand 1982 dollars, respectively.
# This amounts to a 40% decrease from the baseline to the 10 observation case.
# Likewise, the SEs for the estimated was also reduced, but to a lesser extent than the treatment coefficients as the significance dropped to only the 5% significance level.

# Task 2

#summary(jtrain3)

# a)
# another table

Table_4 <- Table_original_function(jtrain3,
                                   vars = c("age","educ","black","hisp","re74","re75"),
                                   treat_var = "train"
)

stargazer(Table_4, summary = FALSE, type = "text", rownames = TRUE)

kable(Table_4, caption = "Table 4") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))

# b)
# new nonsense treatment (called treated)

set.seed(88888)

jtrain3$random <- runif(nrow(jtrain3))
jtrain3$random_order <- rank(jtrain3$random, ties.method = "first")

jtrain3$treated <- ifelse(jtrain3$random_order <= floor(nrow(jtrain3)/2), 0, 1)


# c)
# alternative treatment assignment method

jtrain3$key_id <- 1:nrow(jtrain3)

treated_2 <- treatment_assign(
  jtrain3,
  key = "key_id",
  strata_varlist = NULL,
  share_control = 0.5,
  n_t = 1,
  missfits = "global",
  seed = "888888",
)
# d)
# appending Table_1

Table_5 <- Table_original_function(jtrain3,
                                   vars = c("age","educ","black","hisp","re74","re75"),
                                   treat_var = "treated"
)
colnames(Table_5) <- paste0(colnames(Table_5), "_jtrain3")
Table_6 <- cbind(Table_1, Table_5)

stargazer(Table_6, summary = FALSE, type = "text", rownames = TRUE)

kable(Table_6, caption = "Table 6") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))

# COMMENT TO BE ADDED

# e)
# appending Table_2 using "treated" treatment variable
regression_e1 <- lm(re78  ~ treated, data = jtrain3)
regression_e2 <- lm(re78  ~ treated + age + educ + black + hisp, data = jtrain3)
regression_e3 <- lm(re78  ~ treated + age + educ + black + hisp + re74 + re75, data = jtrain3)

ee1 <- reg_extractor(regression_e1, treat = "treated")
ee2 <- reg_extractor(regression_e2, treat = "treated")
ee3 <- reg_extractor(regression_e3, treat = "treated")

TABLE_7_m <- matrix(NA, nrow = 5, ncol = 6)
rownames(TABLE_7_m) <- c(
  "Treated units",
  "Control units",
  "Treatment coefficient",
  "Standard error",
  "p-value"
)
colnames(TABLE_7_m) <- c(
  "Reg_1",
  "Reg_2",
  "Reg_3",
  "Reg_e1",
  "Reg_e2",
  "Reg_e3"
)

TABLE_7_m[ ,1] <- s1
TABLE_7_m[ ,2] <- s2
TABLE_7_m[ ,3] <- s3
TABLE_7_m[ ,4] <- ee1
TABLE_7_m[ ,5] <- ee2
TABLE_7_m[ ,6] <- ee3

TABLE_7 <- as.data.frame(TABLE_7_m)

# using stargazer as recommended by instructions
stargazer(TABLE_7, summary = FALSE, type = "text", rownames = TRUE)

kable(TABLE_7, caption = "Table 7") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))

# COMMENT TO BE ADDED
# comment: since the treated var we are using is nonsense, evidently the treated/control
# ratio is insane: it has no relation at all with the actual treatment

# f)
# appending Table_2 using "train" treatment variable

# Possible to use a function, but easier to copy-paste
regression_f1 <- lm(re78  ~ train, data = jtrain3)
regression_f2 <- lm(re78  ~ train + age + educ + black + hisp, data = jtrain3)
regression_f3 <- lm(re78  ~ train + age + educ + black + hisp + re74 + re75, data = jtrain3)

ff1 <- reg_extractor(regression_f1, treat = "train")
ff2 <- reg_extractor(regression_f2, treat = "train")
ff3 <- reg_extractor(regression_f3, treat = "train")

summary(regression_f1)
summary(regression_f2)
summary(regression_f3)

TABLE_8_m <- matrix(NA, nrow = 5, ncol = 6)
rownames(TABLE_8_m) <- c(
  "Treated units",
  "Control units",
  "Treatment coefficient",
  "Standard error",
  "p-value"
)
colnames(TABLE_8_m) <- c(
  "Reg_1",
  "Reg_2",
  "Reg_3",
  "Reg_ff1",
  "Reg_ff2",
  "Reg_ff3"
)

TABLE_8_m[ ,1] <- s1
TABLE_8_m[ ,2] <- s2
TABLE_8_m[ ,3] <- s3
TABLE_8_m[ ,4] <- ff1
TABLE_8_m[ ,5] <- ff2
TABLE_8_m[ ,6] <- ff3

TABLE_8 <- as.data.frame(TABLE_8_m)

# using stargazer as recommended by instructions
stargazer(TABLE_8, summary = FALSE, type = "text", rownames = TRUE)

kable(TABLE_8, caption = "Table 8") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))


# COMMENT TO BE ADDED
