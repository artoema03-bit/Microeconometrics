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
Table_1 <- Table_original_function(jtrain2,
                        vars = c("age","educ","black","hisp","nodegree","re74","re75"),
                        treat_var = "train"
                        )

# using stargazer as recommended by instructions
# I am not including this line in the function itself to adapt it to queries included
# in Task 2.
stargazer(Table_1, summary = FALSE, type = "text", rownames = TRUE)

# b)
# regress re78 on train, save estimate and SE of the coefficient as scalars

regression <- lm(re78 ~ train, data = jtrain2)

summary(regression)

alpha <- coefficients(regression)["train"]
beta  <- summary(regression)$coefficients["train", "Std. Error"]

# c)
# create new table to test covariate sensitivity

TABLE_2_m <- matrix(NA, nrow = 5, ncol = 3)
rownames(TABLE_2_m) <- c(
  "Treated units",
  "Control units",
  "Treatment coefficient",
  "Standard error",
  "p-value"
)
colnames(TABLE_2_m) <- c(
  "Reg_1",
  "Reg_2",
  "Reg_3"
)

regression_2 <- lm(re78  ~ train + age + educ + black + hisp, data = jtrain2)
regression_3 <- lm(re78  ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2)

s1 <- reg_extractor(regression)
s2 <- reg_extractor(regression_2)
s3 <- reg_extractor(regression_3)

TABLE_2_m[ ,1] <- s1
TABLE_2_m[ ,2] <- s2
TABLE_2_m[ ,3] <- s3

TABLE_2 <- as.data.frame(TABLE_2_m)
# using stargazer as recommended by instructions
stargazer(TABLE_2, summary = FALSE, type = "text", rownames = TRUE)

# Are your results sensitive to the introduction of covariates?
# The introduction of covariates slightly reduces the treatment effect, but it is
#pretty stable overall. Similarly, the SE remains essentially unchanged after
# adding the covariates. Furthermore the significance remains intact. All of these
# point to the fact that our baseline estimate was not severy biased by ommitted variables

# d)
# redoing reg_3 with dfbeta

inflm_reg <- influence.measures(regression_3) # first subtask
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


reg_3_1 <- lm(re78 ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2[-t1,])
reg_3_2 <- lm(re78 ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2[-t2,])
reg_3_3 <- lm(re78 ~ train + age + educ + black + hisp + re74 + re75, data = jtrain2[-t3,])


# building table (for presentation purposes)
TABLE_3_m <- matrix(NA, nrow = 5, ncol = 4)
rownames(TABLE_3_m) <- c(
  "Treated units",
  "Control units",
  "Treatment coefficient",
  "Standard error",
  "p-value"
)
colnames(TABLE_3_m) <- c(
  "Reg_3_generic",
  "Reg_3_drop3",
  "Reg_3_drop5",
  "Reg_3_drop10"
)

s4 <- reg_extractor(reg_3_1)
s5 <- reg_extractor(reg_3_2)
s6 <- reg_extractor(reg_3_3)
TABLE_3_m[ ,1] <- s3
TABLE_3_m[ ,2] <- s4
TABLE_3_m[ ,3] <- s5
TABLE_3_m[ ,4] <- s6

TABLE_3 <- as.data.frame(TABLE_3_m)
TABLE_3[] <- round(TABLE_3, 3)

# using stargazer as recommended by instructions
stargazer(TABLE_3, summary = FALSE, type = "text", rownames = TRUE)


kable(Table_1, caption = "Table 1") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))
kable(TABLE_2, caption = "Table 2") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))
kable(TABLE_3, caption = "Table 3") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))

# we're gonna wanna use latex (i think). then, we have the following latex-transformed output
# to do that, just change type = "latex" in the stargazer function argument, and
# copy-paste the output in latex


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
