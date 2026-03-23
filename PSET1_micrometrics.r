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

# Comment:
# How many variables are balanced? Are you surprised?
# All variables are more or less balanced. The least so are 'nodegree', as there is a
# 13 p.p. difference across the two treatment groups, and 'hisp'.
# As for 'nodegree', the difference, given the standard error of 0.041, is clearly
# statistically significant. It is possible for it to be due to random imbalance, and
# the nature of the experimental design prevents it from being a concern from the
# standpoint of validity.
# Given the limited representativeness of hispanics in the sampled population,
# it is unsurprising that the smaller control group halves their numbers. This also
# does not constitute a validity concern with respect to the randomization procedure.

# b)
# regress re78 on train, save estimate and SE of the coefficient as scalars

regression <- lm(re78 ~ train, data = jtrain2)

summary(regression)

alpha <- coefficients(regression)["train"]
alpha_se  <- summary(regression)$coefficients["train", "Std. Error"]

#alpha
alpha_se

# Comment:
# Interpret the coefficient
# The coefficient from regressing real earnings (1978) on the treatment variable is
# 1.794, with associated standard error equal to 0.633, ie it is statistically
# significant
# This means that being randomly assigned to the treatment group
# increases a participant's real earnings by $1,794 by the year 1978.
# Naturally, it is important to note that the regression does not include any controls,
# so that, while the validity of the result is not in question, its precision
# is not certain.

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

# Comment:
# Are your results sensitive to the introduction of covariates?
# The introduction of covariates slightly reduces the treatment effect, but it is
# stable overall.
# Similarly, the SE remains essentially unchanged after adding the covariates.
# Furthermore the significance remains intact.
#All of these aspects confirms the hypothesis underlying the initial RCT that
# the baseline estimate is not severely biased by omitted variables

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

# Comment
# We have constructed a table (TABLE_3) for clarity and presentation purposes.
# Evidently, the value of the treatment coefficient changes markedly as the most
# extreme and influential observations are plucked out. Given the baseline coefficient
# of 1.68 that derives from the regression that includes controls, dropping as few as
# 20 observations causes a reduction to 1.02, amounting to a loss of $658 in estimated
# earnings impact.
# We can also note that, as the sample size shrinks,the standard errors also decrease,
# as expected. This helps p-values staying low.
# In conclusion, while the loss does not impinge on the statistical significance of
# the results, we can safely state that the regression results are
# indeed sensitive to influential observations.


kable(Table_1, caption = "Table 1") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))
kable(TABLE_2, caption = "Table 2") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))
kable(TABLE_3, caption = "Table 3") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))

# Task 2

#summary(jtrain3)

# a)
# another table

Table_new <- Table_original_function(jtrain3,
                                   vars = c("age","educ","black","hisp","re74","re75"),
                                   treat_var = "train"
)

colnames(Table_new) <- paste0(colnames(Table_new), "_jtrain3")
Extended_Table_1 <- cbind(Table_1, Table_new)


stargazer(Table_4, summary = FALSE, type = "text", rownames = TRUE)

kable(Table_4, caption = "Table 4") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))

# Comment
# Table four includes, side by side, the contents of Table 1 and the
# data from running the same analysis on jtrain3.
# Note that all jtrain3 covariates are less balanced than the ones in the RCT sample.
# For example, there is a 9 years difference in the average age of units across treatment
# groups; blacks still constitute 85% of the treatment group, though the new control group
# has only 25% black units.
# The new control units also tend to have much higher baseline real earnings in
# years 1974 and 1975 (roughly $19,000 compared to $1,267-$2,107). This is especially stark,
# suggesting that the treated and control populations are fundamentally different.
# For this reason, we can state that jtrain3's control group is far less balanced than
# the RCT control group, as non-experimental controls look nothing like the treated group
# on observables and probably unobservable, too.

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

jtrain3$treated_2 <- treated_2$data[,3]

# Comment:
# Is the correlation between 'treated' and 'treated_2' statistically significant?
cor(jtrain3$treated, jtrain3$treated_2)
# The correlation is -0.0228, which is unsurprisingly low, given that both treatments
# are assigned randomly and independently of both data and each other.

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

# Comment
# What do you find corresponds to your expectations?
# The problem with the jtrain3 dataset, as stated earlier, is (i) that
# the non-experimental, jtrain3 control group has no relation with the units
# in the control group, and (ii) that the original treatment ( variable 'train')
# is not randomly assigned.
# By randomly re-assigning treatment to the jtrain3 dataset, we obtain, as
# expected, a much more balanced set of covariates.
# For example, the age gap, the ethnicity composition gap and the real earnings
# gap in both baseline years are now gone (see table).
# The downside, naturally, is that the new treatment group has no economic interpretation,
# it is merely a coin-flip.

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

# Comment:
# Comment on what you find. Is it what you expected?
# The treated/control ratio has no relation at all with the actual treatment;
# the coefficients obtained by the new regressions are, expectedly, statistically
# insignificant, as can be surmised by the high p-values.

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


# Comment:
# Comment on what you find. Is it what you expected? Are your results sensitive
# to the introduction of covariates?
# The interpretation of each new regression is interesting:
# Regressing re78 on train (Reg_ff1) yields a strongly negative coefficient. This is
# entirely a byproduct of the fact that individuals in the jtrain3 control group
# earn significantly more than those in the original RCT treatment group. Then, there
# is no causal content, though the p-value indicates strong significance.
# Regressing re78 on treatment, given the first set of controls (Reg_ff2)
# does not eliminate the bias.
# Further adding re74 and re75 further reduces the bias, but the analysis is still
# completely unable to recover the RCT coefficient (1.794). Also note that the
# associated p-value is 0.8.
# In conclusion, the non-experimental dataset is unable to recover the original
# effects, and incapable of offering causal interpretations of any measure.
