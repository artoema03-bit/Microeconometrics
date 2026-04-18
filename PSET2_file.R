# R script for Problem Set 2
# Group composition: Emanuele Artoni (3199617), Pedro Cassandra (3387647), and Arturs Janis Nikitins (3342806)

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

# new file for pset2

library (haven)
library (dplyr)
library (ggplot2)
library(writexl)
library (fixest)

data <- read.csv("files/pset_2.csv", sep = ";",)

summary(data)

#Exercise 1 

# (a) 
# We must use analytic weights
# We do so because the dependent variable (divorce rate per 1,000 people) is a mean 
# computed from state-level populations (stpop) of different sizes.

# Larger states may provide estimates with lower variance, so weighting 
# by population accounts for heteroskedasticity and ensures that estimates 
# are representative of the U.S. population.

# Frequency weights are not relevant, since the divorce rates are state-year specific.
# Using them would imply div_rate would be observed as if each 
# observation were repeated stpop times

# Probability weights are necessary when when data come from a random 
# sample rather than the full population, this is not relevant in this scenario.

# (b) 
# (i)

data_1 <- data %>%
  mutate(
    TREATED = case_when(
      lfdivlaw >= 1968 & lfdivlaw <= 1988 ~ 1, lfdivlaw > 1988 | lfdivlaw == 2000 ~ 0, TRUE ~ NA_real_)) %>%
  filter(!is.na(TREATED))

data_1 <- data_1 %>%
  group_by (year, TREATED) %>%
  summarise (Y = weighted.mean (div_rate, w = stpop, na.rm = TRUE) ,
             .groups = "drop" )

data_differences_1 <- data_1 %>%
  pivot_wider(names_from= TREATED, values_from= Y) %>% 
  mutate(differences = `1` - `0`)

ggplot (data_1, aes (x = year , y = Y ,
                          color = factor (TREATED))) +
  geom_line()+
  geom_line (data = data_differences_1,
             aes(x = year, y = differences, color = "Difference"),
             inherit.aes = FALSE) +
  geom_vline ( xintercept = c(1968, 1988),  linetype = "dashed") +
  scale_color_manual(
    name = "Series",
    values = c("0" = "red", "1" = "blue", "Difference" = "black"),
    labels = c("0" = "Control Group", "1" = "Treated Group", "Difference" = "Difference (Treated - Control)")) +
  labs (y = "Divorce Rate per 1000 People",
        title = "Outcome Trends") + 
  theme_minimal()

# States that adopted the law prior to the 1968-1988 period, were removed from the
# sample as they were "always treated" not allowing them to be viable for comparison
# as members of the control group. 

# (ii)

data_2 <- data %>% 
  filter((lfdivlaw == 2000 | (lfdivlaw >= 1969 & lfdivlaw <= 1973)) & year <= 1978) %>%  
  mutate (year = as.numeric (year),
          TREATED = as.numeric(lfdivlaw >= 1969 & lfdivlaw <= 1973)) %>%
  group_by (year , TREATED) %>%
  summarise (Y = weighted.mean (div_rate, w = stpop, na.rm = TRUE) ,
             .groups = "drop" )

data_differences_2 <- data_2 %>% 
  pivot_wider(names_from = TREATED, values_from = Y) %>%
  mutate (differences = `1` - `0`)


ggplot (data_2, aes ( x = year , y = Y ,
                      color = factor (TREATED))) +
  geom_line () +
  geom_line (data = data_differences_2,
             aes(x = year, y = differences, color = "Difference"),
             inherit.aes = FALSE) +
  geom_vline ( xintercept = c(1968.5),  linetype = "dashed") +
  scale_color_manual(
    name = "Series",
    values = c("0" = "red", "1" = "blue", "Difference" = "black"),
    labels = c("0" = "Control Group", "1" = "Treated Group", "Difference" = "Difference (Treated - Control)")) +
  labs (y = "Divorce Rate per 1000 People",
        title = "Outcome Trends") + 
  theme_minimal()


# The graphs show broadly consistent support for the parallel trends assumption. 
# Concretely, despite consistently higher divorce rates, the evolution of treated states before the 
# reform appears to be very similar to  the control group. In particular, the difference
# between the two groups remains relatively stable—fluctuating around a roughly 
# constant level of about 1.3 (1.4) divorces per 1,000 people in the first graph 
# (in the second graph) prior to the reform period. This suggests that in the 
# absence of treatment, both groups would likelyhave followed parallel paths. 


# (c)

data_3 <- data %>%
  filter((lfdivlaw == 2000 | (lfdivlaw >= 1969 & lfdivlaw <= 1973)) & year %in% c(1968, 1978)) %>%
  mutate (year = as.numeric (year),
          UNILATERAL = as.numeric(lfdivlaw >= 1969 & lfdivlaw <= 1973),
          POST = as.numeric(year == 1978),
          POST_UNILATERAL = (POST*UNILATERAL))

# 
# (i)

regression_1 <- feols(div_rate ~ POST_UNILATERAL + POST, data = data_3, weights = data_3$stpop)
summary(regression_1)

# (ii)

did_1 <- feols(div_rate ~ factor (POST)*factor (UNILATERAL), data = data_3, weights = data_3$stpop)
summary (did_1)

#DiD by hand
#Need to Add weights 
#means <- aggregate(div_rate ~ UNILATERAL + POST, data_3, mean)
#did_2 <- ( means $div_rate[means$UNILATERAL==1 & means$POST ==1]   -
#           means$div_rate[means$UNILATERAL==1 & means$POST==0]) -
#  (means$div_rate[means$UNILATERAL==0 & means$POST ==1]  -
#      means$div_rate[means$UNILATERAL==0 & means $ POST==0])
# print (did_2)

# The pooled OLS regression (i) shows that treated states are associated with 
# 1.70 more divorces per 1,000 people compared to the control states. This is 
# statistically significant to the 0.1% level The full DiD specification, (ii), 
#  indicates that the interaction term POST×UNILATERAhas no statistically significant effect
# impact of the treatment on divorce rates. Including the UNILATERAL regressor,
# captures differences between treatment and control groups before the treatment
# which was not accounted for in the pooled OLS
# in question (i) leading to the differences between the two regressions.
# The earlier graphs support this idea, control groups and treatment groups 
# follow similar trends. Further, after reforms, both groups have an increase in 
# divorce rates per 1,000 people, and the gap between the two does not seem to
# widen. Instead, the two move in parallel, explaining the low, non-significant 
# DiD estimate in the second specification. This highlights that the estimated 
# coefficient which arose from the pooled OLS was biased because it did not consider
# baseline differences. 

# (d)

data_3 <- na.omit(data_3)

means2 <- data_3 %>%
  group_by(UNILATERAL, POST) %>%
  summarise(avg = weighted.mean(div_rate, w = stpop), .groups = "drop")

m00 <- means2$avg [means2$UNILATERAL==0 & means2$POST==0]
m01 <- means2$avg [means2$UNILATERAL==0 & means2$POST==1]
m10 <- means2$avg [means2$UNILATERAL==1 & means2$POST==0]
m11 <- means2$avg [means2$UNILATERAL==1 & means2$POST==1]

diff_treated <- m11 - m10
diff_control <- m01 - m00
diff_post <- m11 - m01
diff_pre <- m10 - m00
did_3 <- (m11 - m10) - (m01 - m00)

did_mat <- matrix (c(m11, m01, diff_post, m10, m00, diff_pre, diff_treated, diff_control, did_3),
                   nrow = 3, byrow = TRUE)
rownames(did_mat) <- c("POST=1", "POST=0", "Difference 1")
colnames(did_mat) <- c("UNILATERAL=1", "UNILATERAL=0", "Difference 2")

print (did_mat) 

did_mat_xlsx <- as.data.frame(did_mat)
did_mat_xlsx <- cbind(Group = rownames(did_mat_xlsx), did_mat_xlsx)

write_xlsx(did_mat_xlsx, "TABLE_1.xlsx")

# (e)

data_4 <- data %>% 
  mutate (year = as.numeric (year),
          IMP_UNILATERAL = as.numeric(year >= lfdivlaw))%>%
  filter(year>=1956 & year<=1988)

# (i)

regression_2 <- feols(div_rate ~ IMP_UNILATERAL | st + year,
                      cluster = "st", data = data_4, weights = data_4$stpop)

summary (regression_2)

# (ii)

regression_3 <- feols(div_rate ~ IMP_UNILATERAL | st + year +
                        st[year], cluster = "st", data = data_4, weights = data_4$stpop)

summary(regression_3)

# (iii)

data_4$year2 <- data_4$year^2 
regression_4 <- feols(div_rate ~IMP_UNILATERAL | st + year + st[year] + st[year2], 
                      cluster = "st", data=data_4, weights = data_4$stpop)
summary(regression_4)

# All regressions match with table A1 of appendix 

# Regression i), which assumes strict parallel trends between control and
# treatment groups, has a coefficient of -0.055 which is not statistically significant. 
# Regression ii) has a coefficient of 0.476568 which is significant at the 5% level. 
# Regression iii) has a coefficient of 0.334415 which is significant at the 5% level.
# Regressions (ii) and (iii) include state-specific linear and quadratic trends and  
# test whether the baseline results are robust to differential pre-existing 
# trajectories across states. If the parallel-trends assumption holds, the
# coefficient should be similar to the one estimated without including the 
# linear/quadratic trends. However, the differences in coefficients across 
# regressions, especially the first one against the latter two, suggests that treated 
# and control groups may have different state specific trends prior to treatment. This indicates
# that the baseline regression may be biased, and that part of the effect captured 
# in regressions ii and iii) may be derived not from the treatment effect but 
# from the inclusion of group-specific trends. 