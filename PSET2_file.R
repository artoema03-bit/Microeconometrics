# R script for Problem Set 2
# Group composition: Emanuele Artoni (3199617), Pedro Cassandra (3387647), and Arturs Janis Nikitins (3342806)

# Set wd
user <- Sys.info()["user"]
output_dir <- switch(user,
  "ajnik"="G:/Mans disks/zObsidian/04 Courses/20295 Microeconometrics/Problem Sets/microeconometrics-ps",
  "erick"="/home/erick/TEMP/",
  "pedrocassandra"="/Users/pedrocassandra/Desktop/Bocconi/Microeconometrics/Micro/Microeconometrics",
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
# We do so, because the  variable (divorce rate per 1,000 people) is a mean 
# computed from state-level populations (stpop) of different sizes.
# Larger states may provide estimates with lower variance, so weighting 
# by population accounts for heteroskedasticity and ensures that estimates 
# are representative of the U.S. population.

# Frequency weights are not relevant since the divorce rates are state-year specific
# so using frequency weight would imply div_rate is the observed at the corresponding
# stpop number of times, instead of being a mean.

# Probability weights, are necessary when random sampling is involved which rather
# than the full-population observation as we have in this case

# (b) 
# (i)

data_1 <- data %>% 
  mutate (year = as.numeric (year) ,
          TREATED = as.numeric(lfdivlaw >= 1968 & lfdivlaw <= 1988))

data_1 <- data_1 %>%
  group_by (year, TREATED) %>%
  summarise (Y = weighted.mean (div_rate, w = stpop, na.rm = TRUE) ,
             .groups = "drop" )

data_differences_1 <- data_coll_1 %>%
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


# Do your results support the assumption of parallel trends?
# The graphs support the parallel trends assumption. Concretely, despite consistent higher divorce rates,
# the evolution of treated states before the reform (for more than two periods) appears to be very similar to that 
# of the control group. This, suggests that in the absence of treatment, both groups would have likely followed parallel paths. 


# (c)

data_3 <- data %>%
  filter((lfdivlaw == 2000 | (lfdivlaw >= 1969 & lfdivlaw <= 1973)) & year %in% c(1968, 1978)) %>%
  mutate (year = as.numeric (year),
          UNILATERAL = as.numeric(lfdivlaw >= 1969 & lfdivlaw <= 1973),
          POST = as.numeric(year == 1978),
          POST_UNILATERAL = (POST*UNILATERAL))
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

#  The pooled OLS regression (i) shows that treated states are associated with 
#1.55 more divorces per 1,000 people compared to the control states. This is 
#statistically significant to the 0.1% level- The full DiD specification (ii), 
# shows the interaction term POSTxUNILATERAL as non-statistically significant 
# impact of the treatment on divorce rates. Including the UNILATERAL regressor,
# allows for the capturing of the differences between treatment and control
# groups before the treatment, which was not accounted for the in the pooled OLS
# in question (i) causing the difference between the two regressions.
# The graphs done before support this idea, control groups and treatment group 
# follow similar trends. Further, after reforms, both groups have an increase in 
# divorce rates per 1000 people, and the gap between the two does not seem to
# widen; Rather the two move in parallel, explaining the low, non-significant 
# DiD estimate in the second specification. Highligthing that the estimated 
# coefficient that arose from the pooled OLS was biased because it was not considering 
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

# Regression i), which assumes strict parallell trends between control and
# treatment groups has a coefficient of  -0.054838 which is not  statistically significant 
# Regression ii) has a coefficient of  0.476568 which is significant at the 5% level 
# Regression iii) has a coefficient of  0.334415  significant at 5% level
# The latter two allow for state-specific linear and quadratic trends and are 
# testing whether the baseline results are robust to differential pre-existing 
# trajectories across states. If the parallel-trends assumption holds, the
# coefficient should be similar as the one estimated without including the 
# linear/quadratic trends. However as noted the differences in coefficients across 
# regressions, especially the first one agains the latter two, suggests that treated 
# and control groups may have different trends priot to treatment.  This indicates
# that the baseline regression may be biased, and that part of the effect captured 
# in regressions ii and iii) may be derived not from the treatment effect but 
# from the inclusion of group-specific trends. 