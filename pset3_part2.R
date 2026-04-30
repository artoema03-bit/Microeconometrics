# R script for Problem Set 3
# Group composition: Emanuele Artoni (3199617), Pedro Cassandra (3387647), and Arturs Janis Nikitins (3342806)

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
library(rdrobust)
library(rddensity)
library(readstata13)
library(haven)

data <- read.csv("files/pset_3.csv", sep = ";", )

summary(data)
################################################################################
# Exercise 1
################################################################################

################################################################################
# (e)
################################################################################

density_test <- rddensity(
  X = data$X,
)
summary(density_test)

# Useful graphical visualization
rdplotdensity(
  rdd = density_test,
  X = data$X,
  type = "both"
)

# What are we able to conclude from such a test?
# Is it favorable or against the validity of the rd design?

# Testing whether the running variable X's density jumps at cutoff yields a t-statistic
# T = -1.3937, with p-value = 0.16. Then, we fail to reject the hypothesis that density
# is continuous at cutoff.
# It is also possible to note that binomial tests become significant as the window length
# increases, a result consistent with the observation that the there are many more
# observations to the left of c ((2314 vs 315, also visible from the histogram), i.e.
# the relevant party is a minor, local force that only wins a minority of elections.
# Graphically, the rdplotdensity figure also fails to detect any “bunching” of electoral wins
# right above the win margin cutoff.

# Together, these results are supportive of the RDD's credibility.

################################################################################
# (f)
################################################################################

cutoff_list <- c(-10, -5, 5, 10)

placebo_density_tests <- do.call(rbind, lapply(cutoff_list, function(cut) {
  ttest <- rddensity(X = data$X, c = cut)
  data.frame(
    cutoff = cut,
    t_stat = ttest$test["t_jk"],
    p_value = ttest$test["p_jk"]
  )
}))

print(placebo_density_tests)

# Did we find any evidence in favor of the absence of alternative discontinuities?

# As no t-statistic is significant at the 5% level, we again fail to detect any jump by the
# running variable in places where it shouldn't, making any treatment discontinuity at cutoff
# more credible.

################################################################################
# (g)
################################################################################

rdplot(y = data$Y,
       x = data$X,
       y.label = "Outcome",
       x.label = "Running variable",
       nbins = c(20,20),
       binselect = "es"
       )

################################################################################
# (h)
################################################################################

kernels = c("triangular", "uniform")

rd_results <- lapply(kernels, function (k) {
  rdrobust(
    y = data$Y,
    x = data$X,
    p = 1,
    kernel = k,
    all = TRUE
  )
}) %>%
  setNames(kernels)

summary(rd_results$triangular)
summary(rd_results$uniform)

rd_optim_bw <- rd_results$triangular$bws[1,1]
rd_optim_bw_b <- rd_results$triangular$bws[2,1]

# Does electing a mayor from an Islamic party has a significant effect on the educational
# attainment of women? Do results differ significantly for different kernel choices?

# The point estimate is 3.02 with a triangular kernel, and 3.202 with a uniform one; both are
# significant at the 5% level, with similar confidence intervals. Thus, we can
# conclude that our results are robust to different kernel specifications.

# The positive, statistically significant coefficient allows us to argue in favor of the
# hypothesis that electing a mayor from an Islamic party does positively affect the educational
# attainment of women. A possible mechanism, as discussed by Meyersson (2014), seems to involve
# the Islamic party's greater ability to remove barriers to education and involvement in
# civil society for women from poor and religiously conservative backgrounds.

################################################################################
# (i)
################################################################################

global_rd <- lm(
  Y ~ T + X + X^2 + X^3 + X^4 + T:X + T:X^2 + T:X^3 + T:X^4,
  data = data
)

summary(global_rd)
