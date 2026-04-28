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
library(ggplot2)

data <- read.csv("pset_3.csv", sep = ";", )

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

# what are we able to conclude from such a test?

# is it favorable or against the validity of the rd design

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
    x = data$T,
    p = 1,
    kernel = k,
  )
})
names(rd_results) <- kernels

summary(rd_results$triangular)
summary(rd_results$uniform)


# Does electing a mayor from an Islamic party has a significant effect on the educational
# attainment of women? Do results differ significantly for different kernel choices?


################################################################################
# (i)
################################################################################

global_rd <- lm(
  Y ~ T + X + X^2 + X^3 + X^4 + T:X + T:X^2 + T:X^3 + T:X^4,
  data = data
)

summary(global_rd)