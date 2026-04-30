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
library(tidyverse)
library(rdrobust)
library(rddensity)

data <- read.csv("files/pset_3.csv", sep = ";", )

summary(data)

################################################################################
# (j)
################################################################################

local_rd_tri <- data %>%
  filter(abs(X) <= rd_optim_bw) %>%
  mutate(weight = 1 - abs(X) / rd_optim_bw) %>%
  fixest::feols(Y ~ T*X, data = ., weight = ~ weight)

summary(local_rd_tri)

all.equal(rd_results$triangular$coef[1], local_rd_tri$coefficients[[2]])

# The point estimate in this case is 3.02, which is exactly the same as the 3.02
# obtained in (h). This is because we use the same functional form, the same bandwiths
# and we recreate the triangular kernel weights used in (h).
# Differences arise for inference, where the linear regression estimate is significant
# at the 1%, wherease the (h) estimate was significant at a 5% level.
# The higher standard error of the estimate in (h) is due to the use by rdrobust
# of a heteroskedasticity-robust nearest neighbor variance estimator.

################################################################################
# (k)
################################################################################

# Varying bandwidths
bws <- c(0.5, 0.75, 1.25, 1.5) * rd_optim_bw

robust_bw_results <- lapply(bws, function(h) {
  est <- rdrobust(
    y = data$Y, x = data$X, h = c(h, h),
    rho = rd_optim_bw / rd_optim_bw_b,
    all = TRUE
  )
  c(h = h, tau = est$coef[1], ci_low = est$ci[1], ci_high = est$ci[4], n = sum(est$N_h))
})

robust_df <- bind_rows(robust_bw_results) %>%
  add_row(
    h = rd_optim_bw,
    tau = rd_results$triangular$coef[1],
    ci_low = rd_results$triangular$ci[1], ci_high = rd_results$triangular$ci[4],
    n = sum(rd_results$triangular$N_h)
  )

# Plot RD effects at alternative bandwidths
ggplot(robust_df, aes(x = h, y = tau)) +
  geom_point(color = "darkred") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  ylab("RD Treatment Effect") +
  xlab("Bandwidth")

# Plotting the RD point estimates for different bandwidth sizes, we can see that
# they are all positive. Also, the point estimates are generally aligned at almost
# 3, with the only exception being the estimate for the 0.5 bandwidth, which is only 1.8.
# Looking at their 95% CIs, the 0.5 and 0.75 bandwidth estimates are not statistically
# significant at the 5% level, while the 1.25 and 1.5 bandwidth estimates are statistically
# significant at the 5% level.
# This weaker significance at narrow bandwidths is consistent with lower precision,
# since the number of observations falls substantially as the bandwidth shrinks:
# from 795 for the optimal bandwidth to 622 and 422 for the 0.75 and 0.5 bandwidths, respectively.
# Overall, the result is qualitatively robust: the estimated effect remains positive
# across specifications, while inference becomes less precise for narrower bandwidths.
