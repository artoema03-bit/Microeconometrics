# R script for Problem Set 3
# Group 2: Emanuele Artoni (3199617), Pedro Cassandra (3387647), and Arturs Janis Nikitins (3342806)

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
library(stargazer)
library(patchwork)
library(fixest)


data <- read.csv("files/pset_3.csv", sep = ";", )
summary(data)

################################################################################
# Exercise 1
################################################################################

################################################################################
# (a)
################################################################################

rdplot(y = data$T, x = data$X, title = "T-X Discontinuity", x.label = "Running variable", y.label = "Treatment Variable")

ggsave("out/Q1_a.png")

# This is a sharp RD because treatment is a deterministic function of the running variable.

################################################################################
# (b)
################################################################################

covariates <- c("hischshr1520m", "i89", "vshr_islam1994", "partycount",
                "lpop1994", "merkezi", "merkezp", "subbuyuk", "buyuk")

labels <- c(
  hischshr1520m = "Share men aged 15–20 with high school education",
  i89 = "Islamic Mayor in 1989",
  vshr_islam1994 = "Islamic vote share 1994",
  partycount = "Number of parties receiving votes 1994",
  lpop1994 = "Log population in 1994",
  merkezi = "District center",
  merkezp = "Province center",
  subbuyuk = "Sub-metro center",
  buyuk = "Metro center"
)


results <- sapply(covariates, \(var) {
  est <- rdrobust(y = data[[var]], x = data$X)

  res <- list(
    label = labels[[var]],
    h   = round(est$bws[1, 1], 3),
    tau = round(est$coef[1], 3),
    pval = round(est$pv[1], 3),
    eff_n = sum(est$N_h)
  )
})

balance <- t(results)
colnames(balance) <- c("Label", "MSE-Optimal Bandwidth", "RD Estimator", "p-value", "Effective Number of Observations" )
balance

stargazer(balance, type = "text", title="Table_1", digits=1, out="out/Table_1.txt")

# Change the labels

################################################################################
# (c)
################################################################################

graphs <- lapply(covariates, function(var) {
  rdplot(
    y = data[[var]],
    x = data$X,
    title = labels[[var]],
    x.label = "Running variable",
    y.label = "Variable",
    p = 1,
    hide = TRUE
  )$rdplot
})

graph_1 <- wrap_plots(graphs, ncol = 3)

graph_1

ggsave("out/Graph_1.png", graph_1, width = 25, height = 25)

################################################################################
# (d)
################################################################################

df_h <- tibble(X = data$X, side = ifelse(X >= 0, "Above", "Below"))

p1 <- ggplot(df_h, aes(x = X, fill = side)) +
  geom_histogram(bins = 60, alpha = 0.6, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  labs(title = "Counts at Cutoff", x = "Running Variable", y = "Count") +
  theme_minimal()

# print(p1)

density_test <- rddensity(data$X)
density_plot <- rdplotdensity(density_test, data$X, noPlot = TRUE, title = "Density at Cutoff", xlabel = "Running Variable", ylabel = "Density")

p2 <- density_plot$Estplot + geom_vline(xintercept = 0, linetype = "dashed")

Graph_2 <- p1 + p2

Graph_2

ggsave("out/Graph_2.png", Graph_2, width = 25, height = 25)

################################################################################
# (e)
################################################################################

summary(density_test)

# Graphically, the histogram and density plot show that the running variable is
# much more concentrated on the left of the cutoff overall, but it does not show
# any noticeable bunching of the running variable near the cutoff.
# Likewise, testing whether the running variable X's density jumps at cutoff yields
# a t-statistic of -1.3937, with a p-value of 0.16. Thus, we fail to reject the
# hypothesis that density is continuous at cutoff.
# As a side note, the binomial tests, which check whether the number of observations
# just below and above the cutoff is balanced, are insignificant for the smallest
# windows but become significant for larger ones. This also indicates broader asymmetry
# in the running variable, but does not give evidence of a sharp discontinuity at
# the cutoff.
# Taken together, there is no strong evidence of precise manipulation of the running
# variable at the threshold, which supports the validity of the RD design.

################################################################################
# (f)
################################################################################

cutoff_list <- c(-10, -5, 5, 10)

placebo_density_tests <- map_dfr(cutoff_list, function(cut) {
  ttest_data <- if (cut < 0) subset(data, X < 0) else subset(data, X >= 0)

  ttest <- rdrobust(y = ttest_data$Y, x = ttest_data$X, c = cut)
  data.frame(
    cutoff = cut,
    bw = ttest$bws[1,1],
    coef = ttest$coef[1],
    p_value = ttest$pv[1]
  )
})

print(placebo_density_tests)

# No effect at any alternative cutoff is statistically significant, thus we fail
# to detect a jump in the outcome at alternative thresholds.
# This makes any treatment discontinuity at the 0 cutoff more credible.

################################################################################
# (g)
################################################################################

rdplot(
  y = data$Y,
  x = data$X,
  y.label = "Outcome",
  x.label = "Running variable",
  nbins = c(20, 20)
)

ggsave("out/Q1_g.png")

################################################################################
# (h)
################################################################################

kernels <- c("triangular", "uniform")

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

# The point estimate is 3.02 with a triangular kernel, and 3.202 with a uniform one;
# both are significant at the 5% level (conventional p-values 0.034 and 0.018, respectively),
# with similar confidence intervals. Thus, we can conclude that our results are
# robust to different kernel specifications.

# The positive, statistically significant coefficient allows us to argue in favor
# of the hypothesis that electing a mayor from an Islamic party does positively
# affect the educational attainment of women in municipalities with close elections
# around the cutoff.
# A possible mechanism, as discussed by Meyersson (2014), involves the Islamic party's
# greater ability to remove barriers to education and involvement in civil society
# for women from poor and religiously conservative backgrounds.

################################################################################
# (i)
################################################################################

global_rd <- feols(
  Y ~ T + X + X^2 + X^3 + X^4 + T:X + T:X^2 + T:X^3 + T:X^4,
  data = data
)

summary(global_rd)

# This global estimate should be interpreted cautiously, since high-order global
# polynomials can be sensitive to functional-form choices and give substantial weight
# to observations far from the cutoff.

################################################################################
# (j)
################################################################################

local_rd <- data %>%
  filter(abs(X) <= rd_optim_bw) %>%
  feols(Y ~ T*X, data = .)

summary(local_rd)

local_rd_w <- data %>%
  filter(abs(X) <= rd_optim_bw) %>%
  mutate(weight = 1 - abs(X) / rd_optim_bw) %>%
  feols(Y ~ T*X, data = ., weight = ~ weight)

summary(local_rd_w)

all.equal(rd_results$triangular$coef[1], local_rd_w$coefficients[[2]])

# Estimating a naive local linear regression results in a treatment effect estimate
# of 3.06, which is slightly different from the 3.02 obtained in (h). This is because
# while we use the same functional form and the same bandwiths, we do not use triangular
# kernel weights, used in (h).
# Differences also arise for inference, where the linear regression estimate has
# a slightly lower standard error than the (h) estimate (1.305 vs 1.427). This is
# due to the use by rdrobust of a heteroskedasticity-robust nearest neighbor variance
# estimator.
# If we restimate the local linear regression using kernel weights, we obtain the
# same 3.02 point estimate as in (h).

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

ggsave("out/Graph_3.png")

# Plotting the RD point estimates for different bandwidth sizes, we can see that
# they are all positive. Also, the point estimates are generally aligned at almost
# 3, with the only exception being the estimate for the 0.5 bandwidth, which is only 1.8.
# Looking at their 95% CIs, the 0.5 and 0.75 bandwidth estimates are not statistically
# significant at the 5% level, while the 1.25 and 1.5 bandwidth estimates are statistically
# significant at the 5% level.
# This weaker significance at narrow bandwidths is consistent with lower precision,
# since the number of observations falls substantially as the bandwidth shrinks:
# from 795 for the optimal bandwidth to 622 and 422 for the 0.75 and 0.5 bandwidths, respectively.
# Overall, the estimated effect remains positive across specifications, while inference
# becomes less precise for narrower bandwidths.
