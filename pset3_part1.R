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
library(stargazer)
library(patchwork)
library(png)
library(grid)
library(gridExtra)



data <- read.csv("pset_3.csv", sep = ";", )
summary(data)

################################################################################
# Exercise 1
################################################################################



################################################################################
# (a)
################################################################################


rdplot(y = data$T, x = data$X, title = "T-X Discontinuity", x.label = "Running variable", y.label = "Treatment Variable")

# Sharp RD

################################################################################
# (b)
################################################################################

covariates <- c("hischshr1520m", "i89", "vshr_islam1994", "partycount", 
                "lpop1994", "merkezi", "merkezp", "subbuyuk", "buyuk"
)

labels <- c(
  hischshr1520m = "Share men aged 15–20 with high school education",
  i89 = "1989 Islamic Mayor Control",
  vshr_islam1994 = "Islamic vote share 1994",
  partycount = "Party count",
  lpop1994 = "Log population in 1994",
  merkezi = "1",
  merkezp = "2",
  subbuyuk = "3",
  buyuk = "4"
)


results <- sapply(covariates, function(var) {
  est <- rdrobust(y = data[[var]], x = data$X)
  c(tau = round(est$coef[1], 3),
    pval = round(est$pv[3], 3),
    h_l   = round(est$bws[1, 1], 3),
    h_r   = round(est$bws[1, 2], 3),
    eff_n = sum(est$N_h))})

balance <- t(results)
colnames(balance) <- c( "RD Effect", "Robust p-val", "MSE-Optimal Bandwidth (L)", "MSE-Optimal Bandwidth (R)", "Effective Number of Observations" )
print(balance)

stargazer(balance, type = "text", title="Table_1", digits=1, out="Table_1.txt")

# Change the labels

################################################################################
# (c)
################################################################################


graphs <- lapply(covariates, function(var) {rdplot(y = data[[var]], x = data$X, title = var,
                                                   x.label = "Running variable",
                                                   y.label = var)$rdplot})

graph_1 <- wrap_plots(graphs, ncol = 3)

ggsave("Graph_1.png", combined, width = 25, height = 25)


################################################################################
# (d)
################################################################################

X <- data$X

df_h <- data.frame (X = X,
                    side = ifelse (X >= 0 , "Above" , "Below"))

p1 <- ggplot(df_h , aes (x = X , fill = side)) +
  geom_histogram (bins = 20 , alpha = 0.6 ,
                  position = "identity" ) +
  geom_vline ( xintercept = 0, linetype = "dashed", linewidth = 1) +
  labs ( title = "Running Variable" ,
         x = "Margin" , y = "Count") +
  theme_minimal ()

print(p1)


density_est <- rddensity(data$X)
density_plot <- rdplotdensity(density_est, data$X, plotRange = c(h_l, h_r))

p2 <- density_plot$Estplot + ggplot2::geom_vline(xintercept = 0, linetype = "dashed")

Graph_2 <- p1 + p2 
