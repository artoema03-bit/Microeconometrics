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

data <- read.csv("files/pset_2.csv", sep = ";",)

summary(data)
