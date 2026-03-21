library(hdm)

# Set wd
user <- Sys.info()["user"]
output_dir <- switch(user,
  "ajnik"="G:/Mans disks/zObsidian/04 Courses/20295 Microeconometrics/Problem Sets/microeconometrics-ps",
  getwd()
)
setwd(output_dir)

# Common setup
source("./setup.R")

# Task 3

#a)

regression_lasso1 <- rlasso(re78 ~ age + educ + black + hisp + re74 + re75, data = jtrain2)

summary(regression_lasso1)

regression_postlasso <- lm(re78 ~ train, data = jtrain2)

summary(regression_postlasso)
# Interpretation needed

#b)
# (1)

# matrix of regressor variables

u <- as.matrix(jtrain2[, c("age", "educ", "black", "hisp", "re74", "re75")])

#double selection

regression_ds <- rlassoEffect(x = u, y = jtrain2$re78, d = jtrain2$train,
                                  method = "double selection" )

summary(regression_ds)

# Comments to be added:


#(2)
#Dummy Variable Creation
# 1. Age

for (i in 17:55) {
  jtrain2[[paste0("age_", i)]] <- ifelse(jtrain2$age == i, 1, 0)
}

# 2. Education

for (i in 3:16) {
  jtrain2[[paste0("educ_", i)]] <- ifelse(jtrain2$educ == i, 1, 0)
}


# OR

u2 <- model.matrix(~  factor(age)*factor(educ) + black + hisp + re74 + re75 - 1, data = jtrain2)

regression_ds2 <- rlassoEffect(x = u2, y = jtrain2$re78, d = jtrain2$train,
                              method = "double selection" )

summary(regression_ds2)



## INTERPERTATION NEEDED
