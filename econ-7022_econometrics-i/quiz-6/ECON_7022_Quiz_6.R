
##### ECON 7022 Quiz 6 #########################################################

rm(list = ls())

# Generating 3-dimensional multivariate random normal distribution. I had to
# change the covariance between schooling and intelligence from 1.00 to 0.50
# in order to make the sigma matrix positive definite.
n <- 5000
mus <- c(0, 0, 0)
sigmas <- 
  matrix(c(
    1.00, 0.79, 0.04,
    0.79, 1.00, 0.50,
    0.04, 0.50, 1.00
  ), ncol = 3)

library(MASS)
library(dplyr)
library(tibble)

set.seed(420711)
m <- mvrnorm(n, mu = mus, Sigma = sigmas, empirical = FALSE) %>% as_tibble()
colnames(m) <- c("earn", "school", "intel")
head(m)
summary(m)

# Computing sample variances.
sapply(sapply(m, var), round, 4)

# Computing sample covariances.
round(cov(m), 4)

# Estimation of the real model.
lm_1 <- lm(earn ~ school + intel, data = m)
summary(lm_1)
beta_hat_1_real <- coef(lm_1)
beta_hat_1_real[2]
## 1.033796 

# Estimation of the truncated model.
lm_2 <- lm(earn ~ school, data = m)
summary(lm_2)
beta_hat_1_trunc <- coef(lm_2)
beta_hat_1_trunc[2]
## 0.7899677 

# We can see that the biased coefficient is much smaller than its real estimate
# using the whole model, which is actually undervalued here since I had to
# change the covariance matrix. With my altered covariance matrix, I found a
# bias of -0.243828, which is about half of the theoretical bias I calculated.
# This makes sense since I halved the covariance between intelligence and
# schooling.

