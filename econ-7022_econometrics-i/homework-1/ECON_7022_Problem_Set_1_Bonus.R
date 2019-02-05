
rm(lists = ls())

# Bonus ------------------------------------------------------------------------

# Guidance provided by
# http://blog.revolutionanalytics.com/2016/08/simulating-form-the-bivariate-normal-distribution-in-r-1.html.

# Let X have a mean of 1 and variance of 2.
# Let Y have a mean of 1 and a variance of 8.
# Let their correlation be 0.15.
# Let there be 12000 random samples taken.
N <- 12000
rho <- 0.15
mu1 <- 1
s1 <- 2
mu2 <- 1
s2 <- 8

mu <- c(mu1, mu2)
sigma <- matrix(c(s1 ^ 2, s1 * s2 * rho, s1 * s2 * rho, s2 ^ 2), 2)

library(MASS)
set.seed(1)
bvn <- mvrnorm(N, mu = mu, Sigma = sigma)
colnames(bvn) <- c("X", "Y")
bvn <- as.data.frame(bvn)
# Sample correlation
cor(bvn$X, bvn$Y)

# a. ---------------------------------------------------------------------------

# Calculating the sample means.
mean(bvn$X) # 0.979204
mean(bvn$Y) # 0.9341963

# b. ---------------------------------------------------------------------------

# Calculating the sample Var-Cov matrix.
cov(bvn)
#          X         Y
# X 3.963301  2.496767
# Y 2.496767 64.906114

# Alternate method:
#var(bvn)

# c. ---------------------------------------------------------------------------

# Sample Var-Cov matrix
cov(bvn)
#          X         Y
# X 3.963301  2.496767
# Y 2.496767 64.906114

# Actual Var-Cov matrix
sigma
#      [,1] [,2]
# [1,]  4.0  2.4
# [2,]  2.4 64.0

# These matrices are very similar as expected with so many samples taken.

# d. ---------------------------------------------------------------------------

# Displaying scatter plot and fitting a linear projection of Y on X.
#library(ggpmisc)
ggplot(bvn, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label..)),
               parse = TRUE)
