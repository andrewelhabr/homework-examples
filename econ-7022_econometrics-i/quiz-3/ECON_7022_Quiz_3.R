
rm(list = ls())
library(ggplot2)

# I have chosen to sample from Poisson, normal, and chi-squared distributions.

# Poisson Distribution ---------------------------------------------------------

# Setting the seed will make all results reproducible.
set.seed(1)

# Generating 5000 Poisson random variables, each with lambda = 5.
draw_pois <- rpois(n = 5000, lambda = 5)
# The expectation and variance of each random variable is 5.

# Each entry in the draw_pois is an individual outcome, and the whole vector is
# a sample.
draw_pois

# Plotting the distribution of the simulated data.
qplot(factor(draw_pois), geom = "bar")

# Finding sample mean and sample variance.
mean(draw_pois)
# 5.025
var(draw_pois)
# 4.993374
# Both of these are very close to their theoretical values as expected since we
# are taking a large number of samples.



# Normal Distribution ----------------------------------------------------------

# Setting the seed will make all results reproducible.
set.seed(1)

# Generating 5000 normally distributed random variables, each with mean = 1 and 
# standard deviation = 3 (variance = 9)
draw_norm <- rnorm(n = 5000, mean = 1, sd = 3)
# For each random variable, the expectation is 1 and variance is 9.

# Each entry in the draw_norm is an individual outcome, and the whole vector is
# a sample.
draw_norm

# Plotting the distribution of the simulated data.
qplot(draw_norm, geom = "auto")

# Finding sample mean and sample variance.
mean(draw_norm)
# 0.9904346
var(draw_norm)
# 9.486896
# Both of these are very close to their theoretical values as expected since we
# are taking a large number of samples.



# Chi-squared Distribution -----------------------------------------------------

# Setting the seed will make all results reproducible.
set.seed(1)

# Generating 5000 chi-squared random variables, each with 20 degrees of freedom.
draw_chisq <- rchisq(n = 5000, df = 20)
# For each random variable, the expectation is 20 and variance is 40.

# Each entry in the draw_chisq is an individual outcome, and the whole vector is
# a sample.
draw_chisq

# Plotting the distribution of the simulated data.
qplot(draw_chisq, geom = "auto")

# Finding sample mean and sample variance.
mean(draw_chisq)
# 20.09426
var(draw_chisq)
# 40.45195
# Both of these are very close to their theoretical values as expected since we
# are taking a large number of samples.


