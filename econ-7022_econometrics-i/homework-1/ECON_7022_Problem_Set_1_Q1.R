
rm(list = ls())

# 1) Coin Flips ----------------------------------------------------------------

# Setting the seed will make all results reproducible.
set.seed(1)

# a) ---------------------------------------------------------------------------

# First, we generate a binomial random variable with p=0.5.
# Then, we show its output.
# Then, we find how many heads we got in the first 10 coin flips.
coin.flips <- rbinom(n = 10, size = 1, prob = 0.5)
coin.flips
sum(coin.flips)

# b) ---------------------------------------------------------------------------

# Yes, I produce the binomial random variable, which is the number of heads 
# appearing in 10 flips of a coin, in the first line of code.

# Yes, the outcome of the random variable can be found in the output of the
# third line of code, which happens to be 6 heads for the seed I set.

# Yes, a sample can be found in the output of the second line of code.
# The vector of 0's and 1's is considered a sample.

# c) ---------------------------------------------------------------------------

# Setting the seed will make all results reproducible.
set.seed(1)

# Sampling 5000 random variables of the number of heads in 10 coin flips.
coin.flips.5000 <- rbinom(n = 5000, size = 10, prob = 0.5)

# Plotting a histogram of these random draws.
library(ggplot2)
#qplot(coin.flips.5000, geom = "histogram")
qplot(factor(coin.flips.5000), geom = "bar")

# Finding sample mean and sample variance.
mean(coin.flips.5000)
var(coin.flips.5000)

# For this seed, the sample mean is 4.987, and the sample variance is
# 2.611753. These match closely with what we expected: np = 5 and
# np(1-p) = 2.5, respectively.

