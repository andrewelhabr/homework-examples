
###### Empirical Problems ######################################################

rm(list = ls())

#knitr::opts_chunk$set(
#  echo = FALSE,
#  # results = "hold",
#  warning = FALSE,
#  message = FALSE
#)	

###### Question E1: Ommited Variables ------------------------------------------

# 1 ----------------------------------------------------------------------------

# Generating 5-dimensional multivariate random normal distribution.
n <- 5347
mus <- c(0, 0, 0, 0, 0)
sigmas <- 
  matrix(c(
    1.00, 0.20, 0.10, 0.35, 0.00,
    0.20, 1.00, 0.00, 0.40, 0.00,
    0.10, 0.00, 1.00, 0.00, 0.40,
    0.35, 0.40, 0.00, 1.00, 0.60,
    0.00, 0.00, 0.40, 0.60, 1.00
  ), ncol = 5)

library(MASS)
library(dplyr)
library(tibble)

set.seed(420711)
m_0 <- mvrnorm(n, mu = mus, Sigma = sigmas, empirical = FALSE) %>% as_tibble()
m <- cbind(m_0, rep(1, n))
colnames(m) <- c("Y", "x1", "x2", "z1", "z2", "one")
head(m)
summary(m)

# Computing sample variances.
sapply(sapply(m, var), round, 4)
#diag(cov(m))

# Computing sample covariances.
round(cov(m), 4)
##          Y      x1      x2     z1     z2 one
## Y   1.0262  0.2015  0.1012 0.3705 0.0141   0
## x1  0.2015  1.0251 -0.0017 0.4203 0.0295   0
## x2  0.1012 -0.0017  0.9801 0.0079 0.3876   0
## z1  0.3705  0.4203  0.0079 1.0338 0.6299   0
## z2  0.0141  0.0295  0.3876 0.6299 1.0065   0
## one 0.0000  0.0000  0.0000 0.0000 0.0000   0

# All of the following is extra.
# Compute correlations.
round(cor(m), 4)

# Evaluating correlation significance (with p-values).
library("Hmisc")
rcorr(as.matrix(m))

# Visualize correlations.
library("GGally")
ggcorr(m, label = TRUE, label_round = 4)

# 2: Univariate Regression -----------------------------------------------------

# a)
# Finding regression coefficient by regressing Y on only x1 by using only
# variances and covariances.
beta_hat_1_v1 <- cov(m)[1,2]/var(m[,2])
beta_hat_1_v1
## 0.1965699

# b)
# Finding regression coefficient by regressing Y on only x1 by using R's lm
# function.
lm_x1 <- lm(Y ~ x1, data = m)
beta_hat_1_v2 <- coef(lm_x1)
beta_hat_1_v2[2]
## 0.1965699

# c)
# The source of the bias is the ommitted variables. Note the the regression
# coefficient for x1 is different when all variables are included in the linear
# model.
lm_all <- lm(Y ~ ., data = m)
beta_hat_1_v3 <- coef(lm_all)[2]
beta_hat_1_v3
#-0.08998593

# Two critera must be met to identify omitted variable bias in a linear
# regression model:
# 1) The regression coefficient for the omitted variable is non-zero when it is
#    included in the model.
# 2) The omitted variable has a non-zero correlation with at least one other
#    independent variable.
# In this case, one would consider x2, Z1, and Z2 as possible sources of omitted
# variable bias (since none appear in the x1-only model). Each meets the first
# critiria for having a non-zero coeffiicent value when included in the linear
# regression model. For the second critiera, it is evident that there is a
# non-trivial correlation between x1 and Z1 (although x2 and Z2 have a trivial
# correlations with x1). This can be seen with the following.
library("stargazer")
lm_x1omitted <- lm(Y ~ . - x1, data = m)
stargazer(lm_x1, lm_all, lm_x1omitted, type = "text", omit.stat = c("f", "ser"),
          no.space = TRUE)
cor.test(x = m$x1, y = m$x2)
cor.test(x = m$x1, y = m$z1)
cor.test(x = m$x1, y = m$z2)

# This can also be seen by examining the covariance matrix again.
library("Hmisc")
rcorr(as.matrix(m))

# d)
# Performing various regressions.
cols_expand <- c("Y", "x1", "z1")
vars_grid <-
  expand.grid(cols_expand, cols_expand, stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  filter(Var1 != Var2) %>% 
  filter(Var2 != "Y")
vars_grid

library("stringr")
fmlas <- 
  vars_grid %>% 
  mutate(fmla = str_c(Var1, " ~ ", Var2)) %>% 
  pull(fmla)
fmlas
fmlas <- c(fmlas, "Y ~ x1 + z1")
fmlas

models <- vector(mode = "list", length = length(fmlas))
for (i in 1:length(fmlas)) {
  fmla <- fmlas[i]
  models[[i]] <- lm(fmla, data = m)
}
names(models) <- 
  fmlas %>% 
  str_replace_all("~|\\+|[0]", "_") %>% 
  str_replace_all("\\s", "")
names(models)

models$Y_x1_x2
lapply(models, coef)

stargazer(models, type = "text", omit.stat = c("f", "ser"), no.space = TRUE)

# Computing the theoretical bias of the regression coefficient of x1 that we
# previously computed.
coef(models$Y_x1_z1)["z1"]
coef(models$z1_x1)["x1"]
theoretical_bias <- coef(models$Y_x1_z1)["z1"]*coef(models$z1_x1)["x1"]
theoretical_bias
## 0.1369951

# Checking that our better coefficient plus the bias equals the old coefficient.
coef(models$Y_x1_z1)["x1"] + theoretical_bias

# 3: Multivariate Regression on Foot -------------------------------------------

# a)
# Defining the design matrix X.
X <- m %>% dplyr::select(-Y) %>% as.matrix()
head(X)

# b)
# Computing X'X.
Xs <- t(X) %*% X
Xs
#             x1          x2         z1         z2        one
#x1  5480.750039   -8.513482 2246.49825  158.02918  -43.19759
#x2    -8.513482 5241.288215   41.13015 2072.33144  -97.19133
#z1  2246.498251   41.130149 5527.80842 3367.04239   72.41132
#z2   158.029179 2072.331435 3367.04239 5381.04529  -16.86535
#one  -43.197590  -97.191333   72.41132  -16.86535 5347.00000

# c)
# Computing beta_hat
Y <- m %>% pull(Y)
beta_hat <- solve(Xs) %*% t(X) %*% Y
beta_hat
#           [,1]
#x1  -0.08998593
#x2   0.32292979
#z1   0.74033856
#z2  -0.57101139
#one  0.01476238

# d)
# Comparing calculated beta_hat with R's lm output.
lm_all <- lm(Y ~ ., data = m)
coef(lm_all)

# Note that the two variables to be binded are of different types.
class(coef(lm_all))
class(beta_hat)

# Use as.numeric() to convert b_hat.
coef_compare <- bind_cols(lm_all = coef(lm_all)[c(2,3,4,5,1)],
                          beta_hat = as.numeric(beta_hat))
coef_compare

setdiff(round(coef_compare$lm_all, 5), round(coef_compare$beta_hat, 6))
round(coef_compare$lm_all - coef_compare$beta_hat, 6)
# The regression coefficeints are clearly the same.

# e)
# Comparing the univariate and multivariate regressions.
stargazer(lm_x1, lm_all, type = "text", omit.stat = c("f", "ser"),
          no.space = TRUE)
# The regression coefficeints are clearly different.

# 4: Compute the CEF for Y|x_1 -------------------------------------------------

# a)
# Visualizing.
library("ggplot2")
library("ggpmisc")
# Joint distribution of x1 and Y from the lm_x1 model.
lm_x1$model %>% 
  ggplot(aes(x = x1, y = Y)) + 
  geom_point() +
  geom_density2d() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
    parse = TRUE
  ) +
  theme_minimal()

# b)
# For points 2, 3, and 4, the residual values are positive. For the fitted
# model, the omission bias is positive because the correlation between Y and x1
# is positive and the coefficient for x1 is positive. The positive residuals on
# these points indicate that the positive bias in the model has caused the
# model to miss predict these values in a given direction.
head(lm_x1$model$Y)
head(lm_x1$fitted.values)
head(lm_x1$residuals)
length(lm_x1$residuals[lm_x1$residuals > 0])
length(lm_x1$residuals[lm_x1$residuals < 0])

##### Question E2: IV for Starters ---------------------------------------------

# 1 ----------------------------------------------------------------------------

# Creating linear models for truncated and full model.
lm_x1 <- lm(Y ~ x1, data = m)
lm_x1andx2 <- lm(Y ~ x1 + x2, data = m)
coef(lm_x1andx2)
coef(lm_x1)
# Regression coefficients are similar for beta0 and beta1, likely due to the
# lack of correlation between these two variables. The omitted variables that
# are significant for the truncated model are still significant for the full
# model.

round(cov(m), 4)
