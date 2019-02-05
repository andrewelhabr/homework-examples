#' ---
#' title: "Part IV: Learning and Coding Exercises"
#' author: "Andrew"
#' output:
#'  html_document:
#'    toc: true
#'    keep_md: true
#'    theme: united
#'    highligh: tango
#' ---
#'
#'
#'
#+ global_options, include = FALSE
knitr::opts_chunk$set(
  echo = TRUE,
  cache = FALSE,
  fig.show = "hide",
  fig.align = "center",
  warning = FALSE,
  message = FALSE
)

#'
#'
#'
# Packages. ----
library("dplyr")
library("stringr")
library("tidyr")
# library("readr")
library("ggplot2")
# library("lubridate")
theme_set(theme_minimal())
# theme_set(hrbrthemes::theme_ipsum_rc())

# For printing pretty tables.
# printr package simply converts any data frame to a nice looking format.
library("printr")
library("stargazer")

#'
#'
#+ include = FALSE
# P1a. ----

#'
#'
#' # Question P1a: Job Training in R
#'


#' ### 1) The population of interest is married people, and preferably not married people who are very old. Data needs to be collected in a location and environment in which people are unafraid to admit to having an affair. I would suggest packaging a survey that gaurantees a small financial incentive with a timeshare vacation advertisement. This is because I believe that timeshare vacations are usually taken by married couples who may have kids, and there is not very much selection bias in who would take these types of surveys. Of course, we might see that richer people would respond to the survey more than poor people since richer people tend to be able to afford a timeshare vacation more easily, but there is no obvious reason to me to believe that rich people have more or less affairs than poor people. Of course, this would need to be confirmed through some outside research.
#' 
#'
#'
load("affairs.RData")

#'
#'
#'
#+ include = FALSE
attributes(data)
attr(data, "var.labels")

#' 
#' ## 2a) A Summary Table
#'
#+ include = FALSE
# This does essentially the same thing as the stargazer() function.
data %>%
  as_tibble() %>%
  summarise_all(funs(
    mean,
    sd,
    min,
    max,
    q1 = quantile(., 0.25),
    q3 = quantile(., 0.75)
  )) %>%
  gather(colname_stat, value) %>%
  separate(colname_stat, c("colname", "stat")) %>%
  # mutate(value = ifelse(value > 0, value, round(value, 2))) %>%
  spread(stat, value) %>%
  # mutate_if(is.numeric, funs(ifelse(. > 0, ., round(., 2)))) %>%
  select(colname, min, q1, mean, q3, max, sd)

#'
#'
#'
stargazer(data, type = "text")

#'
#' ## b) How many men and women are there?
#'
cnt_male <- sum(data$male)
cnt_male
cnt_female <- nrow(data) - cnt_male
cnt_female
#'
#' There are `r cnt_male` men and `r cnt_female` women.
#'
#' ## c) How many people have kids?
#'
cnt_kids <- sum(data$kids)
cnt_kids
#'
#' There are `r cnt_kids` people with kids.
#'
#' ## d) What is the average age of respondents? What is the average education of women?
#'
avg_age <- mean(data$age)
avg_age
avg_educ_female <-
  data %>% filter(male == 0) %>% summarise(avg_educ = mean(educ)) %>% pull(avg_educ)
avg_educ_female

#'
#' `r avg_age` is the average age of respondents. `r avg_educ_female` is the average
#' education of women.
#'
#' ## e) Create a barplot of the number of people by attitudes towards religion.
#'
# Note that relig variable (with values from 2 to 5) already contains the same info.
# notrel = 2, slghtrel = 3, smerel = 4, veryrel = 5
# Note that clearning the data like this is not totally necessary.
#data$antirel = as.list(data[,"relig"==1])
relig_tidy <-
  data %>%
  as_tibble() %>%
  select(relig, ends_with("rel")) %>%
  mutate(antirel = ifelse(relig == 1, 1, 0)) %>%
  rename(num = relig) %>%
  gather(label, bool, -num) %>%
  filter(bool != 0) %>%
  select(-bool) %>%
  mutate_at(vars(label), funs(as.factor))

# Just inspecting the data...
relig_tidy %>%
  group_by(label) %>%
  summarise_at(vars(num),
               funs(
                 cnt = n(),
                 mean,
                 median,
                 q1 = quantile(., 0.25),
                 q3 = quantile(., 0.75)
               ))

viz_relig <-
  relig_tidy %>%
  ggplot() +
  geom_bar(aes(x = label, fill = label))
viz_relig
#'
#'
#' ## f) Create a bar plot of the number of people by happiness in marriage.
#'
# Note that ratemarr variable (with values from 2 to 5) already contains the same info.
# unhap = 2, avgmarr = 3, hapavg = 4, vryhap = 5
# This is identical code to that used for relig_tidy.
hap_tidy <-
  data %>%
  as_tibble() %>%
  filter(yrsmarr > 0) %>%
  select(ratemarr, contains("hap"), avgmarr) %>%
  mutate(vryunhap = ifelse(ratemarr == 1, 1, 0)) %>% 
  rename(num = ratemarr) %>%
  gather(label, bool, -num) %>%
  filter(bool != 0) %>%
  select(-bool) %>%
  mutate_at(vars(label), funs(as.factor))

hap_tidy %>%
  group_by(label) %>%
  summarise_at(vars(num),
               funs(
                 cnt = n(),
                 mean,
                 median,
                 q1 = quantile(., 0.25),
                 q3 = quantile(., 0.75)
               ))

viz_hap <-
  hap_tidy %>%
  ggplot() +
  geom_bar(aes(x = label, fill = label))
viz_hap
#'
#'
#+ include = FALSE
# P1b. ----

#'
#'
#' # Q P1a: Job Training in R
#'
#' ##  1) Clean the environment
#'
rm(list = ls())

#'
#' ## 2) Set your working directory and load the ggplot2 package.
#'
setwd(getwd())
# This library is already loaded, but doing it again doesn't hurt.
library("ggplot2")

#'
#' ## 3) Load the "jtrain2.RData" dataset.
#'
load("jtrain2.RData")

#'
#' ## 4) Describe the data.
#'
#' ### How can you check that the data was loaded properly?
#' Multiple commands can be used, including `str()`, `head()`, `tail()`, and `summary()`.
#'
str(data)
head(data)
tail(data)
summary(data)

#'
#' ### Generate a table with summary statistics.
#'

stargazer(data, type = "text")

#'
#' ### Which command can you use to figure out how many people in the sample particpated in the job training program?
#'
#' The `sum()` function can be used.
#'
cnt_train <- sum(data$train)
cnt_train
#'
#' `r cnt_train` people participated in the job training program.
#'
#' ### How to detect outliers?
#' Use ggplot2 to visualize the data
#' (with `geom_boxplot()`, `geom_histogram()`, or another appropriate function).
#'
data %>%
  # mutate_all(scale) %>%
  gather(colname, value) %>%
  ggplot() +
  geom_boxplot(aes(x = colname, y = value)) +
  facet_wrap( ~ colname, scales = "free")

data %>%
  # mutate_all(scale) %>%
  gather(colname, value) %>%
  ggplot() +
  geom_histogram(aes(x = value)) +
  facet_wrap( ~ colname, scales = "free")

#'
#'
#'
#+ include = TRUE
# Alternatively, use statistical measures (combined with some plotting).
# (Reference: http://r-statistics.co/Outlier-Treatment-With-R.html)

# Using "Multivariate Model Approach" with Cook's distance.
lm_0 <- lm(train ~ ., data = data)
cooksd <- cooks.distance(lm_0)
plot(cooksd,
     pch = "*",
     cex = 2,
     main = "Influential Obs by Cooks distance")
abline(h = 4 * mean(cooksd, na.rm = T), col = "red")
text(
  x = 1:length(cooksd) + 1,
  y = cooksd,
  labels = ifelse(cooksd > 4 * mean(cooksd, na.rm = T), names(cooksd), ""),
  col = "red"
)

influential <-
  as.numeric(names(cooksd)[(cooksd > 4 * mean(cooksd, na.rm = T))])
head(data[influential, ])

#' 
#' It is hard to point out any outliers from the boxplots and historgrams, but the Cook's distance shows us that there about a dozen outliers.
#' 

# Using "Outlier's Test" approach.
# car::outlierTest(lm_0)

# Using "outliers package" approach.
# outliers::scores(data, type = "chisq", prob = 0.95)
# outliers::scores(data, type = "z", prob = 0.95)
# outliers::scores(data, type = "t", prob = 0.95)

# The row-column values with FALSE are the outliers that should be ommitted.

#'
#' ### How can you analyze the distribution of `unem78`, `age`, and `educ`?
#' Use ggplot2 again
#' (with `geom_histogram()` or another appropriate function).
#'
data %>%
  select(unem78, age, educ) %>%
  gather(colname, value) %>%
  ggplot() +
  geom_histogram(aes(x = value)) +
  facet_wrap( ~ colname, scales = "free")

#'
#' ## 5) Run a linear regression model.
#'

fmla_p1 <-
  as.formula("unem78 ~ train + unem74 + unem75 + age + educ + black + hisp + married")
lm_p1 <- lm(fmla_p1, data = data)
summary(lm_p1)
stargazer(lm_p1, type = "text", omit.stat = c("ser"))

#'
#' ## 6) Interpret the `train` coefficient. Did you get the same result?
#'
#' The interpretation for the output in table 1 is that participation in the training
#' program has a significant negative impact on unemployment probabities in 1978, i.e. a positive effect
#' on employment probabilities in 1978.
#' No, I didn't get the exactly the same result because it seems to me that table 1 has filtered
#' the data to 300 observsatvions.
#' (as evident from the degrees of freedom),
#' which is less than the original 445 rows.
#' Nevertheless, the coefficient for "train" is nearly the same as that for the
#' single variable model.
#' In addition, the R^2 and adjusted R^2 values for the full model
#' are better than those for the single variable model.
#'
fmla_p1b <- as.formula("unem78 ~ train")
lm_p1b <- lm(fmla_p1b, data = data)
summary(lm_p1b)
stargazer(lm_p1b, type = "text", omit.stat = c("ser"))

# Should convert the response variable to a factor
# (in compliance with best practices), even if it binary.
# Nevertheless, the result doesn't change.
glm_p1b <-
  glm(fmla_p1b,
      data = data,
      # data = mutate_at(data, vars(unem78), funs(as.factor)),
      family = "binomial")
summary(glm_p1b)
stargazer(glm_p1b, type = "text", omit.stat = c("ser"))
#'
#' ## 7) Run a probit regression.
#' I can just use an internet search to find out how to do a probit regression.
#' (Reference: http://r-statistics.co/Probit-Regression-With-R.html)
#'
probit_1 <-
  glm(fmla_p1,
      data = mutate_at(data, vars(unem78), funs(as.factor)),
      family = binomial(link = "probit"))
summary(probit_1)

#'
#'
#+ include = FALSE
# P2a. ----

#'
#'
#' # Question P2a: Mock Monte Carlo
#'
#' ## 1) Generate a 5D MVN. 5,000 observations, with the given bilaterial correlations.
#'
#' See the custom function below.
#'
#' ### a) Compute the sample covariances for all pairs and the sample variances.
#'

generate_stuff <- function(n = 1, seed = 42) {
  # This is the mostly same setup as in Assignment 2.
  # n <- 1000
  mus <- c(0, 0, 0, 0, 0)
  sigmas <-
    matrix(c(
      1.00, 0.20, 0.10, 0.35, 0.00,
      0.20, 1.00, 0.00, 0.40, 0.00,
      0.10, 0.00, 1.00, 0.00, 0.40,
      0.35, 0.40, 0.00, 1.00, 0.60,
      0.00, 0.00, 0.40, 0.60, 1.00
    ), ncol = 5)

  set.seed(42)
  m_0 <- MASS::mvrnorm(n, mu = mus, Sigma = sigmas, empirical = FALSE)
  m <- cbind(m_0, rep(1, n)) %>% as_tibble()
  varnames <- c("x1", "x2", "x3", "z1", "z2")
  colnames(m) <- c(varnames, "one")
  head(m)
  summary(m)

  # Now it's different from Assignment 2.
  eps1 <- rnorm(n, mean = 0, sd = 1)
  eps2 <- rnbinom(n, size = 2, prob = 0.6)
  eps3 <- 0.15 * m$x1 + eps1

  rhs_constant <- with(m, 0.2 * x1 + 2 * x2 + 0.7 * x3)
  y1 <- rhs_constant + eps1
  y2 <- rhs_constant + eps2
  y3 <- rhs_constant + eps3

  data <- cbind(m, eps1, eps2, eps3, y1, y2, y3) %>% as_tibble()
  
  varnamesy = c(varnames, "y1", "y2", "y3")
  vars <- sapply(sapply(data[, varnamesy], var), round, 4)
  covs <- round(cov(data[, varnamesy]), 4)

  list(vars = vars, covs = covs, data = data)
}

n_5000_results <- generate_stuff(5000)
n_5000_results$vars
n_5000_results$covs

#'
#' ### b) Now generate 3 more samples with 50, 500, and 100,000 observations.
#' Report the three new sample variance-covariance matrices.
#'
#+ include = FALSE
# for(i in c(50, 500, 100000)) {
#
#   n_i_results <- generate_stuff(i)
#   # print("n:")
#   print(i)
#   # cat("\n")
#   print("variances:")
#   # cat("\n")
#   print(n_i_results$vars)
#   # cat("\n")
#   print("covariances:")
#   # cat("\n")
#   print(n_i_results$covs)
#   # cat("\n")
# }
#'
#'
#'
n_50_results <- generate_stuff(50)
n_50_results$vars
n_50_results$covs

n_500_results <- generate_stuff(500)
n_500_results$vars
n_500_results$covs

n_100000_results <- generate_stuff(100000)
n_100000_results$vars
n_100000_results$covs


#'
#' ## 2) Next, use the smallest sample, and run a regression for all three variables.
#'
#' ### Which coefficient beta3 do you expect regression 1, which one do you find, and why?
#'
#' For regression 1, I expect that the coefficient for `x3` will be 0.7 because 0.7 is the value
#' assigned to it when generating the data and because the error term `eps1` used
#' to generate `y1` has a mean value of 0 and finite variance.
#' (Similarly, I would expect the coefficient value for `x1` to be 0.2,
#' and the coefficient value for `x2` to be 2.) The regressed value turns out
#' to be approximately equal to the expected value.
#'
#' (Note that I'm not considering `z1` and `z2` to be variables here, and that
#' I do not elimitate the intercept term.)
#'
#' ### Regression 2?
#'
#' For regression 2, I expect that the coefficient value of `x3` (and also
#' for `x1` and `x2`) will be different. They will be biased by the negative
#' binomial form of `eps2`. Predicting `x3`'s value prior to running a regression
#' is difficult due to the interaction with the other covariates.
#'
#' ### Regression 3?
#'
#' For regression 3, I expect that the coefficient value of `x3` will be the same
#' as it is for regression 1 (i.e. 0.7). (Similarly, I would not expect the
#' coefficient value of `x2` to be different from that observed for regression.
#' On the other hand, I expect that the coefficient value of
#' `x1` will be shifted by an addititve factor of `0.15` because
#' the `eps3` term used to generate `y3` is derived from `x1`.) Indeed, the
#' regressed coefficient value is approximately equal to the expected value for `x3`.
#'
#'
# Need to exclude the epsilon terms.
generate_p2_fmla <- function(i, varnames = c("x1", "x2", "x3") ) {
  formula(paste0("y", i, " ~ ", paste(varnames, collapse = " + ")))
}
fmlas_p2 <- lapply(1:3, generate_p2_fmla)

lm_50_1 <- lm(fmlas_p2[[1]], data = n_50_results$data)
summary(lm_50_1)
# Alternatively...
# broom::tidy(lm_50_1)
# broom::glance(lm_50_1)

lm_50_2 <- lm(fmlas_p2[[2]], data = n_50_results$data)
summary(lm_50_2)
coef(lm_50_2)[[4]]

lm_50_3 <- lm(fmlas_p2[[3]], data = n_50_results$data)
summary(lm_50_3)

stargazer(lm_50_1, lm_50_2, lm_50_3, type = "text")

#'
#' ## 3) Now increase the sample size gradually...
#'
#' The regression coefficient values
#' become closer to the expected values as the sample size increases.
#' The Law of Large Numbers explains why the estimated
#' values approach the theoretical values as the sample size increases--
#' simulated outcomes tend to converge to the theoretical outcomes
#' as the number of trials increases. beta_3 definitely moves closer
#' to the result that I expected to find for regression 1 and 3 because of
#' this Law of Large Numbers. However, for regression 2, I did not really know
#' what to suspect. I would have guessed that the strange distribution of the
#' errors may not have had a big effect, and that turns out to be the case.
#' beta3 converges to 0.2 for regressions 1 and 2, and it converges to 0.35 for
#' regression 3.
#'
ns <- c(100, 1000, 10000)
num_fmlas <- 3
i <- 1
j <- 1
while(i <= num_fmlas) {
  fmla_i <- generate_p2_fmla(i)
  while(j <= length(ns)) {
    n <- ns[j]
    n_results <- generate_stuff(n)
    lm_n_ij <- lm(fmla_i, data = n_results$data)
    coefs_row <- c(i, j, n, coef(lm_n_ij))
    names(coefs_row) <- c("regression", "iteration", "n", names(coef(lm_n_ij)))
    # coefs_row
    if(i == 1 & j == 1) {
      coefs_df <- coefs_row %>% t() %>% as_tibble()
    } else {
      coefs_df <- rbind(coefs_df, coefs_row) # bind_rows(coefs_df, coefs_row)
    }
    coefs_df
    # cat("coefs_row:", coefs_row, "\n")
    j <- j + 1
    # cat("j:", j, "\n")
  }
  j <- 1
  i <- i + 1
  # cat("i:", i, "\n")
}
coefs_df <- coefs_df %>% select(-iteration)
coefs_df

#'
#' ## 4) Is there any fundamentally biased coefficient in one of the three regressions?...
#'
#' Yes. The `x1` variable is fundamentally biased in regression 3.
#' This is because the error term  `eps3` used in generating `y3`
#' is derived from `x1`.
#' Thus, the coefficient value for `x1` is transformed
#' from it's "unbiased" value of 0.2. (In this case,
#' the coefficient value of x1 is shifted additively
#' by 0.15, giving a value of 0.35.)
#' One might say that the endogenous variable `x1` is a consequence of
#' measurement error in the dependent variable and/or omitted variable bias
#' (depending on one's interpretation of the how the data was simulated.)
#'
#' To estimate the coefficient
#' correctly, an IV might be used to "uncorrelate"
#' the endogenous variable `x1` from
#' the unobserved error term (in this case, `eps3`)
#' in regression 3.
#' 2SLS can be used to refit the model. To do this, `x1` must be regressed
#' on all other variables (which must be exogenous), along with any IVs.
#' In this case, `z1` seems to be an appropriate choice
#' for an IV since it is correlated with `x1`. The fitted values
#' from the regression on the endogenous variable `x1`
#' are subsequently used in the original regression
#' model in place of the observed `x1` values.
#'
#' However, upon implementing this scheme, it is apparent that the resultant
#' model is not much different. Presumably, this is because the `z1` IV
#' is correlated with the `x2` independent variable. Thus, the 2SLS model
#' is "weak". A better IV(s) would need to be used to improve the model.
#' In addition, one might say that the IV z1 does not satisfy the IV.1 Relevance criteria
#' for the `x1` endogenous variable. Also, the IV.1 Exogeneity criteria is only
#' approximately true (because `cov(z1, eps3)` is not close "enough" to 0).
#'
#' A weak instrument's test, a Hausman's test, and a Sargan test are implemented
#' (with the `summary()` function called on the variable created by the `ivreg()` function).
#' It is observed that the Hausman test only
#' barely rejects the null hypothesis (that the endogenous variable `x1`
#' is uncorrelated with the unobserved error term), which indicates that the `x1` term
#' might not be considered truly endogenous.
#'
#' The difficulty with this problem  is that it could be a case of "over-identification"--
#' there are possibly more instrument variables (i.e. `z1`, `z2`, etc.) than there are endogenous variables (`x1`).
#' GMM might be a more appropriate method.
#'
# This is the IV estimate.
# It doesn't seem to be correct (probably because there are more than one covariate).
with(n_50_results$data, cov(z1, x1) / cov(z1, y3))

# "Manual" implementation of IV 2SLS
lm_50_3_x1 <- lm(x1 ~ x2 + x3 + z1, data = n_50_results$data)
x1_hat <- fitted.values(lm_50_3_x1)

fmla_3_v2 <- generate_p2_fmla(3, varnames = c("x1_hat", "x2", "x3"))
fmla_3_v2
n_50_results_v2 <- n_50_results
n_50_results_v2$data["x1_hat"] <- x1_hat
lm_50_3_v2 <- lm(fmla_3_v2, data = n_50_results_v2$data)
coef(lm_50_3_v2)

# Alternative implementation, using the AER package.
# References:
# 1) https://rpubs.com/wsundstrom/t_ivreg
# 2) https://bookdown.org/ccolonescu/RPoE4/random-regressors.html#the-instrumental-variables-iv-method
# 3) https://www.r-bloggers.com/instrumental-variables-in-r-exercises-part-1/
# Additionally, see the AER package documentation for how to construct the ivreg() formula, etc..
# Basically, ivreg(Y ~ X + W | W + Z, ... ), where X is endogenous variable(s),
# Z is instrument(s), and W is exogenous controls (not instruments).
lm_50_3_ivreg <- AER::ivreg(y3 ~ x1 + x2 + x3 | x2 + x3 + z1, data = n_50_results$data)
lm_50_3_ivreg

summary(lm_50_3_ivreg, diagnostics = TRUE)


# Note that the covariance of the IV z1 and the error eps3 is near 0, which is desirable.
# Note taht the covariance of the IV z1 and the response y3 is non-0, which is desirable.
# Note the relaitvely high correlation of the chosen IV z1 with the x2 independent variable.
# (On the other hand, the correlation between z1 and x3 is low, which is desireable.)
# Presumably, it would be nice if z1 were not so highly correlated with the
# other independent variables, so as to isolate the effect of x1.
with(n_50_results$data, cov(z1, eps3))
with(n_50_results$data, cov(z1, y3))
with(n_50_results$data, cov(z1, x1))
with(n_50_results$data, cov(z1, x2))
with(n_50_results$data, cov(z1, x3))
#'
#'
#+ include = FALSE
# P2b. ----

#'
#'
#' # Q P2b: IV Exercise
#'
#' ## 1) Suggest an IV method.... Explain what is needed for identification.
#' 
#'  It seems like linear 2SLS is an appropriate IV method.
#'  For this method we should regress q1 on all the z variables, and then use
#'  the fitted values from this regression as variables for our second stage
#'  regression, in which we also include all of the original exogenous variables.
#'  Once again, for identification we need the assumptions that the instruments are
#'  exogeneous and relevant. We can actually test if our IVs are relevant, but we
#'  need to make arguments to explain why they are exogenous.
#'
#' ## 2) Describe the economic assumptions for the previous part if the model is extended...
#'
#' These assumptions are exogeneity and relevance, which have been outlined many times
#' already in this problem set. By relevance, we mean that there is some type of significant
#' relationship between the IVs, family background variables, and IQ. I think it is fair 
#' to say that this is true here. Now, we must also have exogeneity, i.e. that the
#' family background variables only affect wages through IQ. In general, we would expect
#' there to be some type of correlation between family background and wages, but if we control
#' for IQ, we believe that most of this effect goes away. I think that this is also
#' a reasonable assumption because something like the education of your parents should not
#' affect your wages without considering their education on your IQ.
#'
#' ## 3) Implementation and discussion.
#'
#' Here is the implementation:
#'
wage_educ <- readr::read_csv("data/wage_educ.csv")
glimpse(wage_educ)

# Building up the formulas here.
fmla_2b_shared_rhs <- "exper + tenure + educ + married + south + urban + black"
fmla_2b_shared_rhs_iq <- str_c(fmla_2b_shared_rhs, " + iq")
fmla_2b_shared_rhs_kww <- str_c(fmla_2b_shared_rhs, " + kww")
fmla_2b_shared_iq <- str_c("log(wage) ~ ", fmla_2b_shared_rhs_iq)
fmla_2b_shared_kww <- str_c("log(wage) ~ ", fmla_2b_shared_rhs_kww)
fmla_2b_shared_rhs_2 <- str_c(fmla_2b_shared_rhs, " + sibs + meduc + feduc")

#fmla_2b_iq_ols <- as.formula(fmla_2b_shared_iq)
fmla_2b_iq_vireg <- as.formula(str_c(fmla_2b_shared_iq, " | ", fmla_2b_shared_rhs_2))

#fmla_2b_kww_ols <- as.formula(fmla_2b_shared_kww)
fmla_2b_kww_vireg <- as.formula(str_c(fmla_2b_shared_kww, " | ", fmla_2b_shared_rhs_2))

#lm_2b_iq_ols <- lm(fmla_2b_iq_ols, data = wage_educ)
#summary(lm_2b_iq_ols)
lm_2b_iq_ivreg <- AER::ivreg(fmla_2b_iq_vireg, data = wage_educ)
summary(lm_2b_iq_ivreg, diagnosics = TRUE)

#lm_2b_kww_ols <- lm(fmla_2b_kww_ols, data = wage_educ)
#summary(lm_2b_kww_ols)
lm_2b_kww_ivreg <- AER::ivreg(fmla_2b_kww_vireg, data = wage_educ)
summary(lm_2b_kww_ivreg, diagnosics = TRUE)

#' In both cases, the married and urban factors are statistically significant when looking
#' at log wages. Also, our IVs for IQ significantly affect log wages through IQ, but not
#' through KWW. This would be more interesting if I knew what KWW was, but it does make
#' sense that these family background factors can effect log wages through IQ.

#'
#'
#'
#+ include = FALSE
# P2b. ----

#'
#'
#' # Q P3: Panel Exercise
#'
#' ## 1)
#'
#' $$
#' newst_i = \theta_t + \beta_1vfst_{it} + \alpha_i + \mu_{it}
#' $$
#'
#' $$
#' tc_i = \theta_t + \beta_1vfst_{it} + \alpha_i + \mu_{it}
#' $$
#'
#' where $\alpha_i$ represents fixed factors that affect the economic climate,
#' $\theta_t$ represents a different intercept for each time period,
#' $t$ represents the time period (i.e.before/after implementation)
#' $i$ is an index for household.
#'
#' ## 2)
#'
#' The male household indicator variable `maleh` can be added to the formuia as a fixed effect.
#' $$
#' newst_i = \theta_t + \beta_1vfst_{it} + \alpha_i + maleh_i + \mu_{it}
#' $$
#'
#' ## 3)
#'
#' The assumption that village funds are random is probably naive.
#' It is likely that a number of factors are related to the initial amount
#' of village funds, including: average household size in a village and the
#' the number of households in a village (i.e. population),
#' the economic standing of each household, (i.e. poverty levels)  etc.
#' If the assumption of random distribution of funds is indeed false,
#' then it will make the estimate of the coefficient for `vfst` unreliable/inconsistent.
#'
#' To provide more detail, the bias in the described case comes at an "entity"-specific (and not
#' necessarily a time-variant) level of abstraction. Thus, if this bias
#' is not accounted for, then model estimates are doomed to be shifted. Therefore,
#' the fitted values of new short term credit and total consumption will be wrong, i.e. misidentified.
#'
#' ## 4)
#'
#' A linear fixed effects model incorporating household attributes
#' seems like an appropriate method for modeling this panel data.
#' Ideally, valid IVs could also be used to help
#' remove bias from the dependent variable,
#' but it seems more probable than not that most IVs will be weak and will
#' not lead to improvement with the model. Rather, controlling for
#' circumstances with fixed effect seems appropriate. A random effect
#' model seems unecessary because it seems probable that the
#' data is highly time-variant or impossible to model with proper controls.
#'
#' Differencing might also be tested since it--like a fixed effects framework--
#' can be useful for accounting for entity-level attributes (by modeling
#' them as entity-specific constants via dummy variables).
#'
#' The proposed procedure avoids the issues of the previous models because
#' the previously unobserved entity-specfic effects (i.e. caused by ommitted variables
#' and sampling bias) are accounted for by controls incorporated into the model.
#'
#' ### 5) Implementation of OLS, FD, and FE
#'
#' It is noteworthy that the FD and FE models estimate similar values for
#' predicting `newst`, but not for predicting `tc`. This could indicate
#' that there is still some bias that is not accounted for in the `tc` panel data models.
#' OLS gives pretty different estimates than the other two because the way the models
#' are carried out is different, so this was expected.
#'
rm(list = ls())
library("plm")
load("microcredit.Rdata")
glimpse(dt.microcredit)
stargazer(dt.microcredit, type = "text")
dt.microcredit %>%
  as_tibble() %>%
  group_by(village, year) %>%
  summarise_at(vars(vfst, newst, tc), funs(n()))


# microcredit_treated <- dt.microcredit
dt.microcredit %>% as_tibble() %>% group_by(caseid, year)

# microcredit_pdf <- pdata.frame(micocredit_treated, index = c("village", "year"))
microcredit_pdf <- data.frame(dt.microcredit, index = c("caseid", "year"))
# microcredit_pdf
# table(index(microcredit_pdf), useNA = "ifany")

plm_rhs_constant <- str_c("vfst + d1 + d2 + d3 + d4 + d5 + d6 + d7")
plm_newst_fmla <- str_c("newst ~ ", plm_rhs_constant) %>% as.formula()
# plm_newst_fmla_no1 <- str_c("newst ~ ", plm_rhs_constant, " + 0") %>% as.formula()

# Must specify index for fixed effects model, although not for others(?)
plm_newst_ols <- plm(plm_newst_fmla, data = microcredit_pdf, index = c("caseid", "year"), model = "pooling")
plm_newst_fe <- plm(plm_newst_fmla, data = microcredit_pdf, index = c("caseid", "year"), model = "within")
plm_newst_fd <- plm(plm_newst_fmla, data = microcredit_pdf, index = c("caseid", "year"), model = "fd")
# plm_newst_fd_no1 <- plm(plm_newst_fmla_no1, data = microcredit_pdf, index = c("caseid", "year"), model = "fd")

stargazer(
  plm_newst_ols,
  plm_newst_fe,
  plm_newst_fd,
  # plm_newst_fd_no1,
  type = "text",
  omit = "d.*",
  no.space = FALSE,
  omit.labels = c("Time Dummies"),
  column.labels = c("Pooling", "FE", "FD") #, "FD w/o Intercept")
)

plm_tc_fmla <- str_c("tc ~ ", plm_rhs_constant) %>% as.formula()

plm_tc_ols <- plm(plm_tc_fmla, data = microcredit_pdf, index = c("caseid", "year"), model = "pooling")
plm_tc_fe <- plm(plm_tc_fmla, data = microcredit_pdf, index = c("caseid", "year"), model = "within")
plm_tc_fd <- plm(plm_tc_fmla, data = microcredit_pdf, index = c("caseid", "year"), model = "fd")
# plm_tc_fd_no1 <- plm(plm_tc_fmla_no1, data = microcredit_pdf, index = c("caseid", "year"), model = "fd")

stargazer(
  plm_tc_ols,
  plm_tc_fe,
  plm_tc_fd,
  # plm_tc_fd_no1,
  type = "text",
  omit = "d.*",
  no.space = FALSE,
  omit.labels = c("Time Dummies"),
  column.labels = c("Pooling", "FE", "FD")
)

#'
#' ## 6) Importance of time trend?
#'
#' It is important to incporate a time trend in order to account for possible
#' seasonality in the data (i.e. correlation among lagged intervals).
#' Not accounting for time-variant effects would lead to misleading deductions.
#'
#' ## 7) Test for Serial Correlation
#'
#' It seems like there is some serial correlation (although this
#' deduction could be to improper coding implementation).
#' This indicates that the FD model might be better to use.
#'
# Method suggested in lecture slides.
u <- residuals(plm_newst_fd)
microcredit_pdf_lag1 <- microcredit_pdf
microcredit_pdf_lag1[as.double(names(u)), "u"] <- u
plm_newst_fd_lag1 <- plm(u ~ lag(u, 1), microcredit_pdf_lag1, index = c("caseid", "year"), model = "pooling")
lm(u ~ lag(u, 1), microcredit_pdf_lag1) %>% summary()
summary(plm_newst_fd_lag1)

# Two alternative tests.
lmtest::dwtest(plm_newst_fd)
Box.test(residuals(plm_newst_fd), type = "Ljung-Box")

# Repeating the lecture method for tc model.
u <- residuals(plm_tc_fd)
microcredit_pdf_lag1 <- microcredit_pdf
microcredit_pdf_lag1[as.double(names(u)), "u"] <- u
plm_tc_fd_lag1 <- plm(u ~ lag(u, 1), microcredit_pdf_lag1, index = c("caseid", "year"), model = "pooling")
lm(u ~ lag(u, 1), microcredit_pdf_lag1)
summary(plm_tc_fd_lag1)

#'
#'
#'
#'
#'
#'

