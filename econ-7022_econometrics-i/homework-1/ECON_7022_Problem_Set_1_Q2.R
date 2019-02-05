
rm(list = ls())

# 2.) Grades analysis on different data set-------------------------------------

# a) ---------------------------------------------------------------------------

load("dt_wages.RData")

# Slide 10
# Displaying the first 10 rows of the data.
dt.wages[1:10,]

# Slide 11
# Displaying a summary of the data by examing the quantiles for each variable.
summary(dt.wages)

# Slide 12
# Plotting wages to better visualize its sample distribution.
#qplot(wage, geom = "histogram", data = dt.wages)
qplot(round(wage), geom = "bar", data = dt.wages)

# Slide 13
# Plotting experience against wages to see a general trend.
qplot(exper, wage, geom = "point", data = dt.wages)

# Slide 14
# Plotting experience against wages with a line of best fit to better see the trend.
qplot(exper, wage, geom = "point", data = dt.wages) +
  geom_smooth(method = lm, se = FALSE)

# Slide 15
# Calculating the sample covariance between wages and experience.
library(data.table)
dt.wages <- data.table(dt.wages)
dt.wages[, cov(wage, exper)] # 5.659076

# Calculating the sample conditional mean for two different subsets of the data.
# Note that the wage level does not differ by much when the sample is split at its median.
mid <- median(dt.wages$exper)
dt.wages[exper < mid, mean(wage)] # 5.294297
dt.wages[exper >= mid, mean(wage)] # 6.497909

# Slide 16
# Displaying the conditional final wage mean for each experience level.
# Note that this relationship no longer looks linear.
ggplot(dt.wages[, list(avg_wage = mean(wage)), by = list(exper = round(exper))]) +
  geom_point(aes(exper, avg_wage))

# b) ---------------------------------------------------------------------------

# Split the data up into subsets of men and women.
library(dplyr)
dt.wages.gender <-
  dt.wages %>%
  mutate(gender = if_else(female == 1, "female", "male")) %>%
  mutate_at(vars(gender), funs(factor))

dt.wages.gender.f <-
  dt.wages.gender %>%
  filter(female == 1)

dt.wages.gender.m <-
  dt.wages.gender %>%
  filter(female == 0)

# Slide 10
# Displaying the first 10 rows of each subset of data.
dt.wages.gender.f[1:10,]
dt.wages.gender.m[1:10,]

# Slide 11
# Displaying a summary of the data by examing the quantiles for each variable of each data set.
summary(dt.wages.gender.f)
summary(dt.wages.gender.m)

# Slide 12
# Plotting wages to better visualize its sample distribution for men and women.
ggplot(dt.wages.gender) +
  geom_histogram(aes(x = wage, fill = gender), bins = 20) +
  facet_wrap(~ gender)

# Slide 13
# Plotting experience against wages for men and women to see a general trend.
ggplot(dt.wages.gender) +
  geom_point(aes(x = exper, y = wage, color = gender)) +
  facet_wrap(~ gender)

# Slide 14
# Plotting experience against wages for men and women with lines of best fit to better see the trend
# for each gender.
# Note that the relationship between experience and wage is negative for women,
# whereas it is positive men, as might be expected.
# Also, the spread of data points is much tighter for women than it is for men.
# Both of these implications suggest that women don't receive the same
# monetary benefits as their male peers given equal experience levels. In fact,
# it appears that women get lower wages as they get older.
library(ggpmisc)
dt.wages.gender %>%
  ggplot(aes(x = exper, y = wage, label = exper)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label..)),
               parse = TRUE) +
  facet_wrap( ~ gender)

# Slide 15
# Calculating the sample covariance between wages and experience for men and women.
dt.wages.gender.f <- data.table(dt.wages.gender.f)
# Note the relatively different covariance compared to the whole sample.
dt.wages.gender.f[, cov(wage, exper)] # -0.2730962
# Interestingly, mean wage is lower for women with higher experience
# (when splitting the data at the whole sample's median).
mid <- median(dt.wages$exper)
dt.wages.gender.f[exper < mid, mean(wage)] # 4.651756
dt.wages.gender.f[exper >= mid, mean(wage)] # 4.518264

dt.wages.gender.m <- data.table(dt.wages.gender.m)
# The covariance is also significantly different than that for the whole sample.
dt.wages.gender.m[, cov(wage, exper)] # 9.769334
# Mean wage level changes more drastically relative to experience for the
# sample with only men.
dt.wages.gender.m[exper < mid, mean(wage)] # 5.93197
dt.wages.gender.m[exper >= mid, mean(wage)] # 8.184789


# Slide 16
# Displaying the conditional final wage mean for each experience level for each gender.
ggplot(dt.wages.gender.f[, list(avg_wage = mean(wage)), by = list(exper = round(exper))]) +
  geom_point(aes(exper, avg_wage))

ggplot(dt.wages.gender.m[, list(avg_wage = mean(wage)), by = list(exper = round(exper))]) +
  geom_point(aes(exper, avg_wage))

# c) ---------------------------------------------------------------------------

# Creating a new data frame for this question
dt.wages.married <-
  dt.wages %>%
  mutate(married_fctr = if_else(married == 1, "yes", "no")) %>%
  mutate_at(vars(married_fctr), funs(factor)) %>%
  dplyr::select(wage, married, married_fctr)

# Trying different ways of visualizing the data.
# Each of the following plots do indicate that there is some unfavorable
# bias against married people (in terms of higher wages).
dt.wages.married %>%
  group_by(married_fctr) %>%
  summarise(avg_wage = mean(wage)) %>%
  ggplot(aes(x = married_fctr, y = avg_wage, fill = married_fctr)) +
  geom_col()

dt.wages.married %>%
  ggplot(aes(x = married_fctr, y = wage, fill = married_fctr)) +
  geom_boxplot()

library(ggjoy)
dt.wages.married %>%
  ggplot(aes(x = wage, y = married_fctr, fill = married_fctr)) +
  geom_joy()

# The linear model output suggests that being married is a significant
# predictor of wages since p = 1.121e-07 < 0.05 (where 0.05 is an
# arbitrary significance level).
# Also, the 95% confidence interval for wages of people who are not
# married (married_fctr = "no") is simply the 95% confidence interval
# on the intercept of the line of best fit, [4.35140, 5.336427].
lin_reg <- lm(wage ~ married_fctr, data = dt.wages.married)
summary(lin_reg) # p = 1.121e-07
confint(lin_reg) # [4.351340, 5.336427]

