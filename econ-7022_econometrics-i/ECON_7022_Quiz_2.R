
rm(list = ls())

# Quiz 2 - Empirical Problem: Grades -------------------------------------------

dt.grades <- read.csv("dt_grades.csv")

# Slide 10
# Displaying the first 10 rows of the data.
dt.grades[1:10,]

# Slide 11
# Displaying a summary of the data by examing the distribution for each assignment.
summary(dt.grades)

# Slide 12
# Plotting the final grades to better visualize its sample distribution.
#qplot(final_exam, geom = "histogram", data = dt.grades)
qplot(factor(final_grade), geom = "bar", data = dt.grades)

# Slide 13
# Plotting midterm grades against final exams to see a general trend.
qplot(midterm, final_exam, geom = "point", data = dt.grades)

# Slide 14
# Plotting midterm grades against final grades with a line of best fit to better see the trend.
qplot(midterm, final_exam, geom = "point", data = dt.grades) +
  geom_smooth(method = lm, se = FALSE)

# Slide 15
# Calculating the sample covariance between the midterm and final exam grades.
library(data.table)
dt.grades <- data.table(dt.grades)
dt.grades[,cov(final_exam, midterm)]

# Calculating the sample conditional mean for two different subsets of the data.
dt.grades[midterm < 10, mean(final_exam)]
dt.grades[midterm >= 10, mean(final_exam)]

# Slide 16
# Displaying the conditional final exam mean for each midterm grade.
ggplot(dt.grades[, list(avg_final_exam = mean(final_exam)), by = list(midterm = round(midterm))]) +
  geom_point(aes(midterm, avg_final_exam))

