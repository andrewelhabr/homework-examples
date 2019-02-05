
##### C6.12 ####################################################################
rm(list = ls())

load('401ksubs.RData')
summary(data)
desc

min(data$age)
sum(data$age == min(data$age))

model_1 = lm(nettfa ~ inc + age + agesq, data = data)
summary(model_1)

data$ageshift = data$age - 25
data$ageshiftsq = data$ageshift^2
model_2 = lm(nettfa ~ inc + age + ageshiftsq, data = data)
summary(model_2)

model_3 = lm(nettfa ~ inc + ageshiftsq, data = data)
summary(model_3)

library("ggplot2")
eq = function(x){-0.483884 + (x-25)^2}
ggplot(data.frame(x=c(25, 100)), aes(x=x)) + stat_function(fun=eq, geom="line")
          + xlab("age") + ylab("nettfa")

model_4 = lm(nettfa ~ inc + incsq + age + agesq, data = data)
summary(model_4)

##### C7.12 ####################################################################
rm(list = ls())

load('beauty.RData')
summary(data)
desc

# Proportion of belavg men, abvavg men, belavg women, and abvavg women.
sum(data[which(data$female == 0),"belavg"] == 1)/sum(data$female == 0)
sum(data[which(data$female == 0),"abvavg"] == 1)/sum(data$female == 0)
sum(data[which(data$female == 1),"belavg"] == 1)/sum(data$female == 1)
sum(data[which(data$female == 1),"abvavg"] == 1)/sum(data$female == 1)

sum(data$abvavg)/sum(data$belavg)
# About 2.5 times as many people are rated as above average.

#t.test()

model_1 = lm(abvavg ~ female, data = data)
summary(model_1)
pt(1.477, 1258, lower.tail=FALSE)

model_m = lm(lwage ~ belavg + abvavg, data = data[which(data$female == 0),])
s_m = summary(model_m)
model_f = lm(lwage ~ belavg + abvavg, data = data[which(data$female == 1),])
s_f = summary(model_f)

# One-sided hypothesis testing of beta1
pt(coef(s_m)[,3], s_m$df[2], lower.tail=TRUE)
pt(coef(s_f)[,3], s_f$df[2], lower.tail=TRUE)

model_2m = lm(lwage ~ belavg + abvavg + educ + exper + expersq + union + goodhlth + black + married + south + bigcity + smllcity + service, data = data[which(data$female == 0),])
summary(model_2m)
model_2f = lm(lwage ~ belavg + abvavg + educ + exper + expersq + union + goodhlth + black + married + south + bigcity + smllcity + service, data = data[which(data$female == 1),])
summary(model_2f)

model_2c = lm(lwage ~ belavg + abvavg + educ + exper + expersq + union + goodhlth + black + married + south + bigcity + smllcity + service + 0, data = data)
summary(model_2c)

fstat = (sum(model_2c$residuals^2) - sum(model_2m$residuals^2) - sum(model_2f$residuals^2)) /(sum(model_2m$residuals^2) + sum(model_2f$residuals^2))*(1260-2*(13+1))/13
fstat

sum(data$female == 0)
sum(data$female == 1)

pf(fstat, 13, 1260-2*(13+1), lower.tail = FALSE)

##### C8.5 #####################################################################
rm(list = ls())

load('smoke.RData')
summary(data)
desc

model_1 = lm(cigs ~ lincome + lcigpric + educ + age + agesq + restaurn, data = data)
summary(model_1)

model_1$residuals^2

data$u = model_1$residuals^2
data$logu = log(data$u)

model_2 = lm(logu ~ lincome + lcigpric + educ + age + agesq + restaurn, data = data)
summary(model_2)

data$h = exp(model_2$fitted.values)
data$hinv = 1/data$h
model_wls = lm(cigs ~ lincome + lcigpric + educ + age + agesq + restaurn, data = data, weights = data$hinv)
summary(model_wls)

data$uhat = model_wls$residuals
data$yhat = model_wls$fitted.values
data$ucurl = data$uhat/sqrt(data$h)
data$ucurlsq = (data$ucurl)^2
data$ycurl = data$yhat/sqrt(data$h)
data$ycurlsq = (data$ycurl)^2
model_test = lm(ucurlsq ~ ycurl + ycurlsq, data = data)
summary(model_test)

data$uhatorig = model_1$residuals
data$yhatorig = model_1$fitted.values
data$ucurlorig = data$uhatorig/sqrt(data$h)
data$ucurlsqorig = (data$ucurlorig)^2
data$ycurlorig = data$yhatorig/sqrt(data$h)
data$ycurlsqorig = (data$ycurlorig)^2
model_test_orig = lm(ucurlsqorig ~ ycurlorig + ycurlsqorig, data = data)
summary(model_test_orig)

library(RCurl)

# import the function from repository
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

summary(model_wls, robust = TRUE)
