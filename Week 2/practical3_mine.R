library(survival)
library(eha)
library(flexsurv)
library(tidyverse)
library(survminer)


whl<-read_csv(file='Datasets/whitehall.csv')

whl$timein=as.Date(whl$timein,"%d%b%Y")
whl$timeout=as.Date(whl$timeout,"%d%b%Y")
whl$timebth=as.Date(whl$timebth,"%d%b%Y")

whl$timein=as.numeric(whl$timein)/365.25
whl$timeout=as.numeric(whl$timeout)/365.25
whl$timebth=as.numeric(whl$timebth)/365.25

table(whl$grade)
# 1194 admin & professional
# 483 clerical & other

table(whl$grade, whl$chd)

whl$time=(whl$timeout-whl$timein)

# Question 2
whl.km <-
     survfit(Surv(
          time = timeout,
          event = chd,
          origin = timein
     ) ~ as.factor(grade),
     data = whl)
     
ggsurvplot(whl.km,
           whl,
           censor = F,
           conf.int = T)

# Summary of 5,10,15 years
summary(whl.km, times = c(5,10,15))

# Log-rank test
survdiff(Surv(
     time = timeout,
     event = chd,
     origin = timein
) ~ grade, data = whl)

# Question 3
# Hazard and survivor functions, likelihood

# H(t) = \lambda 
# S(t) = exp( - \lambda * t )
# f(t) = H(t) * S(t) = - lambda * exp (- \lambda * t)

# Copy from the slides!




