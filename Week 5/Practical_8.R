
###############################
###PACKAGES
###############################

library(tidyverse)
library(survival)
library(flexsurv)
library(eha)
library(timereg)


###############################
###PART A
###############################

#question 1
pbc=read_csv("Datasets/pbcbase_2021.csv")

#question 2

mod.weib.ph = weibreg(Surv(time, d) ~ as.factor(treat), data = pbc)
summary(mod.weib.ph)

#question 3

mod.weib.aft=survreg(Surv(time,d)~as.factor(treat),data=pbc,dist="weibull")
summary(mod.weib.aft)

mod.weib.aft=flexsurvreg(Surv(time,d)~as.factor(treat),data=pbc,dist="weibull")
mod.weib.aft

#survreg and flexsurvreg use the AFT parametrization of the Weibull

#question 5

pbc$logbil0=log(pbc$bil0)

mod.weib2=survreg(Surv(time,d)~as.factor(treat)+logbil0,data=pbc,dist="weibull")
summary(mod.weib2)

#question 6

mod.loglog=survreg(Surv(time,d)~as.factor(treat)+logbil0,data=pbc,dist="loglogistic")
summary(mod.loglog)

#question 7

AIC(mod.weib2)

AIC(mod.loglog)

###############################
###PART B
###############################

#recode treat as a 0/1 variable
pbc$treat=pbc$treat-1

#question 8

mod.aalen=aalen(Surv(time,d)~treat,data=pbc)
plot(mod.aalen)

#cumulative coefficients stored in
View(mod.aalen$cum)

#question 9

mod.aalen=aalen(Surv(time,d)~treat,data=pbc,resample.iid=1)#resample.iid=1 option is needed to get predicted survival probabilities

survprob.treat0=predict(mod.aalen,newdata=data.frame(treat=0),times=5,n.sim=0)
survprob.treat0$S0

survprob.treat1=predict(mod.aalen,newdata=data.frame(treat=1),times=5,n.sim=0)
survprob.treat1$S0

#question 10

mod.aalen2=aalen(Surv(time,d)~treat+logbil0,data=pbc,resample.iid=1)
plot(mod.aalen2)

pbc$logbil0_c=pbc$logbil0-3.5

mod.aalen3=aalen(Surv(time,d)~treat+logbil0_c,data=pbc,resample.iid=1)
plot(mod.aalen3)

#question 11

survprob.treat0.logbil05=predict(mod.aalen3,newdata=data.frame(treat=0,logbil0_c=1.5),times=5,n.sim=0)
survprob.treat0.logbil05$S0

###############################
###PART C
###############################

#question 12

alloauto=read.table("alloauto.csv",sep=",",header = T)
alloauto$type=alloauto$type-1 #IMPORTANT FOR AALEN MODEL

#question 13

km=survfit(Surv(time,delta)~as.factor(type),data=alloauto)

km_transform=log(km$surv/(1-km$surv))
km_transform_type1=km_transform[1:km$strata[1]]
km_transform_type2=km_transform[(km$strata[1]+1):(km$strata[1]+km$strata[2])]

times_type1=km$time[1:km$strata[1]]
times_type2=km$time[(km$strata[1]+1):(km$strata[1]+km$strata[2])]

#question 14

plot(times_type1,km_transform_type1,type="s",col="blue",ylim=c(-1,4),ylab="S(t)/(1-S(t))",xlab="Time")
points(times_type2,km_transform_type2,type="s",col="red")

#question 15

mod.aalen=aalen(Surv(time,delta)~type,data=alloauto,resample.iid=1)
plot(mod.aalen)
