library(survival)
library(ggplot2)
library(survminer)


##############################################################
########################Part A - Alloauto data################
##############################################################

#---
#question 1

allo <- read_csv(file='./Datasets/alloauto.csv')


#---
#question 2

km=survfit(Surv(time,delta)~type,data=allo)

allo.km <- survfit(Surv(time,delta)~as.factor(type),data=allo)

ggsurvplot(allo.km, data = allo,conf.int = T,censor=F,legend.title="",
           legend.labs = c("Allogenic","Autologous"))

plot(allo.km,fun="cloglog",xlab="time (log scale)",ylab="log(-log S(t))",
     col=c("blue","red"))
legend(0.02,0,c("Allogenic","Autologous"),col=c("blue","red"),lty=1,cex=0.5)

ggsurvplot(allo.km, data = allo,conf.int = T,fun="cloglog",censor=F,legend.title="",
           legend.labs = c("Allogenic","Autologous"))


#---
#question 3

allo.cox <- coxph(Surv(time,delta)~as.factor(type),data=allo)

summary(allo.cox)

sch.resid=cox.zph(allo.cox, transform = 'identity')
sch.resid
plot(sch.resid)

#---
#question 4 (a)

allo.mod.t=coxph(Surv(time,delta)~as.factor(type)+tt(type),data=allo,tt=function(x,t,...){x*t})
summary(allo.mod.t)

#---
#question 4 (b)

allo.cox.t2=coxph(Surv(time,delta)~as.factor(type)+tt(type),data=allo,tt=function(x,t,...){x*(t>18)})
summary(allo.cox.t2)

#---
#question 5

allo.split=survSplit(Surv(time,delta)~., data=allo, cut=18, end="time", event="delta", start="time0", episode="time_period")

#equivalent to 4(b)
allo.cox.t2new=coxph(Surv(time0,time,delta)~as.factor(type)*as.factor(time_period),data=allo.split)
summary(allo.cox.t2new)


#equivalent to 4(a)
allo=read.table("alloauto.csv",sep=",",header=T)
event.times=alloauto$time[alloauto$delta==1]
alloauto.split=survSplit(Surv(time,delta)~., data=alloauto, cut=event.times, end="time", event="delta", start="time0")

allo.cox.tnew=coxph(Surv(time0,time,delta)~as.factor(type)*time,data=allo.split)
summary(allo.cox.tnew)

##############################################################
########################Part B - PBC data#####################
##############################################################

#---
#question 1


pbc <- read_csv(file='./Datasets/pbcbase_2021.csv')
names(pbc)
View(pbc)
summary(pbc)

pbc$datein=as.Date(pbc$datein,"%d%b%Y")
pbc$dateout=as.Date(pbc$dateout,"%d%b%Y")

#---
#question 2

pbc.cox=coxph(Surv(time,d)~as.factor(treat),data=pbc)
summary(pbc.cox)

mgale_res1<-resid(pbc.cox,type="martingale")
plot(pbc$bil0,mgale_res1)
lines(lowess(pbc$bil0,mgale_res1))

pbc$logbil0=log(pbc$bil0)
plot(pbc$logbil0,mgale_res1)
lines(lowess(pbc$logbil0,mgale_res1))

pbc.cox2=coxph(Surv(time,d)~as.factor(treat)+logbil0,data=pbc)
summary(pbc.cox2)

#---
#question 3(a)

sch.resid=cox.zph(pbc.cox2, transform = 'identity')
sch.resid
plot(sch.resid)

#---
#question 3(b)

pbc.cox.treat.t=coxph(Surv(time,d)~as.factor(treat)+logbil0+tt(treat),data=pbc,tt=function(x,t,...){x*t})
summary(pbc.cox.treat.t)

pbc.cox.bil.t=coxph(Surv(time,d)~as.factor(treat)+logbil0+tt(logbil0),data=pbc,tt=function(x,t,...){x*t})
summary(pbc.cox.bil.t)

#---
#question 4(a)

pbc.cox2=coxph(Surv(time,d)~as.factor(treat)+logbil0,data=pbc)
mgale_res2<-resid(pbc.cox2,type="martingale")

plot(pbc$age,mgale_res2)
lines(lowess(pbc$age,mgale_res2))

pbc.cox3=coxph(Surv(time,d)~treat+logbil0+age+cir0,data=pbc)
summary(pbc.cox3)

#---
#question 4(b)

sch.resid=cox.zph(pbc.cox3, transform = 'identity')
sch.resid
plot(sch.resid)

#---
#question 5

pbc.cox.strat=coxph(Surv(time,d)~treat+logbil0+age+strata(cir0),data=pbc)
summary(pbc.cox.strat)

#---
#question 6

#deviance residuals

devres<-resid(pbc.cox3,type="deviance")
plot(devres,xlab="index", ylab="Deviance residuals",cex.lab=1.3)
abline(h=0)

#delta betas

delta.betas<-resid(pbc.cox3,type="dfbeta")
head(delta.betas)

plot(delta.betas[,1],xlab="index",ylab="Delta-betas",main="treat")
abline(h=0)
plot(delta.betas[,2],xlab="index",ylab="Delta-betas",main="logbil0")
abline(h=0)
plot(delta.betas[,3],xlab="index",ylab="Delta-betas",main="age")
abline(h=0)
plot(delta.betas[,4],xlab="index",ylab="Delta-betas",main="cir0")
abline(h=0)


