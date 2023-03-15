library(tidyverse)
library(survival)
library(survminer)

allo <- read_csv(file='./Datasets/alloauto.csv')
pbcbase_2021 <- read_csv(file='./Datasets/pbcbase_2021.csv')

allo.km <- survfit(Surv(time,delta)~as.factor(type),data=allo)

# Kaplan-Meier
ggsurvplot(
     allo.km,
     data = allo,
     conf.int = T,
     censor = F,
     legend.title = "",
     legend.labs = c("Allogenic", "Autologous")
)

#------
#question 2


ggsurvplot(
     allo.km,
     data = allo,
     conf.int = T,
     fun = "cloglog",
     censor = F,
     legend.title = "",
     legend.labs = c("Allogenic", "Autologous")
)


#comparing survivor curves from a Cox model with the Kaplan-Meier survivor curves

allo.cox <- coxph(Surv(time,delta)~as.factor(type),data=allo)

par(mfrow=c(1,2))
plot(survfit(allo.cox,newdata=data.frame(type=c(1,2))),
     col=c("blue","red"),xlab="time",ylab="S(t)",main="Cox")
plot(allo.km,mark.time=F,col=c("blue","red"),xlab="time",ylab="S(t)",main="Kaplan-Meier")


sch.resid <- cox.zph(allo.cox, transform = "identity")

par(mfrow=c(1,1))
plot(sch.resid)


allo.mod.t <- coxph(Surv(time, delta)~as.factor(type)+tt(type), data=allo, tt=function(x,t,...){x*t>18})




     