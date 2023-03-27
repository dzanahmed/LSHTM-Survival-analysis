library(survival)
library(Epi)
library(tidyverse)
library(survminer)

# Package for writing decorated code blocks - from Ignazio
library(bannerCommenter)

# Change the below line to your directory path

###################################################
########################Part A#####################
###################################################

#------
#load the data and take a look
hip <- read_csv("Datasets/hip4.csv")

#------
#question 1

dim(hip) #106 rows

length(unique(hip$id)) #48 individuals

hip[hip$id=="11",]

hip[hip$id=="18",]

#------
#question 2

Surv(time=hip$time0, time2=hip$time1, event=hip$fracture, type="counting")
sum(hip$fracture==0) #Surv ends in + symbol when there is no event at time2 (hip$time1) in that row

hip[hip$id=="16",]

#------
#question 3

hip.km<-survfit(Surv(time0, time1, fracture, type="counting")~protect, data=hip)
plot(hip.km,mark.time=F,col=c("blue","red"))
legend(25,1,c("protect=0","protect=1"),col=c("blue","red"),lty=1)

ggsurvplot(hip.km, data = hip,conf.int = T,ylim=c(0,1),censor=F,xlab="Time in study",
           risk.table = T,legend="none")

hip.cox<-coxph(Surv(time0,time1,fracture)~protect,data=hip,ties="breslow")
summary(hip.cox)

#------
#question 4

hip.cox<-coxph(Surv(time0,time1,fracture)~protect+age+calcium,data=hip,ties="breslow")
summary(hip.cox)

#------
#question 5

hip$init_drug_level_50<-hip$init_drug_level/50
hip.cox<-coxph(Surv(time0,time1,fracture)~protect+age+calcium+init_drug_level_50,data=hip,ties="breslow")
summary(hip.cox)


#------
#question 6

hip.cox<-coxph(Surv(time0,time1,fracture)~protect+age+calcium+tt(init_drug_level_50),tt=function(x,t,...)(x*(t<=5)),
data=hip,ties="breslow")
summary(hip.cox)

hip.cox<-coxph(Surv(time0,time1,fracture)~protect+age+calcium+tt(init_drug_level_50),tt=function(x,t,...)(x*exp(-0.35*t)),
data=hip,ties="breslow")
summary(hip.cox)

#------
#question 7

failure.times<-unique(hip$time1[hip$fracture==1])
hip.split<-survSplit(hip,cut=failure.times,event="fracture",end="time1",start="time0")
hip.split<-hip.split[order(hip.split$id),]

hip.split$current_drug<-(hip.split$init_drug_level/10)*exp(-0.35*hip.split$time1)

hip.split.cox<-coxph(Surv(time0,time1,fracture)~protect+age+calcium+current_drug,
               data=hip.split,ties="breslow")
summary(hip.split.cox)

#an alternative way of doing this is given in the lecture notes, making use of the 'tmerge' function


###################################################
########################Part B#####################
###################################################

kidney<-read_csv("Datasets/kidney_frailty.csv")

summary(coxph(Surv(time, infect)~age+female+frailty(patient),data=kidney))

summary(survreg(Surv(time, infect)~age+female+frailty.gamma(patient),data=kidney))

