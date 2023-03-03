
library(survival)
library(ggplot2)
library(survminer)

# Change the below line to your directory path
setwd("practical4")

##############################################################
########################Part A - PBC data#####################
##############################################################

#------
#load the data and take a look
pbc=read.table("pbcbase_2021.csv",sep=",",header=T)

pbc$datein=as.Date(pbc$datein,"%d%b%Y")
pbc$dateout=as.Date(pbc$dateout,"%d%b%Y")

#------
#question 1
#pen and paper exercise
#note the times are recorded to more than 3 decimal places

sum(pbc$time>=0.052 & pbc$time<=0.053) #one person has 'time' in this range
pbc$d[pbc$time>=0.052 & pbc$time<=0.053]#they have the event
pbc$time[pbc$time>=0.052 & pbc$time<=0.053] #this is their actual recorded time
pbc$id[pbc$time>=0.052 & pbc$time<=0.053] #this is the ID number of the person who has the event a time 0.052

pbc$treat[pbc$id==922]#thisperson is in treatment group 1, i.e. x=0

#now let's look at all individuals at risk at time 0.052
#there are 181 people in the risk set at tiem 0.052 (which includes person 922 who had the event)
index.time=pbc$time[pbc$time>=0.052 & pbc$time<=0.053]
sum(pbc$time>=index.time)

table(pbc$treat[pbc$time>=index.time])#89 have x=0 and 92 have x=1

#------
#question 2

pbc.km <- survfit(Surv(time,d)~as.factor(treat),data=pbc)

summary(pbc.km)

#------
#question 3

pbc.cox<-coxph(Surv(time,d)~as.factor(treat),data=pbc)
summary(pbc.cox)

#------
#question 4

pbc.survfit=survfit(pbc.cox,newdata=data.frame(treat=c(1,2)))
summary(pbc.survfit)

plot(pbc.survfit,mark.time=F,col=c("black","grey"),xlab="Time",ylab="Estimated survivor function")
legend(8,1,c("Placebo","Active"),col=c("black","grey"),lty=1,cex=0.5)

#this can also be done using ggadjustedcurves
#however, I don't find the documentation for this function very good, and it's unclear what it is doing in more complex situations
ggadjustedcurves(pbc.cox, data = pbc,variable="treat",legend.title="Treatment group")

#------
#question 5

plot(survfit(pbc.cox,newdata=data.frame(treat=c(1,2))),
     col=c("blue","red"),xlab="time",ylab="S(t)")
lines(pbc.km,mark.time=F,col=c("blue","red"),lty=2,add=T)
legend(8,1,c("Placebo, Cox","Active, Cox",
             "Placebo, Kaplan-Meier","Active, Kaplan-Meier"),
       col=c("blue","red","blue","red"),lty=c(1,1,2,2),cex=0.5)

plot(pbc.km,fun="cloglog",xlab="time (log scale)",ylab="log(-log S(t))",
     col=c("blue","red"),xlim=c(0.02,12))
legend(0.02,0,c("Placebo","Active"),col=c("blue","red"),lty=1,cex=0.5)

ggsurvplot(pbc.km, data = pbc,conf.int = T,fun="cloglog",censor=F,legend.title="",
           legend.labs = c("Placebo","Active"))

#------
#question 6
#pen and paper exercise

#------
#question 7

pbc.cox<-coxph(Surv(time,d)~as.factor(treat)+bil0,data=pbc)
summary(pbc.cox)

#measuring bilirubin in units of 50 instead of units of 1
pbc.cox2<-coxph(Surv(time,d)~as.factor(treat)+I(bil0/50),data=pbc)
summary(pbc.cox2)

#equivalence between results from the 2 models (for bilirubin): log HRs
pbc.cox$coefficients["bil0"]*50
pbc.cox2$coefficients["I(bil0/50)"]

#equivalence between results from the 2 models (for bilirubin): HR
exp(pbc.cox$coefficients["bil0"]*50)
exp(pbc.cox$coefficients["bil0"])^50
exp(pbc.cox2$coefficients["I(bil0/50)"])

#------
#question 8

#(a)

# hr
exp(pbc.cox$coef[1]+75*pbc.cox$coef[2])/exp(pbc.cox$coef[1]+30*pbc.cox$coef[2])
# ci upper
exp(45*pbc.cox$coef[2] + 1.96*sqrt(45^2 * vcov(pbc.cox)[2,2]))
# ci lower
exp(45*pbc.cox$coef[2] - 1.96*sqrt(45^2 * vcov(pbc.cox)[2,2]))

#(b)

# hr
exp(75*pbc.cox$coef[2])/exp(30*pbc.cox$coef[2])
# ci - same as (a)

#(c)

# hr
exp(pbc.cox$coef[1]+75*pbc.cox$coef[2])/exp(30*pbc.cox$coef[2])
# ci upper
exp(pbc.cox$coef[1] + 45*pbc.cox$coef[2] +
            1.96*sqrt(vcov(pbc.cox)[1,1] + 45^2*vcov(pbc.cox)[2,2] + 2*45*vcov(pbc.cox)[1,2]))
# ci lower
exp(pbc.cox$coef[1] + 45*pbc.cox$coef[2] -
            1.96*sqrt(vcov(pbc.cox)[1,1] + 45^2*vcov(pbc.cox)[2,2] + 2*45*vcov(pbc.cox)[1,2]))

#------
#question 9

pbc.survfit.trt1.bil15=survfit(pbc.cox,newdata=data.frame(treat=1,bil0=15))
pbc.survfit.trt1.bil30=survfit(pbc.cox,newdata=data.frame(treat=1,bil0=30))
pbc.survfit.trt1.bil45=survfit(pbc.cox,newdata=data.frame(treat=1,bil0=75))

pbc.survfit.trt2.bil15=survfit(pbc.cox,newdata=data.frame(treat=2,bil0=15))
pbc.survfit.trt2.bil30=survfit(pbc.cox,newdata=data.frame(treat=2,bil0=30))
pbc.survfit.trt2.bil45=survfit(pbc.cox,newdata=data.frame(treat=2,bil0=75))

plot(pbc.survfit.trt1.bil15$time,pbc.survfit.trt1.bil15$surv,type="s",col="black",xlab="Time",ylab="Estimated survivor curve",ylim=c(0,1))
lines(pbc.survfit.trt2.bil15$time,pbc.survfit.trt2.bil15$surv,type="s",lty=2,col="black")

lines(pbc.survfit.trt1.bil30$time,pbc.survfit.trt1.bil30$surv,type="s",lty=1,col="red")
lines(pbc.survfit.trt2.bil30$time,pbc.survfit.trt2.bil30$surv,type="s",lty=2,col="red")

lines(pbc.survfit.trt1.bil45$time,pbc.survfit.trt1.bil45$surv,type="s",lty=1,col="blue")
lines(pbc.survfit.trt2.bil45$time,pbc.survfit.trt2.bil45$surv,type="s",lty=2,col="blue")

legend(0,0.5,c("treat=1,bil0=15","treat=2,bil0=15",
               "treat=1,bil0=30","treat=2,bil0=30",
               "treat=1,bil0=75","treat=2,bil0=75"),
       lty=c(1,2,1,2,1,2),col=c("black","black","red","red","blue","blue"))

##############################################################
########################Part B - alloauto data################
##############################################################

#------
#load the data and take a look
allo=read.table("alloauto.csv",sep=",",header=T)

View(allo)

#------
#question 1

allo.km <- survfit(Surv(time,delta)~as.factor(type),data=allo)
ggsurvplot(allo.km, data = allo,conf.int = T,censor=F,legend.title="",
           legend.labs = c("Allogenic","Autologous"))

#------
#question 2

plot(allo.km,fun="cloglog",xlab="time (log scale)",ylab="log(-log S(t))",
     col=c("blue","red"))
legend(0.02,0,c("Allogenic","Autologous"),col=c("blue","red"),lty=1,cex=0.5)

ggsurvplot(allo.km, data = allo,conf.int = T,fun="cloglog",censor=F,legend.title="",
           legend.labs = c("Allogenic","Autologous"))


#comparing survivor curves from a Cox model with the Kaplan-Meier survivor curves

allo.cox <- coxph(Surv(time,delta)~as.factor(type),data=allo)

par(mfrow=c(1,2))
plot(survfit(allo.cox,newdata=data.frame(type=c(1,2))),
     col=c("blue","red"),xlab="time",ylab="S(t)",main="Cox")
plot(allo.km,mark.time=F,col=c("blue","red"),xlab="time",ylab="S(t)",main="Kaplan-Meier")
