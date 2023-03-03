
library(survival)
library(eha)
library(flexsurv)
library(ggplot2)
library(survminer)

# Change the below line to your directory path
setwd("practical3")

#------
#function used later for obtaining 95% confidence intervals from weibreg or phreg

confint.95<-function(mod){
  ci95.lower=mod$coefficients-1.96*sqrt(diag(mod$var))
  ci95.upper=mod$coefficients+1.96*sqrt(diag(mod$var))
  ci95<-cbind(ci95.lower,ci95.upper)
  rownames(ci95)<-names(mod$coefficients)
  return(ci95)
}

#------
#load the data and format the dates

whl<-read.table("whitehall.csv",sep=",",header=T)

whl$timein=as.Date(whl$timein,"%d%b%Y")
whl$timeout=as.Date(whl$timeout,"%d%b%Y")
whl$timebth=as.Date(whl$timebth,"%d%b%Y")

whl$timein=as.numeric(whl$timein)/365.25
whl$timeout=as.numeric(whl$timeout)/365.25
whl$timebth=as.numeric(whl$timebth)/365.25

#------
#question 1

table(whl$grade, whl$chd)

whl$time=(whl$timeout-whl$timein)

median(whl$time[whl$grade==1 & whl$chd==0])
median(whl$time[whl$grade==1 & whl$chd==1])
median(whl$time[whl$grade==2 & whl$chd==0])
median(whl$time[whl$grade==2 & whl$chd==1])

#------
#question 2

whl.km <- survfit(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade),data=whl)
ggsurvplot(whl.km, data = whl,conf.int = T,ylim=c(0.8,1),censor=F,legend.title="Job grade",legend.labs = c("Grade 1","Grade 2"))

summary(whl.km,times=c(5,10,15))

survdiff(Surv(time=timeout,event=chd,origin=timein)~grade,data=whl)

#------
#question 3

#using weibreg with shape=1
whl.exp<- weibreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade),shape=1,data=whl)
whl.exp
confint.95(whl.exp)

#using flexsurvreg
whl.exp2<- flexsurvreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade),dist="exponential",data=whl)
whl.exp2

#using survreg
whl.exp3<- survreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade),dist="exponential",data=whl)
summary(whl.exp3)

#changing the timescale
#note that survreg and phreg do not allow delayed entry so we have to use weibreg or flexsurvreg here
whl.exp4<- weibreg(Surv(time=timein,time2=timeout,event=chd,origin=timebth)~as.factor(grade),shape=1,data=whl)
whl.exp4
confint.95(whl.exp4)

#------
#question 4

#using weibreg
whl.weib<- weibreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade),data=whl)
whl.weib
confint.95(whl.weib)

#using flexsurvreg
whl.weib2<- flexsurvreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade),dist="weibull",data=whl)
whl.weib2

#using survreg
whl.weib3<- survreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade),dist="weibull",data=whl)
summary(whl.weib3)

#------
#question 5

whl.km <- survfit(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade),data=whl)
ggsurvplot(whl.km, data = whl,conf.int = T,fun="cloglog",censor=F,legend.title="Job grade",legend.labs = c("Grade 1","Grade 2"))

#alternative basic plot
plot(whl.km,conf.int=T,col=c("red","black"),mark.time=F,xlab="log time", 
     ylab="log(-log S(t))",fun="cloglog")

#------
#question 6

whl.weib.age<- weibreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade)+agein,data=whl)
whl.weib.age
confint.95(whl.weib.age)

#------
#question 7

whl$agecat<-cut(whl$agein, breaks=c(40,50,55,60,65,70),right=F,labels=F)

whl.km.agecat1 <- survfit(Surv(time=timeout,event=chd,origin=timein)~grade,data=subset(whl,agecat==1))
whl.km.agecat2 <- survfit(Surv(time=timeout,event=chd,origin=timein)~grade,data=subset(whl,agecat==2))
whl.km.agecat3 <- survfit(Surv(time=timeout,event=chd,origin=timein)~grade,data=subset(whl,agecat==3))
whl.km.agecat4 <- survfit(Surv(time=timeout,event=chd,origin=timein)~grade,data=subset(whl,agecat==4))
whl.km.agecat5 <- survfit(Surv(time=timeout,event=chd,origin=timein)~grade,data=subset(whl,agecat==5))

ggsurvplot(whl.km.agecat1, data = whl,conf.int = T,fun="cloglog",censor=F,legend.title="Job grade",legend.labs = c("Grade 1","Grade 2"))
ggsurvplot(whl.km.agecat2, data = whl,conf.int = T,fun="cloglog",censor=F,legend.title="Job grade",legend.labs = c("Grade 1","Grade 2"))
ggsurvplot(whl.km.agecat3, data = whl,conf.int = T,fun="cloglog",censor=F,legend.title="Job grade",legend.labs = c("Grade 1","Grade 2"))
ggsurvplot(whl.km.agecat4, data = whl,conf.int = T,fun="cloglog",censor=F,legend.title="Job grade",legend.labs = c("Grade 1","Grade 2"))
ggsurvplot(whl.km.agecat5, data = whl,conf.int = T,fun="cloglog",censor=F,legend.title="Job grade",legend.labs = c("Grade 1","Grade 2"))

#------
#question 8

#look at the shape parameter estimate in whl.weib.age

#alternatively use a likelihood ratio test

whl.exp.age<- weibreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade)+agein,shape=1,data=whl)
whl.exp.age

whl.weib.age<- weibreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade)+agein,data=whl)
whl.weib.age

teststat=-2*(whl.exp.age$loglik[2]-whl.weib.age$loglik[2]) #test statistics
teststat
1-pchisq(teststat,df=1)#p-value from chi-squared test with 1-df

#------
#question 9

#------
#using weibreg (which unfortunately does not work with 'predict') 

whl.weib.age<- weibreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade)+agein,data=whl)

survplot.weibreg<-function(mod,t,grade,agein){
  beta<-mod$coefficients[c("as.factor(grade)2","agein")]
  kappa<-exp(mod$coefficients["log(shape)"])
  lambda<-exp(-kappa*mod$coefficients["log(scale)"])
  surv<-exp(-lambda*(t^kappa)*exp(beta[1]*(grade-1)+beta[2]*agein))
  return(surv)
}

curve(survplot.weibreg(whl.weib.age,x,grade=1,agein=45),lty=1,col="red",xlab="Time",ylab="Survival probability",
      ylim=c(0,1),xlim=c(0,20))
curve(survplot.weibreg(whl.weib.age,x,grade=2,agein=45),add=T,lty=2,col="red")

curve(survplot.weibreg(whl.weib.age,x,grade=1,agein=55),add=T,lty=1,col="green")
curve(survplot.weibreg(whl.weib.age,x,grade=2,agein=55),add=T,lty=2,col="green")

curve(survplot.weibreg(whl.weib.age,x,grade=1,agein=65),add=T,lty=1,col="blue")
curve(survplot.weibreg(whl.weib.age,x,grade=2,agein=65),add=T,lty=2,col="blue")

#using survreg (which enables use of 'predict')

whl.weib.age<- survreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade)+agein,dist="weibull",data=whl)
whl.weib.age

plot(predict(whl.weib.age, newdata=data.frame(grade=1,agein=45),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),type="l",col="red",xlab="Time (years since study entry)",
     ylab="Survivor function: S(t)",xlim=c(0,20),ylim=c(0,1))
lines(predict(whl.weib.age, newdata=data.frame(grade=2,agein=45),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),type="l",lty=2,col="red")

lines(predict(whl.weib.age, newdata=data.frame(grade=1,agein=55),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),type="l",col="green")
lines(predict(whl.weib.age, newdata=data.frame(grade=2,agein=55),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),type="l",lty=2,col="green")

lines(predict(whl.weib.age, newdata=data.frame(grade=1,agein=65),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),type="l",col="blue")
lines(predict(whl.weib.age, newdata=data.frame(grade=2,agein=65),type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),type="l",lty=2,col="blue")

#using flexsurvreg (which unfortunately does work with 'predict') 
#Note someone has written a function for this in the tidysurv package, which we do not use here.
whl.weib.age<- flexsurvreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade)+agein,dist="weibull",data=whl)
whl.weib.age

pred.grade1.age45=summary(whl.weib.age, newdata=data.frame(grade=1,agein=45))$"as.factor(grade)=1, agein=45"
pred.grade2.age45=summary(whl.weib.age, newdata=data.frame(grade=2,agein=45))$"as.factor(grade)=2, agein=45"

pred.grade1.age55=summary(whl.weib.age, newdata=data.frame(grade=1,agein=55))$"as.factor(grade)=1, agein=55"
pred.grade2.age55=summary(whl.weib.age, newdata=data.frame(grade=2,agein=55))$"as.factor(grade)=2, agein=55"

pred.grade1.age65=summary(whl.weib.age, newdata=data.frame(grade=1,agein=65))$"as.factor(grade)=1, agein=65"
pred.grade2.age65=summary(whl.weib.age, newdata=data.frame(grade=2,agein=65))$"as.factor(grade)=2, agein=65"

plot(pred.grade1.age45$time, pred.grade1.age45$est,type="l",col="red",xlab="Time (years since study entry)",
     ylab="Survivor function: S(t)",xlim=c(0,20),ylim=c(0,1))
lines(pred.grade2.age45$time, pred.grade2.age45$est,type="l",lty=2,col="red")

lines(pred.grade1.age55$time, pred.grade1.age55$est,type="l",lty=1,col="green")
lines(pred.grade2.age55$time, pred.grade2.age55$est,type="l",lty=2,col="green")

lines(pred.grade1.age65$time, pred.grade1.age65$est,type="l",lty=1,col="blue")
lines(pred.grade2.age65$time, pred.grade2.age65$est,type="l",lty=2,col="blue")

##############################################################
########################Extra exercises#######################
##############################################################

#------
#question 1

whl[whl$id %in% c(5001,5350),c("id","timein","timeout","time","chd")]

whl.split<- survSplit(Surv(time=timeout,chd,origin=timein)~., whl, cut=c(0,5,10,15,20), episode="period")

whl.split[whl.split$id %in% c(5001,5350),c("id","tstart","timeout","chd","period")]

whl.split.exp<- weibreg(Surv(time=tstart,time2=timeout,event=chd)~as.factor(grade)+agein+as.factor(period),
                        shape=1,data=whl.split)
whl.split.exp
confint.95(whl.split.exp)

whl.split.exp<- flexsurvreg(Surv(time=tstart,time2=timeout,event=chd)~as.factor(grade)+agein+as.factor(period),dist="exponential",data=whl.split)
whl.split.exp

#------
#question 2

whl.pois=glm(chd~as.factor(grade)+offset(log(time)),family="poisson",data=whl)
summary(whl.pois)

whl.exp<- weibreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade),shape=1,data=whl)
whl.exp
confint.95(whl.exp)

whl.exp<- survreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade),dist="exponential",data=whl)
summary(whl.exp)

whl.exp<- flexsurvreg(Surv(time=timeout,event=chd,origin=timein)~as.factor(grade),dist="exponential",data=whl)
whl.exp

