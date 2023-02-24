library(survival)

##############################################################
########################part 1 - PBC data#####################
##############################################################

##question 1##

pbc <- read_csv("Datasets/pbcbase_2021.csv")

head(pbc)

##question 2##

pbc.km <- survfit(Surv(time,d)~1,data=pbc)
plot(pbc.km,conf.int=F,xlab="Time", ylab="Survivor function")

summary(pbc.km)

##question 4##

pbc.km <- survfit(Surv(time,d)~treat,data=pbc)

summary(pbc.km)

par(mfrow=c(1,2))

plot(pbc.km,conf.int=F,col=c("black","red"),mark.time=F,xlab="Time", ylab="Survivor function")

legend (0.2,0.2,c("Placebo","Active treatment"),col=c("black","red"),lty=c(1,1))

plot(pbc.km,conf.int=T,col=c("black","red"),mark.time=T,xlab="Time", ylab="Survivor function")

##question 5##

pbc.km1 <- survfit(Surv(time,d)~1,data=subset(pbc,pbc$treat==1))
pbc.km2 <- survfit(Surv(time,d)~1,data=subset(pbc,pbc$treat==2))
cumhaz.1<-cumsum(pbc.km1$n.event/pbc.km1$n.risk)
cumhaz.2<-cumsum(pbc.km2$n.event/pbc.km2$n.risk)

par(mfrow=c(1,2))

plot(pbc.km1$time,cumhaz.1,type="s",col="black",xlab="Time",ylab="Cumulative hazard",xlim=c(0,12),ylim=c(0,2.2))

lines(pbc.km2$time,cumhaz.2,type="s",col="red")

legend (0,2,c("Placebo","Active treatment"),col=c("black","red"),lty=c(1,1))

plot(pbc.km,conf.int=F,col=c("black","red"),mark.time=F,xlab="Time", ylab="Cumulative hazard",fun="cumhaz",xlim=c(0,12),ylim=c(0,2.2))

##question 5##

survdiff(Surv(time,d)~treat,data=pbc)

##############################################################
########################part 2 - whitehall####################
##############################################################
  
whl <- read_csv("Datasets/whitehall.csv")
  
##question 1##
  
par(mfrow=c(1,1))

whl.km <- survfit(Surv(time=(timeout-timein)/365.25,event=chd)~1,data=whl)
plot(whl.km,conf.int=T,mark.time=F,xlab="Time", ylab="Survivor function",
     ylim=c(0.8,1))
plot(whl.km,conf.int=T,mark.time=F,xlab="Time", ylab="Survivor function")


#An alternative way of specifying the timescale using Surv(), which gives identical results

whl.km2<- survfit(Surv(timein,timeout,event=chd,origin=timein)~1,data=whl)
plot(whl.km2,conf.int=T,mark.time=F,xlab="Time", ylab="Survivor function",ylim=c(0.8,1),xscale=365.25)
  
##question 2##

whl.km <- survfit(Surv(time=(timeout-timein)/365.25,event=chd)~sbpgrp,data=whl)

plot(whl.km,conf.int=F,mark.time=F,xlab="Time", ylab="Survivor function",ylim=c(0.75,1),
col=c("blue","red","green","yellow"))
legend (1,0.9,c("group 1","group 2","group 3","group 4"),col=c("blue","red","green","yellow"),lty=c(1,1))

##question 3##

survdiff(Surv(time=(timeout-timein)/365.25,event=chd)~sbpgrp,data=whl)
  