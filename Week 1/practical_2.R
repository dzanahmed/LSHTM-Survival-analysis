library(survival)
library(tidyverse)
library(survminer)

##############################################################
########################part 1 - PBC data#####################
##############################################################

##question 1##

pbc <- read_csv("Datasets/pbcbase_2021.csv")

head(pbc)

# Overall
pbc |> count(d)
pbc |> summarise(median=median(time))

# By groups
pbc |> group_by(treat, d) |> summarise(n=n(), median=median(time)) 

##question 2##

pbc.km <- survfit(Surv(time, d) ~ 1, data = pbc)

plot(pbc.km,
     conf.int = F,
     xlab = "Time",
     ylab = "Survivor function")

summary(pbc.km)
#Prob of survival beyond 1 year is ~90%
#Prob of survival beyond 5 years is 

ggsurvplot(pbc.km)

?survfit

# Question 3
qq19 <- 0.7656 * (1 - 0 / 19)
qq18 <- qq19 * (1 - 1 / 18)
qq17 <- qq18 * (1 - 1 / 17)
qq16 <- qq17 * (1 - 1 / 16)
qq15 <- qq16 * (1 - 1 / 15) #0.5954
qq14 <- qq15 * (1 - 0 / 14)
qq13 <- qq14 * (1 - 1 / 13)
qq12 <- qq13 * (1 - 0 / 12)
qq11 <- qq12 * (1 - 1 / 11)
##question 4##

pbc.km <- survfit(Surv(time,d)~treat,data=pbc)

summary(pbc.km)

# GGPlot version
ggsurvplot(pbc.km, conf.int = T)

# Base R
par(mfrow=c(1,2))

plot(pbc.km,conf.int=F,col=c("black","red"),mark.time=F,xlab="Time", ylab="Survivor function")

legend (0.2,0.2,c("Placebo","Active treatment"),col=c("black","red"),lty=c(1,1))

plot(pbc.km,conf.int=T,col=c("black","red"),mark.time=T,xlab="Time", ylab="Survivor function")

##question 6##
##question 5##

survdiff(Surv(time,d)~treat,data=pbc)

# p = 0.5 for log renk test


# Question 6
pbc.km1 <- survfit(Surv(time,d)~1,data=subset(pbc,pbc$treat==1))
pbc.km2 <- survfit(Surv(time,d)~1,data=subset(pbc,pbc$treat==2))
cumhaz.1<-cumsum(pbc.km1$n.event/pbc.km1$n.risk)
cumhaz.2<-cumsum(pbc.km2$n.event/pbc.km2$n.risk)

par(mfrow=c(1,2))

plot(pbc.km1$time,cumhaz.1,type="s",col="black",xlab="Time",ylab="Cumulative hazard",xlim=c(0,12),ylim=c(0,2.2))

lines(pbc.km2$time,cumhaz.2,type="s",col="red")

legend (0,2,c("Placebo","Active treatment"),col=c("black","red"),lty=c(1,1))

plot(pbc.km,conf.int=F,col=c("black","red"),mark.time=F,xlab="Time", ylab="Cumulative hazard",fun="cumhaz",xlim=c(0,12),ylim=c(0,2.2))


##############################################################
########################part 2 - whitehall####################
##############################################################
  
whl <- read_csv("Datasets/whitehall.csv")
  
##question 1##

whl$timein=as.Date(whl$timein,"%d%b%Y")
whl$timeout=as.Date(whl$timeout,"%d%b%Y")
whl$timebth=as.Date(whl$timebth,"%d%b%Y")

par(mfrow = c(1, 1))

whl.km <-
  survfit(Surv(time = (timeout - timein) / 365.25, event = chd) ~ 1, data =  whl)

ggsurvplot(whl.km, pval=T)

?ggsurvplot

plot(
  whl.km,
  conf.int = T,
  mark.time = F,
  xlab = "Time",
  ylab = "Survivor function",
  ylim = c(0.8, 1)
)

plot(
  whl.km,
  conf.int = T,
  mark.time = F,
  xlab = "Time",
  ylab = "Survivor function"
)

#An alternative way of specifying the timescale using Surv(), which gives identical results


whl.km2 <-
  survfit(Surv(
    as.numeric(timein),
    as.numeric(timeout),
    event = chd,
    origin = as.numeric(timein)
  ) ~ 1,
  data =
    whl)

ggsurvplot(whl.km2)

plot(
  whl.km2,
  conf.int = T,
  mark.time = F,
  xlab = "Time",
  ylab = "Survivor function",
  ylim = c(0.8, 1),
  xscale = 365.25
)

##question 2##



whl.km <-
  survfit(Surv(time = (timeout - timein) / 365.25, event = chd) ~ sbpgrp, data =
            whl)


ggsurvplot(whl.km,
           pval=TRUE,
           conf.int=TRUE,
           risk.table=TRUE,
           tables.theme = theme_cleantable(),
           ggtheme = theme_bw(),
           ylim=c(0.6,1),
           title = "K-M Curve",
           legend = "bottom"
)

plot(
  whl.km,
  conf.int = F,
  mark.time = F,
  xlab = "Time",
  ylab = "Survivor function",
  ylim = c(0.75, 1),
  col = c("blue", "red", "green", "yellow")
)
legend (
  1,
  0.9,
  c("group 1", "group 2", "group 3", "group 4"),
  col = c("blue", "red", "green", "yellow"),
  lty = c(1, 1)
)

##question 3##

survdiff(Surv(time=(timeout-timein)/365.25,event=chd)~sbpgrp,data=whl)
  