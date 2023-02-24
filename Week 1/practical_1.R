
# Change the below line to your directory path
setwd("C:\\Users\\Ruth Keogh\\OneDrive - London School of Hygiene and Tropical Medicine\\survivalanalysis_2021\\practical1")

###########################################################
#########Part A - Using survival data in R#################
#########PBC data##########################################
###########################################################

library(tidyverse)

#------
#load the data and take a look
pbc <- read_csv("Datasets/pbcbase_2021.csv")

names(pbc)
View(pbc)
summary(pbc)

#------
#question 1

table(pbc$d)

#------
#before questions 2 and 3: format datein and dateout as dates

pbc$datein=as.Date(pbc$datein,"%d%b%Y")
pbc$dateout=as.Date(pbc$dateout,"%d%b%Y")

#note that R stores dates as number of days since 1 January 1970
as.numeric(as.Date("1970-01-02"))

#------
#question 2

range(pbc$datein)

#------
#question 3

range(pbc$dateout)

#------
#question 4

pbc$days_in_study = pbc$dateout - pbc$datein

#------
#question 5
#install.packages("survival")
library(survival)

Surv(time=pbc$time,event=pbc$d)

Surv(time=as.numeric(pbc$dateout),event=pbc$d,origin=as.numeric(pbc$datein))

###########################################################
#########Part A - Using survival data in R#################
#########Whitehall data####################################
###########################################################

#------
#load the data and take a look
whl=read.table("whitehall.csv",sep=",",header=T)
names(whl)
View(whl)
summary(whl)

#------
#before question 6: format timein, timeout,and timebth as dates

whl$timein=as.Date(whl$timein,"%d%b%Y")
whl$timeout=as.Date(whl$timeout,"%d%b%Y")
whl$timebth=as.Date(whl$timebth,"%d%b%Y")

#------
#question 6

range(whl$timein)
range(whl$timeout)

#------
#question 7

sum(whl$timeout=="1987-01-30")
#1275 people left the study on the same day
#This will be the day the database was closed for analysis purposes

#------
#question 8

whl$timein=as.numeric(whl$timein)
whl$timeout=as.numeric(whl$timeout)
whl$timebth=as.numeric(whl$timebth)

Surv(time=whl$timeout,event=whl$chd,origin=whl$timein)
Surv(time=whl$timeout,event=whl$chd,origin=whl$timebth)
Surv(time=whl$timein,time2=whl$timeout,event=whl$chd,origin=whl$timebth)
Surv(time=whl$timein/365.25,time2=whl$timeout/365.25,event=whl$chd,origin=whl$timebth/365.25)

###########################################################
#########Part B - fitting exponential model################
###########################################################

mydata=read.table("surv_data_practical1.csv",sep=",",header=T)

View(mydata)

#------
#question 9: using survreg

exp.model = survreg(Surv(survtimes)~1,dist="exponential",data=mydata)
summary(exp.model)

#------
#question 9: using flexsurvreg
install.packages("flexsurv")
library(flexsurv)

exp.model2 = flexsurvreg(Surv(survtimes)~1,dist="exponential",data=mydata)
exp.model2

###########################################################
#################Part C - distributions####################
###########################################################

#------
#question 1: weibull model

wei.haz<-function(x,lambda,kappa){lambda*kappa*x^(kappa-1)}

curve(wei.haz(x,0.2,2),xlab="Time",ylab="Hazard function",col="red",ylim=c(0,1))
curve(wei.haz(x,0.4,2),add=T,col="blue")
curve(wei.haz(x,0.4,3),add=T,col="green")
curve(wei.haz(x,0.4,0.8),add=T,col="pink")
legend(0,1,c(expression(paste(kappa,"=2, ",lambda,"=0.2")),expression(paste(kappa,"=2, ",lambda,"=0.4")),
                 expression(paste(kappa,"=4, ",lambda,"=0.4")),expression(paste(kappa,"=0.8, ",lambda,"=0.4"))),
       col=c("red","blue","green","pink"),lty=c(1,1),bty="n")

#------
#question 2: log-logistic model

loglog.haz<-function(x,theta,kappa){(exp(theta)*kappa*x^(kappa-1))/(1+exp(theta)*x^(kappa))}

curve(loglog.haz(x,1,0.2),xlab="Time",ylab="Hazard function",col="red")
curve(loglog.haz(x,1,2),add=T,col="black")
curve(loglog.haz(x,3,2),add=T,col="blue")
