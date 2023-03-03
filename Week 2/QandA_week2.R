

library(survival)

install.packages("KMunicate")
library(KMunicate)

#load the pbc data
pbc=read.table("pbcbase_2021.csv",sep=",",header=T)

#Kaplan-Meier analysis by treatment group
pbc.km <- survfit(Surv(time,d)~treat,data=pbc)

#KMunicate plot
#windows(10,7)
time_scale <- seq(0, 12, by = 2)
KMunicate(fit = pbc.km, time_scale = time_scale)


