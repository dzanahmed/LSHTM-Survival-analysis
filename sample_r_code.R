#--- sample script for survival analysis to test 
#--- whether everything is working as it should be
#--- for the practicals in the Survival Analysis
#--- course at LSHTM


# if all functions return what is described in the comments after each command,
# then you are set up correctly and ready for the Survival Analysis practicals

#--- Authors: Tim Russell, Leah Pirondini, Ruth Keogh

#--- First of all, make sure you have the survival package, the flexsurv package
#--- the eha package and the survminer package. These are all the packages
#--- you'll need throughout the survival analysis 
#--- installed

#--- these commands load the libraries. Try these first, but if you get an error
#--- from any of the commands complaining that you don't have the library
#--- installed, then install the necessary library from the commands below
library(survival) 
library(flexsurv)
library(eha)
library(ggplot2)
library(survminer)

#--- these commands install the packages. If the command above runs without an 
#--- error, there is no need to run this line. However, its OK if you do run it,
#--- it will just re-install, taking a minute or two to do so. These are 
#--- commented out by default as its likely most of you will already have some.
#--- To run the commands, uncomment the lines (by deleting the # at the start
#--- of the line) and run the line. It should take a minute or so to install 
#--- each package
#install.packages("survival") 
#install.packages("flexsurv")
#install.packages("eha")
#install.packages("ggplot2")
#install.packages("survminer")

# if the package was not originally installed, but now it is, we need to load the library still. This is the same command as the first one and only needs to be run if the first command returned an error and the package was just installed for the first time
library(survival) 
library(flexsurv)
library(eha)
library(ggplot2)
library(survminer)
#------------ SECTION 1: testing "survival" package commands ------------------#

#--- these commands test whether the sample dataset, called "lung" in the survival package is loaded, by inspecting different bits of the dataset
head(lung) # this should return the first 5 rows of a dataset
class(lung) # this should return data.frame, as that is the "class" of lungs 
dim(lung) # this should return 228 10, the two dimensions of the data frame, 228 rows and 10 columns
View(lung) # this should allow you to look at the data frame

#--- we test whether the modelling functions of survival are working
s <- Surv(lung$time, lung$status) # saving an survival object called s
class(s) # this should return "Surv"
s # this should return a bit list of numbers

sfit <- survfit(Surv(time, status)~sex, data=lung) # testing more of the modelling functions of the survival package
plot(sfit) # this should bring up a plot of two survival functions over time

# if all functions return what is described in the comments, then you are set up correctly and ready for the Survival Analysis practicals


#------------ SECTION 2: testing "flexsurv" package commands ------------------#

#--- this line runs a simple survival model using a sample dataset from the 
#--- flexsurv package, named as an R variable "bc"
fs1 <- flexsurvreg(Surv(recyrs, censrec) ~ group, data = bc, dist = "weibull")

#--- this line plots the resulting survival curves. If a plot with three 
#--- red lines and three black lines appears, then its working correctly!
plot(fs1)

#--------------- SECTION 3: testing "eha" package commands --------------------#

#--- this line runs a simple survival model using a sample dataset from the 
#--- eha package, named as an R variable "oldmort"
fit <- coxreg(Surv(enter, exit, event) ~ sex, data = oldmort, method = "ml")

#--- this line plots the resulting model curves. If a plot with a single
#--- increasing black line appears, then its working correctly
plot(c(60, fit$hazards[[1]][, 1]), c(0, cumsum(fit$hazards[[1]][, 2])), 
     type = "l")

#------------ SECTION 4: testing "ggplot2" package commands -------------------#

#--- this command tests whetehr ggplot2 is working. When run, a colourful
#--- scatterplot should appear, using one of the class in-built test datasets
#--- in R, known as "mpg"
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()


# if all functions return what is described in the comments, then you are set up correctly and ready for the Survival Analysis practicals

#------------ SECTION 5: testing "survminer" package commands -----------------#

#--- a simple survival model using functions from the survival package, to test
#--- the plotting functions in survminer
fit <- survfit(Surv(time, status) ~ sex, data = lung)

#--- a plotting function from survminer. It it runs correctly a plot of two
#--- survival functions, one light blue, the other red should appear
ggsurvplot(fit, data = lung)