library(survival)
library(tidyverse)
library(survminer)

pbc <- read.csv(file='Datasets/pbcbase_2021.csv')

pbc$datein <- as.Date(pbc$datein, "%d%b%Y")
pbc$dateout <- as.Date(pbc$dateout, "%d%b%Y")

