##################################################################
## Evaluation Experiment Replication Schulz-Hardt et al. (2000) ##
##################################################################

#Set working directory 
setwd("~/Desktop/Psychologie Master/Masterarbeit /MA_R-Skripte/Replication_Schulz-Hardt-et-al-2000")

##############
## Packages ##
#############

#Load packages
if(!require(psych)){install.packages("psych")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(BayesFactor)){install.packages("BayesFactor")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(effsize)){install.packages("effsize")}
if(!require(janitor)){install.packages("janitor")}

#Activate packages
library(psych)
library(tidyverse)
library(BayesFactor)
library(ggplot2)
library(effsize)
library(janitor)

###############
## Read data ##
###############

#Data import
ds<- readxl::read_excel("Datenblatt_Simulation_Schulz-Hardt et al. (2000).xlsx")
View(ds)

glimpse(ds)
str(ds)

######################
## Data preparation ##
######################

#Remove uneccessary columns 
ds[,c(6,8, )] <-NULL

glimpse(ds)

################################
## Testing exclusion criteria ##
################################

#Missings: No missings 
anyNA(ds)
which(is.na())
sum(is.na())
table(is.na(ds))

# overview missingsin all variables (columns)
colSums(is.na(ds))

# Overview missings in all participants (row)
rowSums(is.na(daten))

#Visalization of missings
VIM::summary(aggr(daten)

############################
## Subject characteristic ##
############################

#Gender
ds$gen <- factor(ds$gen, 
                  levels = c(1, 2, 3), 
                  labels = c("female", "male", "diverse"),
                  exclude = NA)
table(ds$gen)

#Age
psych::describe(ds$age)


