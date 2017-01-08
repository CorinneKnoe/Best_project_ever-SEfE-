##############################################
#######    Knoepfel, Krosigk - SEfE    #######
##############################################

##########
##    Misc
##########

# rm(list = ls())
# setwd("C:/Users/Admin/Google Drive/Uni/HSG/7,610,1.00 Software Engineering for Economists/Project/Best_project_ever_SEfE")
#library(zoo, package ="zoo")
#library(xts, package ="xts")
#library(timeDate)
library(lmtest, quietly = T)
library(AER, quietly = T)
library(dynlm, quietly = T)
library(tseries, quietly = T)
library(forecast, quietly = T)

###
# Read in data
###

# first read given data and adjust
data_given                <- read.csv(file="data/SandP_data.csv", header=TRUE, sep=";")
Date                      <- as.Date(data_given[,1]) 

# check whether all NA values in 'Date' are in the end of the vector 
sum(1:length(which(!is.na(Date)))==which(!is.na(Date)))==length(which(!is.na(Date)))
# truncate vectors to all values that we actually observed
Date                      <- Date[1:length(which(!is.na(Date)))]
SandP                     <- data_given[1:length(Date),2]
riskfree                  <- data_given[1:length(Date),3]


# 'skip' option to start reading from csv in later row

###
# End
###