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
# Read in data and adjust for later analysis
###

# first read given data and adjust
data_given                <- read.csv(file="data/SandP_data.csv", header=TRUE, sep=";")
Date                      <- as.Date(data_given[,1]) 

# check whether all NA values in 'Date' are in the end of the vector 
sum(1:length(which(!is.na(Date)))==which(!is.na(Date)))==length(which(!is.na(Date)))
# truncate vectors to all values that we actually observed
Date                      <- Date[1:length(which(!is.na(Date)))]
# calculate daily returns from S&P index in percent
SandP                     <- (data_given[2:length(Date),2]/data_given[1:length(Date)-1,2]-1)*100
# calculate daily riskfree rate from annulaized riskfree in percent, discard first observation
riskfree                  <- data_given[2:length(Date),3]/365
# sum(is.na(riskfree)) # some values in the risk free rate are NA
# replace them with the last value that is not NA
for(i in 1:length(riskfree)){
  if(is.na(riskfree[i])==T){
    riskfree[i]               <- riskfree[i-1]
  }
} 
# calculate daily excess returns
E_ret                     <- SandP - riskfree
par(mar = c(4, 4, 0.1, 0.1), cex.lab = 0.95, cex.axis = 0.9,
    mgp = c(2, 0.7, 0), tcl = -0.3)
plot(Date[2:length(Date)], E_ret, type='l',xlab="Date",ylab="Excess returns in %")

#################### end first chunk

# create data frame of factors
Factors                   <- data.frame(Date)

# read in first factor
Factor_1                  <- read.csv(file="data/NASDAQCOM.csv", header=TRUE, sep=",",
                                      na.strings =".", stringsAsFactors=FALSE)
Factor_1[,1]              <- as.Date(Factor_1[,1])
# read in second factor
Factor_2                  <- read.csv(file="data/VIXCLS.csv", header=TRUE, sep=",",
                                      na.strings =".", stringsAsFactors=FALSE)
Factor_2[,1]              <- as.Date(Factor_2[,1])
# add column with respective values of Factor_1 and Factor_2 (or arbitrary amount of factors)
for(i in 1:nrow(Factors)){
  Factors[i,2]                  <- Factor_1[which(Date[i]==Factor_1[,1]),2]
  Factors[i,3]                  <- Factor_2[which(Date[i]==Factor_2[,1]),2]  
}
colnames(Factors)[2]      <- "NASDAQ"
colnames(Factors)[3]      <- "VIX"

# replace NAs with the last value that is not NA in all columns of data frame
for(j in 2:ncol(Factors)){
  for(i in 1:nrow(Factors)){
    if(is.na(Factors[i,j])==T){
      Factors[i,j]               <- Factors[i-1,j]
    }
  } 
}

summary(Factors)

#################### end second chunk

###
# Model
###

# choose in-sample



###
# End
###

# Discard data before and after period of interest
Factor_1                  <- Factor_1[which(Factor_1[,1]==Date[1]):
                                        which(Factor_1[,1]==Date[length(Date)]),]

# replace NAs with the last value that is not NA in all columns of data frame
for(i in 1:nrow(Factor_1)){
  if(is.na(Factor_1[i,2])==T){
    Factor_1[i,2]               <- Factor_1[i-1,2]
  }
} 
