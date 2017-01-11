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
E_ret                     <- data.frame(Date[2:length(Date)],SandP - riskfree)
colnames(E_ret)           <- c("Date", "E_ret")
par(mar = c(4, 4, 0.1, 0.1), cex.lab = 0.95, cex.axis = 0.9,
    mgp = c(2, 0.7, 0), tcl = -0.3)
plot(E_ret[,1], E_ret[,2], type='l',xlab="Date",ylab="Excess returns in %")

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
NrFact                    <- 2 # enter how many factors you included in the end

# replace NAs with the last value that is not NA in all columns of data frame
for(j in 2:ncol(Factors)){
  for(i in 1:nrow(Factors)){
    if(is.na(Factors[i,j])==T){
      Factors[i,j]               <- Factors[i-1,j]
    }
  } 
}

summary(Factors)

# add day-of-the-week dummies for Tuesday-Friday (Monday being the intercept)
# if running system is English, change names in vector
DayDummies                <- c("Dienstag","Mittwoch","Donnerstag","Freitag")
for(d in 1:length(DayDummies)){
  for(i in 1:nrow(Factors)){
    if(weekdays(Factors[i,1])==DayDummies[d]){
      Factors[i,NrFact+1+d]                  <- 1
    }
    else {
      Factors[i,NrFact+1+d]                  <- 0
    }
  }
}
colnames(Factors)[(1+NrFact+1):(
  1+NrFact+length(DayDummies))]      <- c("Tue","Wed","Thu","Fri")

#################### end second chunk


###
# Model
###
library(forecast, quietly = T)

# are the series stationary?
# adf.test(E_ret[,2]) # E_ret is
# for(i in 2:ncol(Factors)){
#   adf.test(Factors[,i]) # Factors are as well
# }

# choose in-sample for model estimation
fit                       <- Arima(#
        # dependent var from 2nd observation to 31.12.2015
        E_ret[2:which(E_ret[,1]=="2015-12-31"),2], 
        # factors and dummies as explanatory variables
        xreg = Factors[2:which(Factors[,1]=="2015-12-30"),2:ncol(Factors)],
        # choose order of ARIMA(p,d,q) model you want to estimate
        order = c(2,0,2)
        )#
summary(fit)
err_in                  <- resid(fit) # store the model residuals
tsdisplay(arima.errors(fit), main="ARIMA(2,0,2) errors") # assess model errors
Box.test(residuals(fit),fitdf=3,lag=10,type="Ljung") # check for serial correlation in errors
# in-sample R^2
R_sq                    <- 1-sum((residuals(fit))^2)/
        sum((E_ret[2:which(E_ret[,1]=="2015-12-31"),2]-
        mean(E_ret[2:which(E_ret[,1]=="2015-12-31"),2]))^2)

#################### end third chunk

# h step ahead forecast
fcast                   <- forecast(fit,
                            xreg=
                              Factors[2:which(Factors[,1]=="2015-12-30"),2:ncol(Factors)],
                              h=which(Factors[,1]=="2016-09-30")-which(Factors[,1]=="2015-12-30")-1)
plot(fcast)



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
