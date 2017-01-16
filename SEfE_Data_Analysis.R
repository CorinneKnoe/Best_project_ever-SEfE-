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
#Factor_1                  <- read.csv(file="data/NASDAQCOM.csv", header=TRUE, sep=",",
#                                      na.strings =".", stringsAsFactors=FALSE)
Factor_1                  <- read.csv(file="data/TEDRATE.csv", header=TRUE, sep=",",
                                      na.strings =".", stringsAsFactors=FALSE)
Factor_1[,1]              <- as.Date(Factor_1[,1])
# read in second factor
Factor_2                  <- read.csv(file="data/VIXCLS.csv", header=TRUE, sep=",",
                                      na.strings =".", stringsAsFactors=FALSE)
Factor_2[,1]              <- as.Date(Factor_2[,1])
# add column with respective values of Factor_1 and Factor_2 (or arbitrary amount of factors)
for(i in 1:nrow(Factors)){
  Factors[i,2]                  <- Factor_1[which(Date[i]==Factor_1[,1]),2]
  Factors[i,3]                  <- (Factor_2[which(Date[i]==Factor_2[,1]),2]-Factor_2[which(Date[i]==Factor_2[,1])-1,2]
                                    )/Factor_2[which(Date[i]==Factor_2[,1])-1,2]*100
#  Factors[i,3]                  <- Factor_2[which(Date[i]==Factor_2[,1]),2]  
}
#colnames(Factors)[2]      <- "NASDAQ"
colnames(Factors)[2]      <- "TED"
colnames(Factors)[3]      <- "VIX"
NrFact                    <- 2 # enter how many factors you included in the end

# replace NAs with the last value that is not NA in all columns of data frame
for(j in 2:ncol(Factors)){
  # if first value is NA, take 0
  if(is.na(Factors[1,j])==T){
    Factors[1,j]               <- 0
  }
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
Date_In_End               <- as.Date("2015-12-31") # last date of in-sample
Date_In_End_1dbefore      <- as.Date("2015-12-30") # last date of explanatory var
fit                       <- Arima(#
        # dependent var from 2nd observation to 31.12.2015
        E_ret[2:which(E_ret[,1]==Date_In_End),2], 
        # factors and dummies as explanatory variables
        xreg = Factors[2:which(Factors[,1]==Date_In_End_1dbefore),2:ncol(Factors)],
        # choose order of ARIMA(p,d,q) model you want to estimate
        order = c(2,0,2)
        )#
summary(fit)
err_in                  <- resid(fit) # store the model residuals
tsdisplay(arima.errors(fit), main="ARIMA(2,0,2) errors") # assess model errors
Box.test(residuals(fit),fitdf=3,lag=10,type="Ljung") # check for serial correlation in errors
# in-sample R^2
R_sq                    <- 1-sum((residuals(fit))^2)/
        sum((E_ret[2:which(E_ret[,1]==Date_In_End),2]-
        mean(E_ret[2:which(E_ret[,1]==Date_In_End),2]))^2)

#################### end third chunk

###
# Prediction
###

# h step ahead forecast
fcast                   <- forecast(fit,
                            xreg=
                              Factors[2:which(Factors[,1]==Date_In_End_1dbefore),2:ncol(Factors)],
                              h=which(Factors[,1]==Date_In_End_1dbefore)-which(Factors[,1]==Date_In_End)-1)
plot(fcast)

# rolling one-step-ahead forecast
# create empty matrices
pred                    <- matrix(data=NA,nrow=which(Factors[,1]==Date[length(Date)]
)-which(Factors[,1]==Date_In_End_1dbefore)-1,ncol=1)
benchmark               <- matrix(data=NA,nrow=which(Factors[,1]==Date[length(Date)]
)-which(Factors[,1]==Date_In_End_1dbefore)-1,ncol=1)
actual                  <- matrix(data=NA,nrow=which(Factors[,1]==Date[length(Date)]
)-which(Factors[,1]==Date_In_End_1dbefore)-1,ncol=1)
err_out                 <- matrix(data=NA,nrow=which(Factors[,1]==Date[length(Date)]
)-which(Factors[,1]==Date_In_End_1dbefore)-1,ncol=1)

# run for constant parameters
start.time <- Sys.time()
for(i in 1:(which(Factors[,1]==Date[length(Date)]
)-which(Factors[,1]==Date_In_End_1dbefore)-1)){
  # model estimation as above
  reg_1                   <- Arima(#
          E_ret[2:(which(E_ret[,1]==Date_In_End)+i-1),2],
          xreg = Factors[2:(which(Factors[,1]==Date_In_End_1dbefore)+i-1),2:ncol(Factors)],
          order = c(2,0,2)
          )#  
  # one-step-ahead forecast
  fcast                   <- forecast(#
          reg_1, xreg=#
          Factors[which(Factors[,1]==Date_In_End_1dbefore)+i,2:ncol(Factors)],
          h=1)#
  # fill matrices
  pred[i]                 <- fcast$mean # one-step ahead forecast
  benchmark[i]            <- mean(E_ret[2:(which(E_ret[,1]==Date_In_End)+i-1),2]) # mean of past series as benchmark
  actual[i]               <- E_ret[which(E_ret[,1]==Date_In_End)+i,2] # actual value
  err_out[i]              <- actual[i]-pred[i] # store the error
}
end.time <- Sys.time()
end.time - start.time # how long did it run
# head(cbind(actual,pred,benchmark))
Rsq_os                     <- 1-(sum((actual-pred)^2)/sum((actual-benchmark)^2))

# Plot forecasting performance vs actual and benchmark to further assess your performance
plot(Date[(which(Date==Date_In_End)+1):length(Date)],actual,type="l",col="black",
     xlab="Time",ylab="Predicted vs actual")
lines(Date[(which(Date==Date_In_End)+1):length(Date)],pred,col="cornflowerblue")
lines(Date[(which(Date==Date_In_End)+1):length(Date)],benchmark,col="red")
legend("bottom",c("Actual","Forecast","Benchmark"),
       lty=c(1,1,1),col=c("black","cornflowerblue","red"))

#################### end fourth chunk


###
# End
###

# Discard data before and after period of interest
#Factor_1                  <- Factor_1[which(Factor_1[,1]==Date[1]):
#                                        which(Factor_1[,1]==Date[length(Date)]),]
