#Financial modeling package
library("quantmod")
library("PerformanceAnalytics")
#Hidden Markov Library
library('RHmm') 
#Built-in Dataset
library('zoo')

###----------------------------------------------------------------------------------###
#Need to state dates from which the model is being filed
startDate = as.Date("") #Specify what date to get the prices from
trainingEndDate = as.Date("2010-01-01") # Specify the date we take as our traning sample
NDayLookforwardLowHigh <- 10 #Parameter used when classifing in sample data as in a trend or not
HmmLikelihoodTestLength <- 5 #How many days of data to calculate the likehood ratio on to compare models
 
symbolData <- new.env() #Make a new environment for quantmod to store data in
symbol <- "^GSPC" #S&p 500
 
 
getSymbols(symbol, env = symbolData, src = "yahoo", from = startDate)
marketdata <- head(eval(parse(text=paste("symbolData$",sub("^","",symbol,fixed=TRUE)))), -1)
inSampleMarketData <-  window(marketdata,start=startDate ,end=trainingEndDate)
outOfSampleMarketData <-  window(marketdata,start=trainingEndDate+1)
#This is the daily return
dailyRet <- Delt(Cl(mktdata),k=1,type="arithmetic") 
dailyRet[is.na(dailyRet)] <-0.00001
 
inSampleDailyRet <-  window(dailyRet,start=startDate ,end=trainingEndDate)
outOfSampleDailyRet <-  window(dailyRet,start=trainingEndDate+1)
 
#Interpolates to full signal 
InterpolatesfullSignal <- function(signal){
	results <- rep(0,length(signal))
	intrade <- F
	for(i in seq(1,length(signal))){
		if(signal[i]==-1){
		 intrade <- F
		}
		if(signal[i]==1 || intrade){
		   results[i]<-1
		   intrade <- T
		}
	}
	return(results)
}
 
###-------------------------------------------------------------###
#We generate 2 signals, long trend and short trend, the strategy follows up and categorizes
#downwards trends and upward ones
 
#Generate long trend signal
LongTrendSignal <- rep(0,nrow(inSampleMktData))
for(i in seq(1,nrow(inSampleMarketData)-NDayLookforwardLowHigh)){
	dataBlock <- Cl(inSampleMarketData[seq(i,i+NDayLookforwardLowHigh),])
 
	if(coredata(Cl(inSampleMarketData[i,])) == min(coredata(dataBlock))){
	  LongTrendSignal[i] <- 1
	}
	if(coredata(Cl(inSampleMarketData[i,])) == max(coredata(dataBlock))){
	  LongTrendSignal[i] <- -1
	}
 
}
LongTrendSignal <- ConvertTofullSignal(LongTrendSignal)
 
#Generate short trend signal
ShortTrendSignal <- rep(0,nrow(inSampleMktData))
for(i in seq(1,nrow(inSampleMarketData)-NDayLookforwardLowHigh)){
	dataBlock <- Cl(inSampleMarketData[seq(i,i+NDayLookforwardLowHigh),])
 
	if(coredata(Cl(inSampleMarketData[i,])) == max(coredata(dataBlock))){
	  ShortTrendSignal[i] <- 1
	}
	if(coredata(Cl(inSampleMarketData[i,])) == min(coredata(dataBlock))){
	  ShortTrendSignal[i] <- -1
	}
 
}
ShortTrendSignal <- ConvertTofullSignal(ShortTrendSignal)
 
#Plot our signals
LongTrendSignalForPlot <- LongTrendSignal
LongTrendSignalForPlot[LongTrendSignalForPlot==0] <- NaN
LongTrendSignalForPlot <- Cl(inSampleMarketData)*LongTrendSignalForPlot - 100
inSampleLongTrendSignalForPlot <-LongTrendSignalForPlot
 
ShortTrendSignalForPlot <- ShortTrendSignal
ShortTrendSignalForPlot[ShortTrendSignalForPlot==0] <- NaN
ShortTrendSignalForPlot <- Cl(inSampleMarketData)*ShortTrendSignalForPlot + 100
inSampleShortTrendSignalForPlot <- ShortTrendSignalForPlot
 
layout(1:2)
plot(Cl(inSampleMarketData), main="Training Samples")
lines(inSampleLongTrendSignalForPlot,col="green",type="l")
lines(inSampleShortTrendSignalForPlot,col="red",type="l")
legend(x='bottomright', c("S&P 500 Closing Price","Long Signal","Short Signal"),  fill=c("black","green","red"), bty='n')
 
#Calculate Returns
LongReturns <- Lag(LongTrendSignal)* (inSampleDailyRet)
LongReturns[is.na(LongReturns)] <- 0
ShortReturns <- Lag(-1*ShortTrendSignal)* (inSampleDailyRet)
ShortReturns[is.na(ShortReturns)] <- 0
TotalReturns <- LongReturns + ShortReturns
plot(cumsum(TotalReturns),main="S&P 500 Trend Follow In Sample HMM Training Signals Strategy Returns")
lines(cumsum(LongReturns),col="green")
lines(cumsum(ShortReturns),col="red")
legend(x='bottomright', c("Total Returns","Long Trend Returns","Short Trend Returns"),  fill=c("black","green","red"), bty='n')
 
#Extracts a list of varying length features for each signal/class label
CreateListOfMatrixFeatures <- function(features,signal){
	  results <- list()
	  extract <- F
	  for(i in seq(1,length(signal))){
		  if(signal[i]==1 && !extract){
				startIndex <- i
				extract <- T
		  }
		  if(signal[i]==0 && extract){
			  endIndex <- i-1
			  dataBlock <- features[startIndex:endIndex,]
			  extract <- F
			  #print(dataBlock)
			  results[[length(results)+1]] <- as.matrix(dataBlock)
 
		  }
	  }
	  return(results)
}
 
#HMM Training
#Generate the features that describe the data & split into training and out of sample sets
features <- cbind(dailyRet,Hi(mktdata)/Lo(mktdata),Hi(mktdata)/Op(mktdata),Hi(mktdata)/Cl(mktdata),Op(mktdata)/Cl(mktdata),Lo(mktdata)/Cl(mktdata),Lo(mktdata)/Op(mktdata))
inSampleTrainingFeatures <-  window(features,start=startDate ,end=trainingEndDate)
outOfSampleFeatures <- window(features,start=trainingEndDate+1)
 
#For each long / short position extract the corresponding features data and create list of them
inSampleLongFeaturesList <- CreateListOfMatrixFeatures(inSampleTrainingFeatures,LongTrendSignal)
inSampleShortFeaturesList <- CreateListOfMatrixFeatures(inSampleTrainingFeatures,ShortTrendSignal)
 
#Train the HMM models
LongModelFit = HMMFit(inSampleLongFeaturesList, nStates=3)
ShortModelFit = HMMFit(inSampleShortFeaturesList, nStates=3)
 
#Will take NDayLookforwardLowHigh days of data and calculate the rolling log likelihood for each HMM model
inSampleLongLikelihood <- rollapply(inSampleTrainingFeatures,HmmLikelihoodTestLength,align="right",na.pad=T,by.column=F,function(x) {forwardBackward(LongModelFit,as.matrix(x))$LLH})
inSampleShortLikelihood <- rollapply(inSampleTrainingFeatures,HmmLikelihoodTestLength,align="right",na.pad=T,by.column=F,function(x) {forwardBackward(ShortModelFit,as.matrix(x))$LLH})
outOfSampleLongLikelihood <- rollapply(outOfSampleFeatures,HmmLikelihoodTestLength,align="right",na.pad=T,by.column=F,function(x) {forwardBackward(LongModelFit,as.matrix(x))$LLH})
outOfSampleShortLikelihood <- rollapply(outOfSampleFeatures,HmmLikelihoodTestLength,align="right",na.pad=T,by.column=F,function(x) {forwardBackward(ShortModelFit,as.matrix(x))$LLH})
 
#Create signals for plot / trading
outOfSampleLongTrendSignalForPlot <- 1*(outOfSampleLongLikelihood > outOfSampleShortLikelihood)
outOfSampleLongTrendSignalForPlot[outOfSampleLongTrendSignalForPlot==0] <- NaN
outOfSampleLongTrendSignalForPlot <- outOfSampleLongTrendSignalForPlot*Cl(outOfSampleMktData)-100
 
outOfSampleShortTrendSignalForPlot <- 1*(outOfSampleLongLikelihood < outOfSampleShortLikelihood)
outOfSampleShortTrendSignalForPlot[outOfSampleShortTrendSignalForPlot==0]<-NaN
outOfSampleShortTrendSignalForPlot <- outOfSampleShortTrendSignalForPlot*Cl(outOfSampleMktData)+100
 
 
dev.new()
layout(1:2)
plot(Cl(inSampleMktData), main="S&P 500 Trend Follow In Sample HMM Training Signals")
lines(inSampleLongTrendSignalForPlot,col="green",type="l")
lines(inSampleShortTrendSignalForPlot,col="red",type="l")
legend(x='bottomright', c("S&P 500 Closing Price","Long Signal","Short Signal"),  fill=c("black","green","red"), bty='n')
 
#tt <- Cl(inSampleMktData)
#tt[,1] <- inSampleLongLikelihood
plot(inSampleLongLikelihood,main="Log Likelihood of each HMM model - In Sample")
lines(inSampleLongLikelihood,col="green")
lines(inSampleShortLikelihood,col="red")
legend(x='bottomright', c("Long HMM Likelihood","Short HMM Likelihood"),  fill=c("green","red"), bty='n')
 
dev.new()
layout(1:3)
plot(Cl(outOfSampleMktData), main="S&P 500 HMM Trend Following Out of Sample")
lines(outOfSampleLongTrendSignalForPlot,col="green",type="l")
lines(outOfSampleShortTrendSignalForPlot,col="red",type="l")
legend(x='bottomright', c("S&P 500 Closing Price","Long Signal","Short Signal"),  fill=c("black","green","red"), bty='n')
 
#tt <- Cl(outOfSampleMktData)
#tt[,1] <- outOfSampleLongLikelihood
plot(outOfSampleLongLikelihood,main="Log Likelihood of each HMM model - Out Of Sample")
lines(outOfSampleLongLikelihood,col="green")
lines(outOfSampleShortLikelihood,col="red")
legend(x='bottomright', c("Long HMM Likelihood","Short HMM Likelihood"),  fill=c("green","red"), bty='n')
 
#Calculate Out of Sample Returns
outOfSampleLongReturns <- Lag((1*(outOfSampleLongLikelihood > outOfSampleShortLikelihood)))* (outOfSampleDailyRet)
outOfSampleLongReturns[is.na(outOfSampleLongReturns)] <- 0
outOfSampleShortReturns <- Lag(-1*(1*(outOfSampleLongLikelihood < outOfSampleShortLikelihood)))* (outOfSampleDailyRet)
outOfSampleShortReturns[is.na(outOfSampleShortReturns)] <- 0
outOfSampleTotalReturns <- outOfSampleLongReturns + outOfSampleShortReturns
outOfSampleTotalReturns[is.na(outOfSampleTotalReturns)] <- 0
 
plot(cumsum(outOfSampleTotalReturns),main="S&P 500 HMM Trend Following Out of Sample Strategy Returns")
lines(cumsum(outOfSampleLongReturns),col="green")
lines(cumsum(outOfSampleShortReturns),col="red")
legend(x='bottomright', c("Total Returns","Long Trend Returns","Short Trend Returns"),  fill=c("black","green","red"), bty='n')
 
print(SharpeRatio.annualized(outOfSampleTotalReturns))
