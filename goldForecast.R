#looking into ets, tbats, and auto arima
blackMarket <- subset(wowgold, region=="Wow")
#we have some missing values and need to do some imputation to fill them up for time series. I gathered the data and have no idea why 
#some of the values are missing therefore im going un the asumption they are randomly missing and not some bias

library(mice)
md.pattern(blackMarket, plot = TRUE)

#we are missing 16 values for our gold to usd ratios
#m is the number of imputations, maxit is iterations, meth is the  type of method you want to use to find the values 
#with pmm being predictive  mean matching

tempData <- mice(blackMarket,m=5,maxit=50,meth='pmm',seed=1337)
summary(tempData)
completedBlack<-complete(tempData,1)

trainBlack <- head(completedBlack,-3)
testBlack <- tail(completedBlack, 3)

##forecast package

arimaGold <- auto.arima(trainBlack$gtousd)
fcastarimagold <- forecast(arimaGold, h= 3)
plot(fcastarimagold)

etsGold <- ets(trainBlack$gtousd)
fcastetsgold <- forecast(etsGold, h=3)
plot(fcastetsgold)

tbatsGold <- tbats(trainBlack$gtousd)
fcasttbatsgold <- forecast(tbatsGold, h=3)
plot(fcasttbatsgold)

##prophet
#prophet does its own imputations reverting back to orginal data
prophetBlack <- blackMarket
prophetBlack <- prophetBlack[c(7:8)]

#have to change names for prophet package
library(tidyverse)
prophetBlack <- rename(prophetBlack, ds = rtime, y = gtousd)
pBlack <- prophet(prophetBlack)
fprophet <- make_future_dataframe(pBlack, periods = 3, freq = "month")
fcastprophet <- predict(pBlack, fprophet)
plot(pBlack, fcastprophet)


##forecasthybrid
library(forecastHybrid)
hybridblack <- hybridModel(trainBlack$gtousd)
blackfcast <- forecast(hybridblack, h=3)
plot(blackfcast)

hybridblack1 <- hybridModel(completedBlack$gtousd, weights = "insample.errors", errorMethod = "MASE") 
blackfcast1 <- forecast(hybridblack1, h=3)
plot(blackfcast1)

#now we can look at all the forecasts and compare metrics to them all

actVsFcast <- cbind(ts(completedBlack$gtousd), ts(fcastetsgold$mean, start = 118), ts(fcastarimagold$mean, start = 118), ts(fcasttbatsgold$mean, start = 118), ts(tail(fcastprophet$yhat,3), start = 118), ts(blackfcast$mean, start = 118), ts(blackfcast1$mean, start = 118))
colnames(actVsFcast) <- c("Orignal", "ETS", "Arima", "Tbats", "Prophet", "HybridE", "HybridIn")
time <- index(actVsFcast)

actVsFcast <- as.xts(actVsFcast)
autoplot(actVsFcast["100::120"])


library(ggplot2)
library(xts)

data(sample_matrix)
test_xts <- as.xts(sample_matrix)

# look at autoplot results
autoplot( test_xts )

# to look at one series
autoplot( test_xts[,1] )

autoplot( test_xts["2000::2007-03",1] )

#lets look at the metrics of our forecasted values
library(Metrics)

ETS <- rbind(mape(testBlack$gtousd,fcastetsgold$mean), rmse(testBlack$gtousd, fcastetsgold$mean), mse(testBlack$gtousd, fcastetsgold$mean))
ARIMA <- rbind(mape(testBlack$gtousd,fcastarimagold$mean), rmse(testBlack$gtousd, fcastarimagold$mean), mse(testBlack$gtousd, fcastarimagold$mean))
TBATS <- rbind(mape(testBlack$gtousd,fcasttbatsgold$mean), rmse(testBlack$gtousd, fcasttbatsgold$mean), mse(testBlack$gtousd, fcasttbatsgold$mean))
PROPHET <- rbind(mape(testBlack$gtousd,tail(fcastprophet$yhat,3)), rmse(testBlack$gtousd, tail(fcastprophet$yhat,3)), mse(testBlack$gtousd, tail(fcastprophet$yhat,3)))
HYBRIDE <- rbind(mape(testBlack$gtousd,blackfcast$mean), rmse(testBlack$gtousd, blackfcast$mean), mse(testBlack$gtousd, blackfcast$mean))
HYBRIDIN <- rbind(mape(testBlack$gtousd,blackfcast1$mean), rmse(testBlack$gtousd, blackfcast1$mean), mse(testBlack$gtousd, blackfcast1$mean))

error_metrics <- cbind(ETS, ARIMA, TBATS, PROPHET, HYBRIDE, HYBRIDIN)
colnames(error_metrics) <-c("ETS", "Arima", "Tbats", "Prophet", "HybridE", "HybridIn") 
error_metrics

most_precise <- cbind(ts(completedBlack$gtousd), ts(blackfcast1$mean, start = 117)) 
colnames(most_precise) <- c("Original", "HybridIn")
autoplot(most_precise)


  