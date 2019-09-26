## loading the packages you want
library(tidyverse, quietly = TRUE)
library(forecastHybrid, quietly = TRUE)
library(Metrics, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(prophet, quietly = TRUE)

## renaming the files and removing the singular date value at the end of the file
token <- wowtokenstata 
token <- na.omit(token)

## subsetting on regions
token_cn <- subset(token, region=="CN")
token_eu <- subset(token, region=="EU")
token_kr <- subset(token, region=="KR")
token_nam <- subset(token, region=="NAM")
token_tw <- subset(token, region=="TW")


arimaGold <- auto.arima(token_cn$gtousd)
test <- forecast(arimaGold, h=20)
plot(test, include = 200)


## turning what we did in the last paper into a function so I can apply it to all the seperate data sets.
forecast_token <- function(data, ahead ){
  arimaGold <- auto.arima(data$gtousd)
  fcastarimagold <- forecast(arimaGold, h= ahead)
  x <- plot(fcastarimagold, include = 200)
  
  etsGold <- ets(data$gtousd)
  fcastetsgold <- forecast(etsGold, h=ahead)
  x1 <- plot(fcastetsgold, include = 200)
  
  tbatsGold <- tbats(data$gtousd)
  fcasttbatsgold <- forecast(tbatsGold, h=ahead)
  x2 <- plot(fcasttbatsgold, include = 200)
  
  prophetBlack <- data
  prophetBlack <- prophetBlack[c(2,4)]
  prophetBlack <- rename(prophetBlack, ds = date, y = gtousd)
  
  pBlack <- prophet(prophetBlack)
  fprophet <- make_future_dataframe(pBlack, periods = ahead, freq = "day")
  fcastprophet <- predict(pBlack, fprophet)
  x3 <- plot(pBlack, fcastprophet)
  
  hybridblack <- hybridModel(data$gtousd, weights = "insample.errors", errorMethod = "MASE") 
  blackfcast <- forecast(hybridblack, h=ahead)
  x4 <- plot(blackfcast, include = 200)
  
  hybridblack1 <- hybridModel(data$gtousd, weights = "equal", errorMethod = "MASE") 
  blackfcast1 <- forecast(hybridblack1, h=ahead)
  x5 <- plot(blackfcast1, include = 200)
  
  actVsFcast <- cbind(ts(data$gtousd), ts(fcastetsgold$mean, start = length(data)-length(ahead)), ts(fcastarimagold$mean, start = length(data)-length(ahead)), ts(fcasttbatsgold$mean, start = length(data)-length(ahead)), ts(tail(fcastprophet$yhat,ahead), start = length(data)-length(ahead)), ts(blackfcast$mean, start = length(data)-length(ahead)), ts(blackfcast1$mean, start = length(data)-length(ahead)))
  colnames(actVsFcast) <- c("Orignal", "ETS", "Arima", "Tbats", "Prophet", "HybridE", "HybridIn")
  x6 <- autoplot(actVsFcast)
  
  ETS <- rbind(mape(tail(data$gtousd,ahead),fcastetsgold$mean))
  ARIMA <- rbind(mape(tail(data$gtousd,ahead),fcastarimagold$mean))
  TBATS <- rbind(mape(tail(data$gtousd,ahead),fcasttbatsgold$mean))
  PROPHET <- rbind(mape(tail(data$gtousd,ahead),tail(fcastprophet$yhat,3)))
  HYBRIDE <- rbind(mape(tail(data$gtousd,ahead),blackfcast$mean))
  HYBRIDIN <- rbind(mape(tail(data$gtousd,ahead),blackfcast1$mean))
  
  error_metrics <- cbind(ETS, ARIMA, TBATS, PROPHET, HYBRIDE, HYBRIDIN)
  colnames(error_metrics) <-c("ETS", "Arima", "Tbats", "Prophet", "HybridE", "HybridIn") 
  error_metrics
  
  #most_precise <- cbind(ts(data$gtousd), ts(blackfcast1$mean, start = 117)) 
  #colnames(most_precise) <- c("Original", "HybridIn")
  #x7 <- autoplot(most_precise)
  
  return(error_metrics)
  
}


## calling the function
cn <-  forecast_token(token_cn, 20)
tw <-  forecast_token(token_tw, 20)
na <-  forecast_token(token_nam, 20)
kr <-  forecast_token(token_kr, 20)
eu <-  forecast_token(token_eu, 20)

## making a final data frame with all the errors
final <- rbind(cn, tw, na, kr, eu)
row.names(final) <- c("China", "Taiwan", "NA", "Korea", "EU")

## changing the mape to make it eaiser to explain
final <- final*100



