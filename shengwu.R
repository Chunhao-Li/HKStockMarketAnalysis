setwd("D:/BDT/2022 S2/MSBD5006/Project/HKStockMarketAnalysis/data")
df = read.csv("sw_train.csv",header = TRUE)
predict = read.csv("sw_test.csv", header = TRUE)
data = df[, c('Close')]

df$Date <- as.Date(df$Date, '%Y-%m-%d')
plot(df$Date, data, type="l", col = "blue", xlab = "time", ylab = "closing price", main = "Closing prices of Sino Biopharmaceutical Limited")
logreturn = diff(log(data), lag=1)*100
plot(df$Date[2:1356], logreturn, type="l", col = "blue", xlab = "time", ylab = "Percentage log return", main = "Perecentage log returns of Sino Biopharmaceutical Limited")

acf1 = acf(logreturn, lag = 24, main ="First 24 lags of ACF of the log returns of the stock closing price")
acf1
pacf1 = pacf(logreturn, lag = 24, main ="First 24 lags of PACF of the log returns of the stock closing price")
pacf1


#ADF Test - stationary
library(aTSA)
adf.test(logreturn)

#Box test - serial correlation
for (k in 1:30) print(Box.test(logreturn,lag = k, type = "Ljung-Box"))

#m1 = arima(logreturn, c(0, 0, 29))
#m1
#Box.test(m1$residuals,lag = 30, type="Ljung")
#pv=1-pchisq(1.4383,30)
#pv

#NAN values produced
#m2 = arima(logreturn, c(29, 0, 29))
#m2
#Box.test(m2$residuals,lag = 30, type="Ljung")
#pv=1-pchisq(4.2912,30)
#pv

#m3 = arima(logreturn, c(29, 0, 0))
#m3
#Box.test(m3$residuals,lag = 30, type="Ljung")
#pv=1-pchisq(2.0362,30)
#pv

#Test arch effect
at = logreturn - mean(logreturn)
acf(at^2)
for (k in 1:12) print(Box.test(at^2,lag = k, type = "Ljung-Box"))

#eGARCH(5,7)
library(rugarch)
spec4=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(5,7)),
                 mean.model=list(armaOrder=c(0,0),include.mean = TRUE),
                 distribution.model="norm")
m4=ugarchfit(spec=spec4,data=logreturn)
m4
l4 = likelihood(m4)
(-2*l4)/length(logreturn)+2*(length(m4@fit$coef))/length(logreturn)


stresi4=residuals(m4,standardize=T)
Box.test(stresi4,10,type="Ljung")
Box.test(stresi4^2,10,type="Ljung")

#predict 7 days
pred1 = ugarchforecast(m4, n.ahead=5)
sigma = pred1@forecast$sigmaFor
series = pred1@forecast$seriesFor
U=series +1.96 * sigma
L=series - 1.96 * sigma

whole = read.csv("shengwu1177.HK.csv", header = TRUE)
logreturn_whole = diff(log(whole[, c('Close')]), lag=1)*100

plot(1:12, logreturn_whole[1350:1361],type="o",ylim=c(-8,8),ylab="",xlab="",main="Forecasting")
lines(7:12,append(logreturn_whole[1356], series),type="o",col="red")
lines(8:12, U,type="l",col="blue")
lines(8:12, L,type="l",col="blue")



#predict 100 days
pred1 = ugarchforecast(m4, n.ahead=100)
sigma = pred1@forecast$sigmaFor
series = pred1@forecast$seriesFor
U=series +1.96 * sigma
L=series - 1.96 * sigma

whole = read.csv("shengwu1177.HK.csv", header = TRUE)

plot(1:107, logreturn_whole[1350:1456],type="o",ylim=c(-8,8),ylab="",xlab="",main="Forecasting")
lines(7:107,append(data[1356], series),type="o",col="red")
lines(8:107, U,type="l",col="blue")
lines(8:107, L,type="l",col="blue")


##########################Select model#####################

best_aic = 1000
model = m1
for (k in 1:7) {
  for (l in 1:7) {
    spec1=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(k,l)),
                     mean.model=list(include.mean = TRUE),
                     distribution.model="norm")
    m1=ugarchfit(spec=spec1,data=logreturn)
    #Calculate AIC score
    l1 = likelihood(m1)
    aic1 = (-2*l1)/length(logreturn)+2*(length(m1@fit$coef))/length(logreturn)
    
    if (aic1 < best_aic){
      aic1
      best_aic = aic1
      model = m1
    }

    #GARCH-M(1,1)
    spec2 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(k,l)),
                       mean.model=list(include.mean=TRUE,archm=TRUE,archpow=2),
                       distribution.model="norm")
    m2 = ugarchfit(spec = spec2, data = logreturn)
  
    #Calculate AIC score
    l2 = likelihood(m2)
    aic2 = (-2*l2)/length(logreturn)+2*(length(m2@fit$coef))/length(logreturn)
    
    if (aic2 < best_aic){
      aic2
      best_aic = aic2
      model = m2
    }
    
    #iGARCH(1,1)
    spec3=ugarchspec(variance.model=list(model="iGARCH", garchOrder = c(k,l)),
                     mean.model=list(include.mean = TRUE),
                     distribution.model="norm")
    m3=ugarchfit(spec=spec3,data=logreturn)
    l3 = likelihood(m3)
    aic3 = (-2*l3)/length(logreturn)+2*(length(m3@fit$coef))/length(logreturn)
    
    if (aic3 < best_aic){
      aic3
      best_aic = aic3
      model = m3
    }
    

    
  }
}



library(rugarch)
#GARCH(1,1)
spec1=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1,4)),
                 mean.model=list(armaOrder=c(0,0),include.mean = TRUE),
                 distribution.model="norm")
m1=ugarchfit(spec=spec1,data=logreturn)
#Calculate AIC score
l1 = likelihood(m1)
(-2*l1)/length(logreturn)+2*(length(m1@fit$coef))/length(logreturn)

#GARCH-M(1,1)
spec2 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(0,0),include.mean=TRUE,archm=TRUE,archpow=2),
                   distribution.model="norm")
m2 = ugarchfit(spec = spec2, data = logreturn)
#Calculate AIC score
l2 = likelihood(m2)
(-2*l2)/length(logreturn)+2*(length(m2@fit$coef))/length(logreturn)

#iGARCH(1,1)
spec3=ugarchspec(variance.model=list(model="iGARCH", garchOrder = c(1,4)),
                 mean.model=list(armaOrder=c(0,0),include.mean = TRUE),
                 distribution.model="norm")
m3=ugarchfit(spec=spec3,data=logreturn)
l3 = likelihood(m3)
(-2*l3)/length(logreturn)+2*(length(m3@fit$coef))/length(logreturn)