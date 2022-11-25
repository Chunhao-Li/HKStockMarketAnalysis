library(fUnitRoots)
library(forecast)
library(fGarch)
vw = read.csv("/Users/lichunhao/Dropbox/Study/HKUST/IT/MSBD5006/project/HKStockMarketAnalysis/data/ali0241.HK.csv", header=TRUE)
da = as.numeric(vw$Close)
plot(da, type='l', col='blue', xlab="Index", ylab="Price", main="Price of Alibaba Health")
adfTest(da, lags = 2, type = c("c"), title = NULL,
        description = NULL)

da = 100*diff(log(da)) # 对数收益率
plot(da, type='l', col='blue', xlab="Index", ylab="Log Return", main="Log Return of Alibaba Health")
ar(da,method='mle')
adfTest(da, lags = 1, type = c("c"), title = NULL,
        description = NULL)
adfTest(da, lags = 2, type = c("c"), title = NULL,
        description = NULL)
adfTest(da, lags = 0, type = c("c"), title = NULL,
        description = NULL)
adfTest(da, lags = 8, type = c("c"), title = NULL,
        description = NULL)
adfTest(da, lags = 12, type = c("c"), title = NULL,
        description = NULL)

acf(da, lag=24, main="ACF of Alibaba Health Information",col="red",ylim=c(-0.2,0.2))
pacf(da, lag=24, main="PACF of Alibaba Health Information",col="red",ylim=c(-0.2,0.2))
Box.test(da, lag=6, type="Ljung-Box") 
Box.test(da, lag=12, type="Ljung-Box")
Box.test(da, lag=15, type="Ljung-Box") # reject white noise
Box.test(da, lag=24, type="Ljung-Box")
Box.test(da, lag=30, type="Ljung-Box") # reject white noise

auto.arima(da, trace=TRUE, seasonal = FALSE, stepwise=FALSE, approximation = FALSE)

m1 = arima(da, order=c(2,0,3), include.mean=FALSE)
m1
m2 = arima(da, order=c(3,0,0), include.mean=FALSE)
m2
Box.test(m1$residuals, lag=12, type="Ljung-Box")
pv=1-pchisq(8.5695,7)
pv

at = da - mean(da)
acf(at^2, lag=12)
Box.test(at^2, lag=1, type="Ljung-Box")
Box.test(at^2, lag=2, type="Ljung-Box")
Box.test(at^2, lag=3, type="Ljung-Box")
Box.test(at^2, lag=4, type="Ljung-Box")
Box.test(at^2, lag=5, type="Ljung-Box")
Box.test(at^2, lag=6, type="Ljung-Box")
Box.test(at^2, lag=7, type="Ljung-Box")
Box.test(at^2, lag=8, type="Ljung-Box")
Box.test(at^2, lag=9, type="Ljung-Box")
Box.test(at^2, lag=10, type="Ljung-Box")
Box.test(at^2, lag=11, type="Ljung-Box")
Box.test(at^2, lag=12, type="Ljung-Box")

m3 = garchFit(da~arma(2,3)+garch(1,1), data=da, trace=FALSE, include.mean=FALSE)
summary(m3)
m4 = garchFit(da~arma(2,3)+garch(1,2), data=da, trace=FALSE, include.mean=FALSE)
summary(m4)

