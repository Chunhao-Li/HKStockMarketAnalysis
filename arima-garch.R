library(fUnitRoots)
library(forecast)
library(fGarch)
vw = read.csv("/Users/lichunhao/Dropbox/Study/HKUST/IT/MSBD5006/project/HKStockMarketAnalysis/data/ali0241.HK.csv", header=TRUE)
#vw = read.csv("/Users/lichunhao/Dropbox/Study/HKUST/IT/MSBD5006/project/HKStockMarketAnalysis/data/fuxing2196.HK.csv", header=TRUE)
da_ori = as.numeric(vw$Close)
da_ori = 100*diff(log(da_ori)) # 对数收益率
length(da_ori) 
end_index = length(da_ori)-100
da = da_ori[1:end_index]
test = da_ori[(end_index+1):length(da_ori)]
length(test) # 100
length(da)  # 1356
plot(da, type='l', col='blue', xlab="Index", ylab="Price", main="Price of Alibaba Health")
adfTest(da, lags = 2, type = c("c"), title = NULL,
        description = NULL)

# da = 100*diff(log(da)) # 对数收益率
plot(da, type='l', col='blue', xlab="Index", ylab="Log Return", main="Log Return of Alibaba Health")
ar(da,method='mle')
adfTest(da, lags = 1, type = c("c"), title = NULL,
        description = NULL)
adfTest(da, lags = 2, type = c("c"), title = NULL,
        description = NULL)
adfTest(da, lags = 6, type = c("c"), title = NULL,
        description = NULL)
adfTest(da, lags = 8, type = c("c"), title = NULL,
        description = NULL)
adfTest(da, lags = 12, type = c("c"), title = NULL,
        description = NULL)

acf(da, lag=24, main="ACF of Alibaba Health Information",col="red",ylim=c(-0.2,0.2))
pacf(da, lag=24, main="PACF of Alibaba Health Information",col="red",ylim=c(-0.2,0.2))
Box.test(da, lag=1, type="Ljung-Box") 
Box.test(da, lag=6, type="Ljung-Box") 
Box.test(da, lag=12, type="Ljung-Box")
Box.test(da, lag=15, type="Ljung-Box") # reject white noise
Box.test(da, lag=24, type="Ljung-Box")
Box.test(da, lag=30, type="Ljung-Box") # reject white noise

auto.arima(da, trace=TRUE, seasonal = FALSE, stepwise=FALSE, approximation = FALSE)

m1 = arima(da, order=c(2,0,3), include.mean=FALSE)
m1
m2 = arima(da, order=c(0,0,6), include.mean=FALSE)
m2
Box.test(m1$residuals, lag=12, type="Ljung-Box")
pv=1-pchisq(10.152,7)
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
stresi = residuals(m4, standardize=TRUE)
Box.test(stresi, 10, type="Ljung")
Box.test(stresi^2, 10, type="Ljung")


#fore=predict(m4,100)
fore=predict(m4,7)

fore

U=append(da[length(da)],fore$meanForecast+1.96*fore$standardDeviation)
L=append(da[length(da)],fore$meanForecast-1.96*fore$standardDeviation)
U
L


#plot(1:101,append(da[length(da)],test[1:100]),type="o",ylab="",xlab="",main="Forecasting")
plot(1:8,append(da[length(da)],test[1:7]),ylim=c(-10,10),type="o",ylab="",xlab="",main="Forecasting")
#lines(1:101,append(da[length(da)],fore$meanForecast),type="o",col="red")
lines(1:8,append(da[length(da)],fore$meanForecast),type="o",col="red")
#lines(1:101, U,type="l",col="blue")
lines(1:8, U,type="l",col="blue")
#lines(1:101, L,type="l",col="blue")
lines(1:8, L,type="l",col="blue")

legend(x="topleft",c("True returns","prediction"),lty=c(1,1),pch=c(1,1),col=c("black","red"))


