library(fUnitRoots)
library(forecast)
library(fGarch)
library(Metrics)
library(zoo)
library(dygraphs)
library(xts)
vw = read.csv("/Users/lichunhao/Dropbox/Study/HKUST/IT/MSBD5006/project/HKStockMarketAnalysis/data/ali0241.HK.csv", header=TRUE)
#vw = read.csv("/Users/lichunhao/Dropbox/Study/HKUST/IT/MSBD5006/project/HKStockMarketAnalysis/data/fuxing2196.HK.csv", header=TRUE)
da_ori = as.numeric(vw$Close)
da_return = 100*diff(log(da_ori)) # 对数收益率
length(da_return) 
end_index = length(da_return)-100
end_index
da = da_return[1:end_index]
test = da_return[(end_index+1):length(da_return)]
length(test) # 100
length(da)  # 1355
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

length(test[1:7])
length(fore$meanForecast)
rmse(test[1:7],fore$meanForecast)

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


# ========================================================================================
# train loop
ts_price_predicted = data.frame()
test_step = 50
for (k in test_step:1) {
cur_da = window(da_return, end = index(da_return)[length(da_return)-k])
#model_fitted = garchFit(cur_da~arma(2,3)+garch(1,2), data=cur_da, trace=FALSE, include.mean=FALSE)
## change model automatically
best_model = auto.arima(cur_da)
model_fitted = arima(cur_da,c(best_model$arma[1],0,best_model$arma[2]))
fore=predict(model_fitted,1)
base_price = as.numeric(da_ori[index(tail(cur_da,1))])
cat("Fitted model is:", as.character(best_model$arma),"\n")
cat("训练的数据为",as.character(tail(index(cur_da),1)),"前所有的数据","\n","正在预测", as.character(index(da_ori)[length(da_ori)-k+1]),"的数据\n",sep = " ")
## for arima-garch
#price_predicted = exp(as.numeric(fore$meanForecast))*base_price #log_return to price
## for arima
price_predicted = exp(as.numeric(fore$pred[1]))*base_price #log_return to price
ts_price_predicted = rbind(ts_price_predicted,price_predicted)
}
names(ts_price_predicted) = c("Predictions")
ts_price_predicted
da_ori_ts = xts(da_ori, order.by = as.Date(vw$Date))%>% as.zoo()
ts_price_predicted = xts(ts_price_predicted,order.by=as.Date(index(tail(da_ori_ts,test_step)))) %>% as.zoo()

dygraph(merge(ts_price_predicted,tail(da_ori,test_step)))
## Plot error
ts_error = tail(da_ori,test_step)-ts_price_predicted
names(ts_error) = c("Error")
par(mfrow=c(1,1))
barplot(ts_error)
summary(ts_error)

rmse(tail(da_ori, test_step), ts_price_predicted)
