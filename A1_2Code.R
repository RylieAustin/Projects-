#turn data ino time series
y=ts(milksold, start=2009, frequency=12)
plot(y)
#smooth by factor of 3
Milksold3<-SMA(y,n=3)
plot(Milksold3)
#smooth by factor of 6
Milksold6<-SMA(y,n=6)
plot(Milksold6)
#smooth by factor of 12
Milksold12<-SMA(y,n=12)
plot(Milksold12)
#additively decompose 
milksolddecomp<-decompose(y)
plot(milksolddecomp)
#seasonally adjust
MilkSoldSznAdj<-y- milksolddecomp$seasonal
plot(MilkSoldSznAdj)
#simple exponential smoothing model
MilksoldSimpleExpSmth<-HoltWinters(y,beta=FALSE,gamma=FALSE)
MilksoldSimpleExpSmth
plot(MilksoldSimpleExpSmth)
MilksoldSimpleExpSmth$SSE
#forecast with exponential smoothing model
MilksoldSimpleExpSmthforecast<- forecast(MilksoldSimpleExpSmth, h=24)
plot(MilksoldSimpleExpSmthforecast)
#test for correlations between in-sample residuals & succesive predictions
acf(MilksoldSimpleExpSmthforecast$residuals, na.action = na.pass, lag.max=12.39)
Box.test(MilksoldSimpleExpSmthforecast$residuals, lag=12.39, type="Ljung-Box")
plot(MilksoldSimpleExpSmthforecast$residuals)
hist(MilksoldSimpleExpSmthforecast$residuals)
#holts forecasting model (smooths trend out of data)
MilksoldHoltsModel<-HoltWinters(y,gamma=FALSE)
MilksoldHoltsModel
plot(MilksoldHoltsModel)
#forecast with Holts Model
MilksoldHoltsForecast<-forecast(MilksoldHoltsModel, h=24)
plot(MilksoldHoltsForecast)
#check residuals of holts forecast
checkresiduals(MilksoldHoltsForecast)
#RMSE Holts Model
accuracy(MilksoldHoltsForecast)
#holts damped forecasting (smooths trend out of data)
MilksoldHoltsdampedForecast<-holt(y,damped=TRUE, h=24)
plot(MilksoldHoltsdampedForecast)
#check residuals of holts damped forecast
checkresiduals(MilksoldHoltsdampedForecast)
#RMSE Holts damped model
accuracy(MilksoldHoltsdampedForecast)
#winters forecasting model (smooths seasonality out of data)
MilksoldWinterModel<-HoltWinters(y)
MilksoldWinterModel
plot(MilksoldWinterModel)
#forecast with Holts Model
MilksoldWinterForecast<-forecast(MilksoldWinterModel, h=24)
plot(MilksoldWinterForecast)
#check residuals of Holt Winters 
checkresiduals(MilksoldSimpleExpSmthforecast)
#RMSE Holt Winters 
accuracy(MilksoldWinterForecast)
#ETS forecast
x<-y%>%ets()%>%forecast
plot(x)
#check residuals of MAA forecast
checkresiduals(x)
#RMSE MAA forecast
accuracy(x)
#######################################
#ARIMA Modeling
plot(y) + ylab("Milk Sold") + xlab("Year")
#check ACF and PACF
ggtsdisplay(y)
#seasonal differencing
sznldiff<-diff(y, lag=12)
ggtsdisplay(sznldiff)
#first differencing
dbldiff<- diff(sznldiff)
ggtsdisplay(dbldiff)
#Check residuals of selected model
arima1<- Arima(y, order = c(1,1,0), seasonal = c(1,1,1))
ggtsdisplay(residuals(arima1))
summary(arima1)
#model based on ACF chnageshelp
arima2<- Arima(y, order = c(1,1,1), seasonal = c(1,1,1))
ggtsdisplay(residuals(arima2))
summary(arima2)
#model based on PACF chnages
arima3<- Arima(y, order = c(2,1,0), seasonal = c(1,1,1))
ggtsdisplay(residuals(arima3))
summary(arima3)
#model based on both
arima4<- Arima(y, order = c(2,1,1), seasonal = c(1,1,1))
ggtsdisplay(residuals(arima4))
summary(arima4)
#model based on sinusoidal decay
arima5<- Arima(y, order = c(4,1,4), seasonal = c(1,1,1))
ggtsdisplay(residuals(arima5))
summary(arima5)
#check residuas of selected model: Model 5
checkresiduals(arima5)
#forecast with model
forecast5<-forecast(arima5, h=24)
plot(forecast5) + ylab("Milk Sold") + xlab("Year")
summary(forecast5)
#auto fit for comparison 
validate<- auto.arima(y, stepwise = FALSE, approximation = FALSE)
ggtsdisplay(residuals(validate))
summary(validate)
#time series linear model for milk sold
lnmodel<-tslm(formula = y~trend + season)
plot(lnmodel)
#forecast with linear model
lnforecast<- forecast(lnmodel, h=24)
plot(lnforecast)
#measure accuracy
accuracy(lnforecast)
checkresiduals(lnforecast)
summary(lnforecast)
###################
x<- window(y, end=c(2016,6))
length(x)
plot(forecast(Arima(x, order = c(4,1,4), seasonal = c(1,1,1))))
##################
ets<-ETS(y)
