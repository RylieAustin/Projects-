#time series for difference in Chinese and US exports
diffEXP=ts(Diff_Exports, start=1992, frequency=4)
plot(diffEXP)
#decompose 
EXPdecomp<-decompose(diffEXP)
plot(EXPdecomp)
#seasonally adjust
EXPSznAdj<-diffEXP - EXPdecomp$seasonal
plot(EXPSznAdj)
#simple exponential smoothing model
EXPsimpleExpSmth<-HoltWinters(diffEXP,beta=FALSE,gamma=FALSE)
plot(EXPsimpleExpSmth)
EXPsimpleExpSmth$SSE
#forecast with exponential smoothing model
EXPsimpleExpSmthforecast<- forecast(EXPsimpleExpSmth, h=20)
plot(EXPsimpleExpSmthforecast)
#test for correlations between in-sample residuals & succesive predictions
accuracy(EXPsimpleExpSmthforecast)
residuals(EXPsimpleExpSmthforecast)
ggtsdisplay(EXPsimpleExpSmthforecast)
#holts damped forecasting (smooths trend out of data)
EXPHoltsdampedForecast<-holt(diffEXP, damped=TRUE, h=20)
plot(EXPHoltsdampedForecast)
#check residuals of holts damped forecast
checkresiduals(EXPHoltsdampedForecast)
#RMSE Holts damped model
accuracy(EXPHoltsdampedForecast)
#holt winters forecasting model (smooths seasonality out of data)
EXPWinterModel<-HoltWinters(diffEXP)
plot(EXPWinterModel)
#forecast with Holt winters Model
EXPWinterModelForecast<-forecast(EXPWinterModel, h=20)
plot(EXPWinterModelForecast)
#check residuals of Holt Winters 
checkresiduals(EXPWinterModelForecast)
#RMSE Holt Winters 
accuracy(EXPWinterModelForecast)
#ETS forecast
EXPets<-diffEXP%>%ets()
EXPets
plot(forecast(EXPets, h=20))
#check residuals of MAA forecast
checkresiduals(EXPets)
#RMSE MAA forecast
accuracy(EXPets)
#######################################
#ARIMA Model auto fit
EXParima<- auto.arima(diffEXP, stepwise = FALSE, approximation = FALSE)
ggtsdisplay(residuals(EXParima))
residuals(EXParima)
summary(EXParima)
#forecast with arima
EXParimaForecast<-forecast(EXParima, h=20)
plot(EXParimaForecast)
accuracy(EXParimaForecast)
summary(EXParimaForecast)
#time series linear model for milk sold
lnmodel<-tslm(formula = diffEXP~trend + season)
plot(lnmodel)
#forecast with linear model
lnforecast<- forecast(lnmodel, h=24)
plot(lnforecast)
#measure accuracy
accuracy(lnmodel)
accuracy(lnforecast)
checkresiduals(lnmodel)
summary(lnmodel)
ggtsdisplay(residuals(lnmodel))
##################################
#regression with arima errors input values sznl
EXPra1<- auto.arima(diffEXP, xreg = EXPdecomp$seasonal)
EXPra1
EXPra1F<-forecast(EXPra1,xreg = EXPdecomp$seasonal, h=20)
plot(EXPra1F)
#################################
##combination
fit1 <- hybridModel(diffEXP, weights="equal")
fit2 <- hybridModel(diffEXP, weights="insample")

fc1 <- forecast(fit1, h=20)
fc2 <- forecast(fit2, h=20)
autoplot(fc1) + ggtitle("Hybrid Equal Weights") + xlab("Year") + ylab(expression("USA Net Balance"))
autoplot(fc1) + ggtitle("Hybrid Insample Weights") + xlab("Year") + ylab(expression("USA Net Balance"))

accuracy(fit1)
checkresiduals(fc1)
accuracy(fc1)
accuracy(fit2)
accuracy(fc2)
checkresiduals(fc2)
#######
#forecast values with arima model
###################### regression with arima errors
#prpeare input ts
#create ts for GDP
chGDP=ts(GDP_China_tsR, start=1960, frequency=1)
plot(chGDP)
usGDP=ts(GDP_US_tsR, start=1929, frequency=1)
plot(usGDP)
#forecast future periods for chGDP
inputchGDP<-auto.arima(chGDP, stepwise = FALSE, approximation = FALSE)
ggtsdisplay(residuals(inputchGDP))
residuals(inputchGDP)
summary(inputchGDP)
accuracy(inputchGDP)
forecast(inputchGDP)
plot(forecast(inputchGDP, h=5))
#forecast future periods for usGDP
inputusGDP<-auto.arima(usGDP, stepwise = FALSE, approximation = FALSE)
ggtsdisplay(residuals(inputusGDP))
residuals(inputusGDP)
summary(inputusGDP)
accuracy(inputusGDP)
forecast(inputusGDP, h=5)
plot(forecast(inputusGDP, h=5))
#create GDP predictor ts with forecasted values and 4 repetitions per yr
predchGDP=ts(GDP_China_tsR_input, start= c(2018,3), frequency=4)
plot(predchGDP)
predusGDP=ts(GDP_US_tsR_input, start= c(2018,3), frequency=4)
plot(predusGDP)
#omit negative values for diffEXP in order to create regression
nonnegdiffEXP=ts(Diff_Exports_DYNREG, start=1992, frequency=4)
plot(nonnegdiffEXP)
#create ts to fit dynamic reg model
fitchGDP=ts(GDP_China_tsR_fitinput, start=1992, frequency=4)
fitusGDP=ts(GDP_US_tsR_fitinput, start=1992, frequency=4)
#make dynamic reg fxn
xregfit1 <- cbind(fitchGDP,fitusGDP)
diffDYNARIMA<- auto.arima(nonnegdiffEXP, xreg= xregfit1)
checkresiduals(diffDYNARIMA)
summary(diffDYNARIMA)
#forecast with GDP dynamic reg
xregfcst1 <- cbind(predchGDP,predusGDP)
fcstGDPreg<- forecast(diffDYNARIMA, xreg= xregfcst1)
plot(fcstGDPreg)
accuracy(fcstGDPreg)
#create ts for US share prices
usSHP=ts(Share_Prices_US_tsR, start=1992, frequency=4)
plot(usSHP)
#forecast future periods for US Total Share Prices
inputusSHP<-auto.arima(usSHP, stepwise = FALSE, approximation = FALSE)
ggtsdisplay(residuals(inputusSHP))
residuals(inputusSHP)
summary(inputusSHP)
accuracy(inputusSHP)
forecast(inputusSHP, h=18)
plot(forecast(inputusSHP, h=18))
#create SHare price predictor ts with forecasted values over horizon
predusSHP=ts(Share_Prices_US_tsR_input, start= c(2018,3), frequency=4)
plot(predusSHP)
#make dyn reg fxn now including total US share prices
xregfit2 <- cbind(fitchGDP,fitusGDP, usSHP)
diffDYNARIMA2<- auto.arima(nonnegdiffEXP, xreg= xregfit2)
checkresiduals(diffDYNARIMA2)
summary(diffDYNARIMA2)
#forecast with GDP+SHP dynamic reg
xregfcst2 <- cbind(predchGDP,predusGDP,predusSHP)
fcstGDPSHPreg<- forecast(diffDYNARIMA2, xreg= xregfcst2)
plot(fcstGDPSHPreg)
accuracy(fcstGDPSHPreg)
summary(fcstGDPSHPreg)
#######################################
#create ts for US/Chinese Population
chPOP=ts(China_Pop_tsR, start=1950, frequency=1)
plot(chPOP)
usPOP=ts(US_pop_tsR, start=1950, frequency=1)
plot(usPOP)
#forecast future periods for US Population
inputusPOP<-auto.arima(usPOP, stepwise = FALSE, approximation = FALSE)
ggtsdisplay(residuals(inputusPOP))
residuals(inputusPOP)
summary(inputusPOP)
accuracy(inputusPOP)
forecast(inputusPOP, h=5)
plot(forecast(inputusPOP, h=5))
#forecast future periods for Chinese Population
inputchPOP<-auto.arima(chPOP, stepwise = FALSE, approximation = FALSE)
ggtsdisplay(residuals(inputchPOP))
residuals(inputchPOP)
summary(inputchPOP)
accuracy(inputchPOP)
forecast(inputchPOP, h=5)
plot(forecast(inputchPOP, h=5))
#create ts for fitting model
fitusPOP=ts(US_pop_tsR_iput, start=1992, frequency = 4)
fitchPOP=ts(China_Pop_tsR_input, start= 1992, frequency = 4)
#create Population predictor ts with forecasted values over horizon
predchPOP=ts(China_Pop_tsR_pred, start= c(2018,3), frequency=4)
plot(predchPOP)
predusPOP=ts(US_pop_tsR_pred, start= c(2018,3), frequency=4)
plot(predusPOP)
#make dyn reg fxn now including total populations
xregfit3 <- cbind(fitchGDP,fitusGDP, fitusPOP, fitchPOP)
diffDYNARIMA3<- auto.arima(nonnegdiffEXP, xreg= xregfit3)
checkresiduals(diffDYNARIMA3)
summary(diffDYNARIMA3)
#forecast with GDP+SHP dynamic reg
xregfcst3 <- cbind(predchGDP,predusGDP, predusPOP, predchPOP)
fcstGDPSHPPOPreg<- forecast(diffDYNARIMA3, xreg= xregfcst3)
plot(fcstGDPSHPPOPreg)
accuracy(fcstGDPSHPPOPreg)
summary(fcstGDPSHPPOPreg)
#####test all
#make dyn reg fxn now including total populations
xregfit3 <- cbind(fitchPOP)
diffDYNARIMA3<- auto.arima(nonnegdiffEXP, xreg= xregfit3)
#
xregfcst3 <- cbind(predchPOP)
fcstGDPSHPPOPreg<- forecast(diffDYNARIMA3, xreg= xregfcst3)
plot(fcstGDPSHPPOPreg)
accuracy(fcstGDPSHPPOPreg)
summary(fcstGDPSHPPOPreg)
ggtsdisplay(residuals(fcstGDPSHPPOPreg))