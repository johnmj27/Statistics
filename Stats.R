install.packages("ggplot2")
install.packages("fpp2")
library(fpp2)
install.packages("forecast")
library(forecast)
library(tseries)
library(ggplot2)
# CreatingImportimg a time series
cardata <- read.csv("D:/Masters/Statistics/CA2/CarRegistrations.csv")
#names(regdata) = c("regyear", "noregestration")
cardata
# Adding New Coloumn in the existing Table 
cardata1 = cardata$NoRegistration
cardata1
tcardata1 <- ts(cardata1, start=c(1995,1), end=c(2022,1), frequency=12)
tcardata1

plot(tcardata1)
autoplot(tcardata1)
any(is.na(tcardata1))


ggseasonplot(tcardata1, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("No. of Registrations") +
  ggtitle("Ireland's Private Car Registration - Seasonal Graph Plot")

ggsubseriesplot(tcardata1) +
  ylab("No. of Registrations") +
  ggtitle("Ireland's Private Car Registration - SubSeries Graph Plot")


#Decomposing Data addictive
decomp_tcardata1 <- decompose(tcardata1, "additive")
plot(decomp_tcardata1)


decomp_tcardata2 <- decompose(tcardata1, "multiplicative")
plot(decomp_tcardata2)

#autoplot(ts.sales)
ggAcf(tcardata1, plot = TRUE)


#Box Test for correlation
Box.test(tcardata1, lag = 10, type = "Ljung-Box")
Box.test(tcardata1, lag = 10, type = "Box-Pierce")

# Mean Model 
fcast.mean <- meanf(tcardata1, h=6)
summary(fcast.mean)
plot(fcast.mean)



# naive model

fcast.naive <- naive(tcardata1, h=6)
summary(fcast.naive)
plot(fcast.naive)

# Seasonal Naive model 

fcast.snaive <- snaive(tcardata1, h=6)
summary(fcast.snaive)
plot(fcast.snaive)
checkresiduals(fcast.snaive)
# Moving Average 
rwf1 <- rwf(tcardata1, h=6 , drift=TRUE)
print(rwf1)
accuracy(rwf1)
checkresiduals(rwf1)
#STL 

stlfit <- stl(tcardata1, s.window = "period")
plot(stlfit)
stlfit$time.series

# simple exponential smoothing model 


simpleexpo <- ses(tcardata1, h = 6)
simpleexpo
simpleexpo$model
checkresiduals(simpleexpo)


autoplot(simpleexpo) +
  autolayer(fitted(simpleexpo), series="Fitted") +
  ylab("No. of registraions)") + xlab("Year")


# Holts Linear Model Fitting 

#simple holts model


simplholtnew <- holt(tcardata1, h = 6)
simplholtnew
simplholtnew$model


#ETS Model (Error, Trend, Seasonal)

etsmodel <- ets(tcardata1, model="AAN")
etsmodel
forecast(etsmodel, h=6)

autoplot(etsmodel) +
  autolayer(fitted(etsmodel), series="Fitted") +
  ylab("No. of registraions)") + xlab("Year")

#ETS Model (Error, Trend, Seasonal)

etsmodel1 <- ets(tcardata1, model="ZZZ")
etsmodel1

autoplot(etsmodel1) +
  autolayer(fitted(etsmodel1), series="Fitted") +
  ylab("No. of registraions)") + xlab("Year")

# Holt Winters 

HoltWIN <- hw(tcardata1, h=6)
HoltWIN
HoltWIN$model
accuracy(HoltWIN)

fc2 <- holt(tcardata1, damped=TRUE, phi = 0.9, h=15)
fc2
autoplot(tcardata1) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Quarter") +
  ylab("No. of registraions") +
  guides(colour=guide_legend(title="Forecast"))
#holt's wilter methodaust <- window(tcarreg,start=c(1995,1))

fit1 <- hw(tcardata1,seasonal="additive", h=6)
fit1
plot(fit1)
checkresiduals(fit1)
accuracy(fit1)
fit1$model
fit2 <- hw(tcardata1,seasonal="multiplicative", h=6)
fit2
plot(fit2)
checkresiduals(fit2)
accuracy(fit2)
fit2$model
fit3 <- hw(tcardata1, damped = TRUE, seasonal = "multiplicative", h=6)
fit3
plot(fit3)
checkresiduals(fit3)
accuracy(fit3)

#aust <- window(tcarreg,start=c(1995,1))
#holtwint1 <- hw(tcarreg, seasonal = "additive", h=6)
#holtwint2 <- hw(tcarreg, seasonal = "multiplicative", h=6)
#holtwint3<-hw(tcarreg, damped=TRUE, seasonal="multiplicative", h=6)
autoplot(tcardata1) + autolayer(fit1, series = "HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series = "HW multiplicative forecasts", PI=FALSE)+
  autolayer(fit3, series = "HW damped multiplicative forecasts", PI=FALSE)
#Seasonal Arima

fit_arima1 <- auto.arima(tcardata1,d=1,D=1,stepwise = FALSE, approximation =FALSE, trace = TRUE )
print(summary(fit_arima1))
checkresiduals(fit_arima1)

#Non-Seasonal Arima


autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Quarter") +
  ylab("Sales in Billion $") +
  ggtitle("United States E-commerce Retail Sales") +
  guides(colour=guide_legend(title="Forecast"))
#rlang::last_error()start(tcarreg)
end(tcarreg)
frequency(tcarreg)install.packages("TSstudio")
library(TSstudio)
ts_info(tcarreg)#Subsetting the series with the window function
tcarreg.subset <- window(tcarreg, start=c(1995, 1), end=c(2009, 12))
tcarreg.subset
autoplot(tcarreg.subset)
#moving average function
plot(tcarreg, main="Raw Time Series")
ma(tcarreg,order=3)
plot(ma(tcarreg,order=3))
ma(tcarreg,order=5)
plot(ma(tcarreg,order=5))
ma(tcarreg,order=7)
plot(ma(tcarreg,7))autoplot(tcarreg)+
  autolayer(ma(tcarreg,3))+
  autolayer(ma(tcarreg,5))+
  autolayer(ma(tcarreg,7))ggtsdisplay(tcarreg)#Seasonal Decompositiontcarreg
plot(tcarreg)
monthplot(tcarreg)
seasonplot(tcarreg)
#Seasonal decomposition using decompose() - additive
fit.decadd<-decompose(tcarreg, type = "additive")
fit.decadd
plot(fit.decadd)
#Seasonal decomposition using decompose() - multiplicative
fit.decmult<-decompose(tcarreg,type = "multiplicative")
fit.decmult
plot(fit.decmult)
#Seasonal decomposition using stl()
logtcarreg <- log(tcarreg)
plot(logtcarreg, ylab="log(AirPassengers)")
fit.stl <- stl(logtcarreg, s.window="period")
plot(fit.stl)
fit.stl$time.series
exp(fit.stl$time.series)
#Fitting some simple time series models#mean model
fcast.mean<-meanf(tcarreg,h=3)
summary(fcast.mean)
plot(fcast.mean)#naive model
fcast.naive<-naive(tsales,h=4)
summary(fcast.naive)
plot(fcast.naive)#seasonal naive model
fcast.seasonalnaive<-snaive(tsales,h=2)
summary(fcast.seasonalnaive)
plot(fcast.seasonalnaive)



#correlation exists as we reject the null hypothesis.


