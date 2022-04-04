# The Big Mac Index is a price index published by The Economist as an informal way of measuring the purchasing power parity (PPP) between two currencies and provides a test of the extent to which market exchange rates result in goods costing the same in different countries. It "seeks to make exchange-rate theory a bit more digestible. 
# One suggested method of predicting exchange rate movements is that, according to the law of one price, the rate between two currencies should naturally adjust so that a sample basket of goods and services should cost the same in both currencies. In the Big Mac Index, the basket in question is a single Big Mac burger as sold by the McDonald's fast food restaurant chain. The Big Mac was chosen because it is available to a common specification in many countries around the world as local McDonald's franchisees at least in theory have significant responsibility for negotiating input prices. For these reasons, the index enables a comparison between many countries' currencies.
# The Big Mac PPP exchange rate between two countries is obtained by dividing the price of a Big Mac in one country (in its currency) by the price of a Big Mac in another country (in its currency). This value is then compared with the actual exchange rate; if it is lower, then the first currency is under-valued (according to PPP theory) compared with the second, and conversely, if it is higher, then the first currency is over-valued.
# At this point, it is good to ask what is happening with Uruguay. Next the analysis for the next 10 years.

# We load the Data and the Libraries

library(tidyverse)
library(tsibble)
library(feasts)
library(readxl)
library(tseries)
library(forecast)
BigUY <- read_excel("BigUY2.xlsx")


# We convert the data to time series format

uy.ts = ts(BigUY, start = c(2011), frequency = 2)
print(uy.ts)
plot(uy.ts)

# We decompose the time series

ggtsdisplay(uy.ts)
boxplot(uy.ts ~ cycle(uy.ts))
uy.ts.desc = decompose(uy.ts)
plot(uy.ts.desc, xlab='Año')
plot(log(uy.ts))

# Trend Elimination

x = log(uy.ts)  
dif1.x = diff(x)
plot(dif1.x)

# Elimination of seasonality

dif12.dif1.x = diff(dif1.x, lag=12) 
plot(dif12.dif1.x)

# We develop what best explains the projections

arima1<- Arima(uy.ts, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=2))
arima2<- Arima(uy.ts, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=2))
arima3<- Arima(uy.ts, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=2))
arima4<- Arima(uy.ts, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=2))
arima5<- Arima(uy.ts, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=2))
arima6<- Arima(uy.ts, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=2))
arima7<- Arima(uy.ts, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=2))

# We compare the performance of the models

AIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7)
BIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7)

# We choose the best values of AIC and BIC

plot(arima6$residuals)
autoplot(acf(arima6$residuals, plot = FALSE))
autoplot(pacf(arima6$residuals, plot = FALSE))

# We make the projections based on the best model

library(ggfortify)
ggtsdiag(arima6)
auto.arima(uy.ts, stepwise = FALSE, approximation = FALSE)
forecast1<-forecast(arima5, level = c(95), h = 15)
autoplot(forecast1)
