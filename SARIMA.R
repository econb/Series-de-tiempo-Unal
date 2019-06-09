library(urca)
library(forecast)
library(tseries)
library(lmtest)

######Ajuste Serie datos AirPassengers  

data(AirPassengers)
plot(AirPassengers,type="l")
acf(AirPassengers)
pacf(AirPassengers)

##Test Dickey Fuller
adf.test(log(AirPassengers), k=12)
summary(ur.df(log(AirPassengers),selectlags="AIC", lags=12))

adf.test(diff(log(AirPassengers)))

##Autocorrelacion muestral y parcial
acf(log(AirPassengers))
pacf(log(AirPassengers))

acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

##Ajuste del modelo

#Modelo MA(1) estacional
modelo <- Arima(AirPassengers, c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12), lambda = 0)
coeftest(modelo)

## An?lisis de residuales
residuales <- modelo$residuals
acf(residuales)
pacf(residuales)
#Test de normalidad
jarque.bera.test(residuales)
#Test de autocorrelaci?n
Box.test(residuales, lag = (length(residuales)/4), type = "Ljung-Box", fitdf = 2)


###Estad?ticas CUSUM
res=residuales
cum=cumsum(res)/sd(res)
N=length(res)
cumq=cumsum(res^2)/sum(res^2)
Af=0.948 ###Cuantil del 95% para la estad????stica cusum
co=0.14422####Valor del cuantil aproximado para cusumsq para n/2=200
LS=Af*sqrt(N)+2*Af*c(1:length(res))/sqrt(N)
LI=-LS
LQS=co+(1:length(res))/N
LQI=-co+(1:length(res))/N
plot(cum,type="l",ylim=c(min(LI),max(LS)),xlab="t",ylab="",main="CUSUM")
lines(LS,type="S",col="red")
lines(LI,type="S",col="red")
#CUSUM Square
plot(cumq,type="l",xlab="t",ylab="",main="CUSUMSQ")                      
lines(LQS,type="S",col="red")                                                                           
lines(LQI,type="S",col="red")

##Pron?sticos
Pronosticos=forecast(modelo,h=12,level=0.95)
plot(Pronosticos)
