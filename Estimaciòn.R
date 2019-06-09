library(lmtest)
library(forecast)
library(tseries)

data("AirPassengers")
plot(AirPassengers)

### Transformación de la serie
logAirP=log(AirPassengers)
plot(logAirP)
acf(logAirP)
pacf(logAirP)
#Diferencia ordinaria
dif_logAirp=diff(logAirP)
plot(dif_logAirp)
acf(dif_logAirp,lag.max=48)
pacf(dif_logAirp,lag.max=48)
#Diferencia estacional
dif_logAirp_12=diff(dif_logAirp,lag=12)
plot(dif_logAirp_12)
acf(dif_logAirp_12,lag.max=48)
pacf(dif_logAirp_12,lag.max=48)
AR12=arima(dif_logAirp_12,order=c(12,0,0))
MA12=arima(dif_logAirp_12,order=c(0,0,12))
coeftest(AR12)
coeftest(MA12)

AR12_fixed=arima(dif_logAirp_12,order=c(12,0,0),include.mean = FALSE,fixed=c(NA,0,0,0,0,0,0,0,0,0,0,NA),method = c("CSS-ML"))
coeftest(AR12_fixed)

MA12_fixed=arima(dif_logAirp_12,order=c(0,0,12),include.mean = FALSE,fixed=c(NA,0,0,0,0,0,0,0,0,0,0,NA),method = c("CSS-ML"))
coeftest(MA12_fixed)

## Análisis de residuales
residuales=MA12_fixed$residuals
acf(residuales)
pacf(residuales)
#Test de normalidad
jarque.bera.test(residuales)
#Test de autocorrelaciòn
Box.test(residuales, lag = length(residuales), type = "Ljung-Box", fitdf = 2)


###Estadísticas CUSUM
res=residuales
cum=cumsum(res)/sd(res)
N=length(res)
cumq=cumsum(res^2)/sum(res^2)
Af=0.948 ###Cuantil del 95% para la estadìstica cusum
co=0.14013####Valor del cuantil aproximado para cusumsq para n/2=200
LS=Af*sqrt(N)+2*Af*c(1:length(res))/sqrt(N)
LI=-LS
LQS=co+(1:length(res))/N
LQI=-co+(1:length(res))/N
plot(cum,type="l",ylim=c(min(LI),max(LS)),xlab="t",ylab="",main="CUSUM")
lines(LS,type="S",col="red")
lines(LI,type="S",col="red")
#CUSUMSQ
plot(cumq,type="l",xlab="t",ylab="",main="CUSUMSQ")                      
lines(LQS,type="S",col="red")                                                                           
lines(LQI,type="S",col="red")

##Pronósticos
PronosticosAR12=forecast(AR12_fixed,h=12,level=0.95)
PronosticosMA12=forecast(MA12_fixed,h=12,level=0.95)
plot(PronosticosAR12)
plot(PronosticosMA12)

#Rolling
h <- 1
train <- window(dif_logAirp_12,end=c(1958,9))
test <- window(dif_logAirp_12,start=c(1958,10))
n <- length(test)
fcmat <- matrix(0, nrow=n, ncol=h)
for(i in 1:n)
{  
  x <- window(dif_logAirp_12, end=c(1958,8)+ (i)/12)
  refit <- Arima(x,order=c(0,0,12),include.mean = FALSE,fixed=c(NA,0,0,0,0,0,0,0,0,0,0,NA),method = c("CSS-ML"))  
  fcmat[i,] <- forecast(refit, h=h)$mean
}
plot(fcmat, type="l")