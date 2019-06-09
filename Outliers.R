library(tsoutliers)
library(forecast)

###### Outlier aditivo

set.seed(12)
n=200
serie=arima.sim(n=n,list(ar=(0.5)))
serie2=serie
serie2[50]=serie2[50]+6

par(mfrow=c(1,2))
plot(serie,ylim=c(-3,6),main="Serie original")
plot(serie2,ylim=c(-3,6),main="Outlier aditivo")
auto.arima(serie2)
fit= arima(serie2,order=c(1,0,0),include.mean = F)
fit
resi= residuals(fit)
plot(resi)
coef= coefs2poly(fit)
outliers= locate.outliers(resi,coef)
tso(serie2)
xreg = outliers.effects(outliers, n)
tso(y=serie2,xreg=xreg,types=c("AO")  )
arima(serie2,order=c(1,0,0),xreg=xreg,include.mean = F)

##### Pronóstico 
fit= arima(serie2,order=c(1,0,0),include.mean = F)
fit2= arima(serie2,order=c(1,0,0),include.mean = F,xreg=xreg)
pronostico=  forecast(object=fit,h=1)                    #0.08509943   
regresoras=0
pronostico_out=forecast(object=fit2,xreg=regresoras,h=1)  #0.09981112
par(mfrow=c(1,2))
plot(pronostico$residuals)
plot(pronostico_out$residuals)  


#####Outlier cambio de nivel

set.seed(12)
n=500
serie=arima.sim(n=n,list(ar=0.3))
serie2=serie
serie2[100:n]=serie2[100:n]+4
par(mfrow=c(1,2))
plot(serie,ylim=c(-3,7),main="Serie original")
plot(serie2,ylim=c(-3,7),main="Outlier cambio de nivel")
fit= arima(serie2,order=c(1,0,0),include.mean = F)
resi= residuals(fit)
coef= coefs2poly(fit)
outliers= locate.outliers(resi,coef,c=5)
xreg = outliers.effects(outliers, n)
tso(y=serie2,xreg=xreg,types=c("LS"),maxit = 1)
plot(tso(y=serie2,types=c("LS"),maxit = 1))
arima(serie2,order=c(1,0,0),xreg=xreg,include.mean = F)
plot(serie2)

##### Pronóstico 
fit= arima(serie2,order=c(1,0,0),include.mean = F)
fit2= arima(serie2,order=c(1,0,0),include.mean = F,xreg=xreg)
pronostico=  forecast(object=fit,h=15)  #4.093698                      
regresoras=c(rep(1,15))
pronostico_out=forecast(object=fit2,xreg=regresoras,h=15) 
par(mfrow=c(1,2))
plot(pronostico,ylim=c(-3,7))
plot(pronostico_out,ylim=c(-3,7))  
plot(pronostico$residuals,ylim=c(-3,5))
plot(pronostico_out$residuals,ylim=c(-3,5))  


### Análisis AirPassengers
library(readr)
data <- read_csv("C:/Users/usuario/Downloads/AirPassengers.csv")
serie=ts(data$`#Passengers`, start=c(1949,1), frequency=12)
fit=auto.arima(serie)
outliers=tso(serie)
##Valores ajustados
fitted(fit)

ajuste=arima(log(serie),order=c(0,1,1),seasonal = list(order = c(0, 1, 1)),include.mean=F)
resi= residuals(ajuste)
coef= coefs2poly(ajuste)
outliers1= locate.outliers(resi,coef)

n=length(serie)
xreg = outliers.effects(outliers1,n )
tso=tso(log(serie),xreg=xreg,types=c("AO"))

####Estimación del modelo con variables regresoras een funcion Arima
modelo=Arima(log(serie),order=c(0,1,1),seasonal = list(order = c(0, 1, 1)),include.mean=F, xreg=xreg)
