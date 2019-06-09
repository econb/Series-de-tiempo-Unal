####Simulación de modelos ARMA
library(forecast)
library(lmtest)

### ACF teórico del proceso
acf_ARMA<- ARMAacf(ar=c(0.5),ma=c(0.5), lag.max = 10)
lags <- 0:10 
plot(lags,acf_ARMA,xlim=c(1,10),ylab = "ACF",type="h",main = "ACF para ARMA(1,1)")
abline(h=0) #Agrega un eje al gráfico

### PACF teórico
pacf_ARMA<- ARMAacf(ar=c(0.5),ma=c(0.5), lag.max = 36, pacf = TRUE)
plot(pacf_ARMA, type = "h",main = "PACF para ARMA(1,1)")
abline(h=0)

# Simulación AR(1)
set.seed(186)
n=100
simAR=arima.sim(list(ar = c(0.5)), n = n, sd = 1)
plot(simAR)
# Autocorrelación teórica
acf_teor <- ARMAacf(ar=c(0.5), lag.max = 20)
lags <- 0:20 
plot(lags,acf_teor,xlim=c(1,20),ylab = "ACF",type="h",main = "ACF teórico")
abline(h=0)
# Autocorrelación muestral
acf_muest <- acf(simAR, plot=F, lag.max = 20)
acf(simAR, main="ACF AR(1)")
pacf(simAR,  main="PACF AR(1)")

##Intervalo de confianza
acf_AR <- acf(simAR, plot=F, lag.max = 25, type = "covariance")
acf_AR <- acf_AR$acf
#Autocovarianza estimada
cov = 0
k = 26 #Cuarta parte del tamaño de muestra
for (h in 1:k-1){
  cov=sum((1-(abs(h)/k))*acf_AR[h])
}

lim_sup <- mean(simAR) + 1.96*sqrt(corr/n)
lim_inf <- mean(simAR) - 1.96*sqrt(corr/n)
int <- cbind(lim_inf, lim_sup)
int      


##Simulación MA(2)
set.seed(186)
n=100
simMA<-arima.sim(n = n, list(ma = c(0.5,0.7)), sd=1)
simMA<- simMA+10
plot(simMA)
acf_MA <- acf(simMA, plot=F)
acf(simMA, main="Autocorrelograma Simple MA(2)")
pacf(simMA, main="Autocorrelograma Parcial MA(2)")


##Simulación Serie ARMA(1,1)
simARMA<-arima.sim(n = 100, list(ar=c(0.5), ma = c(0.5)), sd=1)
plot(simARMA)
acf(simARMA, main="Autocorrelograma Simple ARMA(1,1)")
pacf(simARMA, main="Autocorrelograma Parcial ARMA(1,1)")

##Ajuste automático con auto.arima
ajuste <- auto.arima(simARMA)
coeftest(ajuste)

##Ajuste de la Serie con arima
estARMA <- arima(simARMA, order=c(1, 0, 1), include.mean = F)
summary(estARMA)
coeftest(estARMA)
