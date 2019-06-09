## Liberías a emplear
library(readr)
library(readxl)

## Importación de base de datos "Air Passengers"
AirPassengers <- read_csv("C:/Users/usuario/Downloads/AirPassengers.csv")
AirPassengers
## Serie mensual
seriesAP=ts(AirPassengers$`#Passengers`,start=c(1949,1),frequency=12)
seriesAP
plot(seriesAP, main="Air Passengers", xlab="Fecha")
monthplot(seriesAP)

## Serie Trimestral
seriesAP_tri=ts(AirPassengers$`#Passengers`,start=c(1949,1),frequency=4)
seriesAP_tri
plot(seriesAP_tri, main="Air Passengers")
## Serie Trimestral indicando fecha final 
seriesAP_tri=ts(AirPassengers$`#Passengers`,start=c(1949,1), end=c(1960,12),frequency=4)
seriesAP_tri
plot(seriesAP_tri, main="Air Passengers")
## Estadísticas descriptivas
summary(seriesAP)


## Importación de base de datos con varias variables
Datos2 <- read_excel("C:/Users/usuario/Downloads/Base_Accidentes.xlsx")
View(Datos2)
## Serie con frecuencia mensual
HER=ts(Datos2$HER,start=c(2005,1),end=c(2018,7),frequency=12)
HER
IC=ts(Datos2$IC,start=c(2005,1),end=c(2018,7),frequency=12)
IC
ISE=ts(Datos2$ISE,start=c(2005,1),end=c(2018,7),frequency=12)
ISE
plot(HER, main="Número de heridos")
## Graficar varias series al tiempo
plot(ISE, main="",ylab="",xlab="Fecha", ylim=c(60,300))
lines(IC, main="", col="red",ylab="IC",xlab="Fecha")
## Empleando varias ventanas
par(mfrow=c(2,1))
plot(ISE, main="",ylab="ISE",xlab="Fecha")
plot(IC, main="", col="red",ylab="IC",xlab="Fecha")

## Estadísticas descriptivas
summary(HER)
summary(Datos2[,c(3:21)])


### Descomposición de la serie Air Passengers
descomposicionAP=decompose(seriesAP)
plot(descomposicionAP)

HWAP=HoltWinters(seriesAP,seasonal="additive")
plot(HWAP)
ajustados=fitted(HWAP)
plot(ajustados)