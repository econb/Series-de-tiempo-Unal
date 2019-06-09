##Asignación de valores a las variables
precio = 100.50 
impuesto = 200
suma=precio + impuesto
mult=precio * impuesto
div=precio / impuesto
round(div, 2)
exponente = 2^3
raiz=sqrt(4)
## Vectores
v=1:10 #Secuencia de números del 1 al 10
w=seq(1,10)
m=c(0,4,6,4,3)
n=c(34,56,2,7,9)
t(m+n) ##suma
2*m ##multiplicación por escalar
##Matrices
matr=matrix(c(m,n), nrow=5, ncol=2, byrow=T) ##Matriz llenada por filas
matr2=matrix(c(m,n), nrow=5, ncol=2, byrow=F) ##Matriz llenada por columnas
##De otra forma
col1<- c(1,2,3)
col2<- c(4,5,6)
col3<- c(0,6,7)
A<- cbind(col1,col2,col3) #pegamos los vectores en columnas
B<- rbind(col1,col2,col3) #pegamos los vectores en filas
C=A%*%B ##Multiplicación de matrices
inv=solve(C) ##Inversa de la matriz
apply(A,2,sum) # Aplicar sumas por columnas
apply(B,2,var) # Aplicar varianzas por columnas
## Data frames
x1 <- c(100, 99, 100, 20)
x2 <- c(20, 19, 19, 10)
x3 <- c("A", "A", "A", "C")
notas <- data.frame(x1, x2, x3)
## Cambiar nombres
dimnames(notas) <- list(c("Pedro", "Juan", "Pablo", "Lucas"), c(
  "Trabajo", "Examen", "Concepto"))
notas[,1] ##Seleccionar primera columna
notas[1,] ##Seleccionar primera fila
notas[,c(1,2)] ##Seleccionar dos primeras columnas
notas[c(1,2),] ##Seleccionar dos primeras filas

## Grafica funciones seno y coseno
x=seq(0,2*pi,length=100) ##Secuencia de 0 a 2pi de longitud 100
plot(x,cos(x),type="l",col="blue",lwd=3,main="Seno y Coseno", xlab="", ylab="", las=1)
lines(x,sin(x),col="green",lwd=3)
legend("bottomleft",col=c("blue","green"),legend =c("Coseno","Seno"), lwd=3, bty = "n")

##ESTADÍSTICA DESCRIPTIVA
library(readxl)
Base <- read_excel("C:/Users/usuario/Downloads/Base.xlsx", sheet = "Datos")
attach(Base)
##Tabla de frecuencias
tabla=table(jx9)
tab=prop.table(table(jx9))
tabla1=table(jx10)
tab1=prop.table(table(jx10))
##Diagrama de barras jx9
diagbar=barplot(tabla,col=c("orange","blue"),
                legend.text=c("No","Sí"),main="Presencia de Oficina de Registro de Instrumentos Públicos 
        en el municipio 2015",ylim=c(0,80),
                ylab ="Frecuencias Absolutas",las=1,font.axis=4)
diagbar1=barplot(tab,col=c("orange","blue"),
                 legend.text=c("No","Sí"),main="Presencia de Oficina de Registro de Instrumentos Públicos 
        en el municipio 2015",ylim=c(0,0.8),
                 ylab ="Frecuencias Relativas",las=1,font.axis=4)
##Histograma de frecuencias
hist(jx7, freq=T, main="Número de Conciliadores en equidad en el municipio 2015",col='14', xlab="Número de conciliadores", ylab="Frecuencia")
hist(jx12, freq=T, main="Tasa Violencia Intrafamiliar Promedio",col='14', xlab="Tasa de violencia", ylab="Frecuencia")
##Boxplot
boxplot(jx7,main="Número de Conciliadores en equidad en el municipio 2015",col='4', xlab="", ylab="")
boxplot(jx12,main="Tasa Violencia Intrafamiliar Promedio",col='4', xlab="", ylab="")
##Diagrama de dispersión
plot(ix8,jx12,xlab="Tasa de violencia",ylab="Número de conciliadores", main="Diagrama de dispersión", col='6')

## Tabla de contingencia
tabla3=table(jx9,jx10)
tabla4=prop.table(tabla3,2)

##Estadísticas descriptivas
summary(Base)
mean(jx7) ##Media
var(jx12) ##Varianza
d=data.frame(jx7,jx12)
var(d) ##Matriz de varianzas y covarianzas
cor(d) ##Matriz de correlaciones
