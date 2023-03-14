#Crear una carpeta o directorio de trabajo

setwd("C:/Users/sbarr/Documents/clases_USCO/2023_1/Estadística y Probabilidad/R_Sandra")
dir()


#Lectura de la base de datos

datos1<- read.csv2("CardioGoodFitness.csv",sep=",")


head(datos1)  #se muestra el encabezado de los datos
dim(datos1)
str(datos1)


#llamando la libreria
library(fdth) # libreria para el cálculo de distribución de frecuencias

library(ggplot2) # paquete de visualización de datos

dist <- fdt(datos1$Age,breaks="Sturges")# calcula la distribución de frecuencias
dist

#histogramas
hist(datos1$Age,
     breaks = "Sturges",
     # breaks = seq(-50, -350, -50),
     col = "darkgray",
     border = "gray10",
     main = "Age (years)",
     xlab = 'Age (years)',
     ylab = 'conteo')


library(plotrix)
table(datos1$Product)

count<-table(datos1$Product)

label<-paste(round(count/180*100),"%",sep=" ")

pie(count,labels = label,col=rainbow(length(label)),main="Gráfico circular")

legend("topright", c("TM195","TM498","TM798"), cex = 0.8,
       fill = rainbow(length(count)))


par(mfrow=c(1,1))




Cuentas1 <- table(datos1$
                    Product)                 #A) Tabla de frecuencias no agrupadas para Sexo
barplot(Cuentas1,                       #B) Se aplica barplot a esa tabla
        main="Caminadoras vendidas último trimestre",          #C) Título principal     
        xlab="Producto",                   #D) Tíulo del eje X
        ylab="Cantidad",                   #E) Título del eje Y
        legend = rownames(Cuentas1),    #F) Mostrar las leyendas
        ylim = c(0, 100),               #G) Ajustar límites en eje Y
        col=c("red","blue","green")            #H) Colorear las barras
)                               #I) No olvidar cerrar el paréntesis



Cuentas2 <- table( datos1$Gender,datos1$Product); 
Cuentas2


barplot(Cuentas2,                            #B) Se aplica barplot a esa tabla
        main="Diagrama de barras",           #C) Título principal     
        xlab="Producto",                     #D) Tíulo del eje X
        ylab="Frecuencias",                  #E) Título del eje Y
        legend = rownames(Cuentas2),         #F) Mostrar las leyendas
        ylim = c(0, 100),                    #G) Ajustar límites en eje Y
        col=c("green","blue"),               #H) Colorear las barras
        beside=TRUE                          #I) Para agrupar las barras
)   


B <- layout(matrix(c(1,1,1,1), 2, 2, byrow = TRUE))

par(mfrow=c(2,2), oma=c(0, 0, 4, 0))
barplot(Cuentas1, main="Diagrama simple (sin color) ", xlab="Eje X", ylab="Eje Y", 
        ylim = c(0, 100))

barplot(Cuentas2, main="Diagrama simple (a color)", xlab="Eje X", ylab="Eje Y", col=c("pink","blue"),
        ylim = c(0, 100))

barplot(Cuentas1, main="Barras apiladas", xlab="Eje X", ylab="Eje Y", col=c("yellow","brown"), 
        legend = rownames(Cuentas1), ylim = c(0, 100))

barplot(Cuentas2, main="Barras agrupadas", xlab="Eje X", ylab="Eje Y", col=c("green","red"),
        legend = rownames(Cuentas2), ylim = c(0, 100), beside=TRUE)

mtext(side=3, line=0, cex=1.2, outer=T,"Unión de diagramas de barras con par()")











#tabla de estadísticos descriptivos
#En general la variable Edad


#Medidas de tendencia central en general para la variable Diámetro
mean1 <- mean(datos1$Age)  #Promedio Diámetro
mean1
median1 <- median(datos1$Age)  #Mediana Diámetro
median1


#pegado del promedio y mediana en una tabla
tabla1_W <- cbind(round(mean1,digits=1), round(median1,digits=1))
tabla1_W
names(tabla1_W)
colnames(tabla1_W) <- c("media","mediana")
tabla1_W


#Medidas de posición
cuartil <- seq(0, 1, 0.25)
cuartil
quantile(datos1$Age, cuartil)  # todos los cuartiles para la variable W


decil1 <- seq(0, 1, 0.1)
decil1
quantile(datos1$Age, decil1)  # todos los deciles para la variable W


#Medidas de variación

ri1 <- IQR(datos1$Age)
ri1
var1 <- var(datos1$Age)
var1
sd1 <- sd(datos1$Age)
sd1
cv1 <- sd1/mean1*100
cv1

#pegado de: IR, varianza y desviación estándar en una tabla
tabla2_W <- cbind(round(ri1,digits=1), round(var1,digits=1), round(sd1,digits=1), round(cv1,digits=1))
tabla2_W
names(tabla2_W)
colnames(tabla2_W)<-c("RI","varianza","desv estándar","CV")
tabla2_W


#Ahora promedios por grupo
tapply(datos1$Age, datos1$Product, mean)


aggregate(datos1$Age~datos1$Product, data=datos1, mean)




list(Species,Petal.Width>1)

head(datos1)




#¿Cuál es la autoevaluación de la condición física de los clientes que 
#compran una cinta para correr?

a1=aggregate(datos1$Fitness~datos1$Product+datos1$Gender, data=datos1, mean)

barplot(a1,                       #B) Se aplica barplot a esa tabla
        main="Caminadoras vendidas último trimestre",          #C) Título principal     
        xlab="Producto",                   #D) Tíulo del eje X
        ylab="Cantidad",                   #E) Título del eje Y
        legend = rownames(Cuentas1),    #F) Mostrar las leyendas
        ylim = c(0, 100),               #G) Ajustar límites en eje Y
        col=c("red","blue","green")            #H) Colorear las barras
)                               #I) No olvidar cerrar el paréntesis




boxplot(datos1$Age,col = "yellowgreen",main="Box plot Edad",horizontal=TRUE, xlab="Edad")

stripchart(datos1$Age, method = "jitter", pch = 19, add = TRUE, col = "blue")





boxplot(datos1$Age ~datos1$Product , xlab = "Tipo de caminadora",horizontal = TRUE,
        main="Box plot Producto",plot=TRUE,
        ylab = "edad", ylim = c(10, 60), yaxs = "i",
        boxfill = 3:9,
        medcol = "dark blue", medcex = 2, medpch = 20)



stripchart(datos1$Age, method = "jitter", pch = 19, add = TRUE, col = "blue")


boxplot(x, horizontal = TRUE)

plot(x = datos1$Age, y = datos1$Usage,)
legend(x = "topleft", legend = c("No", "Yes"), fill = c("Black", "Red"), title = "Loan")










#convertir a numerica una variable chart
datos1$SepalLengthCm=as.numeric(datos1$SepalLengthCm)
datos1$PetalWidthCm=as.numeric(datos1$PetalWidthCm)
datos1$PetalLengthCm=as.numeric(datos1$PetalLengthCm)
datos1$SepalWidthCm=as.numeric(datos1$SepalWidthCm)

str(datos1)



#medidas de tendencia central
summary(datos1$SepalLengthCm)



#tabla de estadísticos descriptivos


#Medidas de tendencia central en general para la variable Diámetro
mean1 <- mean(datos1$SepalLengthCm)  #Promedio Diámetro
mean1
median1 <- median(datos1$SepalLengthCm)  #Mediana Diámetro
median1


#pegado del promedio y mediana en una tabla
tabla1_W <- cbind(round(mean1,digits=1), round(median1,digits=1))
tabla1_W
names(tabla1_W)
colnames(tabla1_W) <- c("media","mediana")
tabla1_W


#Medidas de posición
cuartil <- seq(0, 1, 0.25)
cuartil
quantile(datos1$SepalLengthCm, cuartil)  # todos los cuartiles para la variable W


decil1 <- seq(0, 1, 0.1)
decil1
quantile(datos1$SepalLengthCm, decil1)  # todos los deciles para la variable W


#Medidas de variación

ri1 <- IQR(datos1$SepalLengthCm)
ri1
var1 <- var(datos1$SepalLengthCm)
var1
sd1 <- sd(datos1$SepalLengthCm)
sd1
cv1 <- sd1/mean1*100
cv1

#pegado de: IR, varianza y desviación estándar en una tabla
tabla2_W <- cbind(round(ri1,digits=1), round(var1,digits=1), round(sd1,digits=1), round(cv1,digits=1))
tabla2_W
names(tabla2_W)
colnames(tabla2_W)<-c("RI","varianza","desv estándar","CV")
tabla2_W








#histogramas
hist(datos1$SepalWidthCm,
     breaks = 10,
     # breaks = seq(-50, -350, -50),
     col = "darkgray",
     border = "gray10",
     main = "Sepal Width Cm",
     xlab = 'Sepal Widht Cm',
     ylab = 'conteo')








# trazar dos histogramas en el mismo gráfico
hist (datos1$SepalWidthCm, col = rgb (0,0,1,0.2), xlim = c (0, 10),
      xlab = ' Valores ', ylab = ' Frecuencia ', main = ' Histograma por variables')
hist (datos1$PetalWidthCm, col = rgb (1,0,0,0.2), add = TRUE )
hist (datos1$PetalLengthCm, col = rgb (0,1,0,0.2), add = TRUE )
hist (datos1$SepalLengthCm, col = rgb (0,1,0,0.2), add = TRUE )

#add legend 
legend('topright',legend=c('SepalWidthCm','PetalWidthCm','PetalLengthCm','PetalLengthCm'),
       col=c('red','blue','yellow','black'),lty=c(1,1),cex=1,bty ='n')


#diagrama de tallos y hojas
stem(datos1$SepalLengthCm)

boxplot(datos1$SepalLengthCm)

boxplot(datos1$SepalLengthCm , xlab = "Sepal Length Cm",
        main="Box plot Sepal Length Cm",plot=TRUE,
        ylab = "mm", ylim = c(0, 10), yaxs = "i",
        boxfill = 3:9,
        medcol = "dark blue", medcex = 2, medpch = 20)

boxplot(datos1$SepalLengthCm ~datos1$Species , xlab = "Iris Species",horizontal = FALSE,
        main="Box plot Iris",plot=TRUE,
        ylab = "mm", ylim = c(0, 10), yaxs = "i",
        boxfill = 3:9,
        medcol = "dark blue", medcex = 2, medpch = 20)



# Color por grupo

datos1$Species <- as.factor(datos1$Species)
str(datos1$Species)


speciesID <- as.numeric(datos1$Species)
plot(datos1$SepalLengthCm, datos1$PetalWidthCm, pch = speciesID, col = "green")

plot(datos1$SepalLengthCm, datos1$PetalWidthCm, pch = speciesID, col = speciesID)

plot(datos1$PetalLengthCm, datos1$PetalWidthCm, pch = speciesID, col = speciesID)





plot(datos1[, 2:5], main = "Matriz de correlación")










# tomando solo los datos apropiados para la matriz
ma <- as.matrix(datos1[, 2:5]) # convert to matrix
colMeans(ma) # column means for matrix

pairs(ma)


pairs(ma, col = rainbow(3)[speciesID]) # set colors by species



#############DATOS AGRUPADOS########################3


#llamando la libreria
library(fdth)
library(ggplot2)

dist <- fdt(datos1$SepalLengthCm,breaks="Sturges")# calcula la distribución de frecuencias
dist

#Donde
#f= frecuencia absoluta
#rf= frecuencia relativa
#rf(%) frecuencia relativa porcentual
#cf= frecuencia acumulada
#cf(%)=frecuencia acumulada porcentual

par(mfrow=c(1,1)) # particiona mi ventana grafica en 3x2.
hist(datos1$SepalLengthCm, breaks = "Sturges", xlab="rangos diámetro(mm)",ylab="frecuencia",main="Histograma", col="blue") #histograma

tt<-table(datos1$Species)




  
  
  x <- c(8, 5, 14, -9, 19, 12, 3, 9, 7, 4,
         4, 6, 8, 12, -8, 2, 0, -1, 5, 3)
  boxplot(x, horizontal = TRUE)
  
  #Medidas de posición
  cuartil <- seq(0, 1, 0.25)
  cuartil
  quantile(x, cuartil)  # todos los cuartiles para la variable W
  
  
  
  library(ggplot2)
  library(dplyr)
  
  
  
  
  set.seed(1)
  datos <- tibble(x = rlnorm(100, meanlog = 2),
                  letra = sample(LETTERS, 100, TRUE))
  datos

  datos %>% 
    ggplot(aes(x)) +
    geom_histogram(bins = 10,
                   fill = "white", 
                   col = "gray")  
  fivenum(datos$x)  
  datos %>% 
    ggplot(aes(y = x)) +
    geom_boxplot()  
  
  
  
  
  
  
  # El paquete dplyr integra numerosas funciones para resumir y describir datos,
  #que son totalmente compatibles unas con otras y que presentan salidas más 
  #refinadas y generales que aggregate.
  # En primer lugar cargamos esta librería:
  
  library(dplyr)
  
  
  medias <- aggregate(Age~Product, datos1, mean, na.rm=TRUE)
  
  
  datos1%>% 
  group_by(datos1$Product)%>% 
    summarize(media=mean(datos1$Age,na.rm=TRUE))
  