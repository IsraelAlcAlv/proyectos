rm(list = ls())

#En el contexto financiero actual, el riesgo constituye una variable determinante 
#que amenaza la generación de valor por parte de las compañías al estar expuestas 
#a diferentes tipos de ellos, entre los que se encuentran los de mercado, liquidez, 
#operativo, entre otros.

#Por tal motivo, se hace necesario crear mecanismos de control que mitiguen el 
#riesgo existente.  El Riesgo de Crédito es la posibilidad de sufrir una pérdida 
#resultante del impago o incumplimiento de nuestra contrapartida en una operación 
#financiera. Así, desarrollaremos un Modelo Logit para estimar estas probabilidades,
#es decir, la probabilidad de que una persona pague un crédito (sí o no), en función 
#de algunas características, como su: Edad, Ingreso y la cantidad del préstamo 
#(todo ello con ayuda del lenguaje de programación R).

#El Modelo Logit es un modelo econométrico no lineal que se utiliza cuando la variable 
#dependiente es binaria o dummy, es decir que sólo puede tomar dos valores. Este se 
#basa en una distribución acumulada logística estándar.


#Comenzamos por instalar y o llamar las librerías necesarias:
#install.packages("dplyr")
#install.packages("ggplot2")
##install.packages('InformationValue')
##install.packages("vcd")

library(ggplot2)
library(dplyr)
library("InformationValue")
library(vcd)

getwd()
setwd("~/Cartel")
dir()

#Para lograr la construcción de nuestro modelo, necesitamos un conjunto de datos 
#que nos brinde información necesaria para la creación de este, dicho conjunto de 
#datos fue obtenido de (bibliografía de página). La cual nos ofrece información 
#sobre un listado de deudores de cierto banco, donde se visualiza su Edad, Ingreso, 
#Cantidad del Préstamo, y si el Préstamo fue pagado o no. 

#Iniciamos importando nuestra base de datos

original <- read.csv("original.csv", header = T)

#Como paso siguiente se realizó un análisis exploratorio de los datos, esto, con
#la finalidad de encontrar alguna inconsistencia en los mismos (ya sea, datos vacíos, 
#valores inconsistentes, etc.) y poder comprenderlos mejor.Además, se renombran algunas
#variables

str(original)
names(original)
summary(original)

original <- rename(original, ingreso = income, edad = age, prestamo = loan)

attach(original)

#Gráfico de Barras de las Personas que pagaron y las que no (Pago = 0, No Pago = 1)

ggplot(data= original, 
       aes(x = default)) + 
     geom_bar() + 
     xlab("Evento:|0 = Pago|1 = No Pago ") + 
     ylab("Frecuencia") + 
     ggtitle("Pagaron vs No Pagaron")+ 
     theme(plot.title = element_text(size=15, face = "bold", hjust = 0.5))  +
     theme(axis.text.x = element_text(face = "bold", color="blue" , 
                                      size = 10, angle = 45, 
                                      hjust = 1),
           axis.text.y = element_text(face = "bold", color=" blue" , 
                                      size = 10, angle = 45, 
                                      hjust = 1))


# Boxplots
Conf1x3 <-  matrix(c(1:3), ncol =3, byrow=FALSE)
Conf1x3


layout(Conf1x3)
layout.show(3)
boxplot(edad ~ default, xlab = "", ylab = "")
title("Boxplot - Edades por Defaults", xlab = "Evento:| Pago = 0 | No Pago = 1")
boxplot(ingreso ~ default,  xlab = "", ylab = "")
title("Boxplot - Ingresos por Defaults", xlab = "Evento:|Pago = 0 | No Pago = 1")
boxplot(prestamo ~ default,  xlab = "", ylab = "")
title("Boxplot - Cantidad Prestada por Defaults", xlab = "Evento:|Pago = 0 | No Pago = 1")

# Diagramas de Dispersión
layout(Conf1x3)
layout.show(3)
plot(default,ingreso, ylab = "Ingresos", xlab = "")
title("Dispersión - Ingresos por Defaults", xlab = "Evento:|Pago = 0 | No Pago = 1")
plot(default,edad, ylab = "Edades", xlab = "")
title("Dispersión - Edades por Defaults", xlab = "Evento:|Pago = 0 | No Pago = 1")
plot(default,prestamo, ylab = "Cantidad Prestada", xlab = "")
title("Dispersión - Cantidad Prestada por Defaults", xlab = "Evento:|Pago = 0 | No Pago = 1")

# Histogramas
Conf3x1 <-  matrix(c(1:3), nrow =3, byrow = F)


layout(Conf3x1)
layout.show(3)
hist(ingreso, breaks = 100, xlab = "Ingresos", main = "Histrograma de Ingresos")
hist(edad, breaks = 100, xlab = "Edades", main = "Histrograma de Edades")
hist(prestamo, breaks = 100, xlab = "Cantidad Prestada", main = "Histrograma de Prestamo")

detach(original)

#Dea acuerdo a esto, vemos que hay na's. edades negativas y prestamos negativos, 
#las arreglamos
original <- na.omit(original)
original[original$edad<0,3] <- original[original$edad<0,3]*(-1)

#Comprobamos el estado actual de nuestros datos
summary(original)

Conf1x3 <-  matrix(c(1:3), ncol =3, byrow=FALSE)
layout(Conf1x3)
layout.show(3)
boxplot(original$edad ~ original$default, xlab = "", ylab = "")
title("Boxplot - Edades por Defaults", xlab = "Evento:| Pago = 0 | No Pago = 1")
boxplot(original$ingreso ~ original$default,  xlab = "", ylab = "")
title("Boxplot - Ingresos por Defaults", xlab = "Evento:|Pago = 0 | No Pago = 1")
boxplot(original$prestamo ~ original$default,  xlab = "", ylab = "")
title("Boxplot - Cantidad Prestada por Defaults", xlab = "Evento:|Pago = 0 | No Pago = 1")


#AL tener una base da datos limpia, una significativa relacipon entre nuestros datos,
#Comenzamos con el Modelo

Rlog<-glm(formula=default~ingreso+edad+prestamo,original, family=binomial(link='logit'))

summary(Rlog)

#Así, de acuerdo a los resultados tenemos que:
# Por cada unidad unidad del ingreso que aumenta disminuye la probabilidad que no pague la persona,
# Por cada unidad unidad de la edad que aumenta disminuye la probabilidad que no pague la persona,
# Por cada unidad unidad de la Cantidad de Préstamo que aumenta aumenta la probabilidad que no pague la persona.

#Además, vemos que todas las variables son significativas.

#Aplicamos Exp(), para poder llegar a una interpretación mejor de nuestros ODD's:

exp(coefficients(Rlog))

#Tenemos entonces que:
# Por cada unidad unidad del ingreso que aumenta,  los odds de 
#no pagar disminuyen en un .99, lo cual es en promedio 1, esto significa que el ingreso 
#no produce ningún cambio en los odds.

# Por cada unidad unidad de la edad que aumenta, los odds de no pagar disminuyen 
# en un .70 que no pague la persona

# Por cada unidad unidad de la Cantidad de Préstamo que aumenta, los odds de no 
# pagar aumentan en un 1.0017 de que no pague la persona.

###### prueba de significancia ###### 

#Ho:El modelo no es significativo
#Ha:El modelo es significativo

#Forma 1:
# Diferencia de residuos
#dif_residuos <- Rlog$null.deviance - Rlog$deviance

# Grados libertad
#df <- Rlog$df.null - Rlog$df.residual
# p-value
#p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)
#paste("Diferencia de residuos:", round(dif_residuos, 4))
#paste("Grados de libertad:", df)
#paste("p-value:", round(p_value, 4))


#Forma 2

#Estadistico de prueba
with(Rlog,null.deviance-deviance)

#Valor p
with(Rlog,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail=FALSE))

### CONCLUSION
#Se rechaza Ho en favor de Ha, por lo tanto, nuestro modelo es significativo.

## El siguiente paso es ver, que tanta precisión tiene nuestro modelo al momento de
## predecir los valores de nuestra variable objetivo, en este caso, en que porcentaje
## es capaz de predecir a las personas que cometen Default o impago.

## Para esto, hay dos formas:

################## FORMA 1 ##################


predicciones <- ifelse(test = Rlog$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(Rlog$model$default, predicciones,
                          dnn = c("Observaciones", "Predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))



################# FORMA 2 ##################

original$Predict <- Rlog$fitted.values

View(original)

PuntoCorte<-optimalCutoff(original$default,original$Predict,optimiseFor = "Both")

PuntoCorte

obsCorrectas<-1- misClassError(original$default,original$Predict, threshold = PuntoCorte)

confusionMatrix(original$default,original$Predict)


# Por lo tanto, tenemos que el modelo es capaz de clasificar correctamente el 94.89%
# de las observaciones dadas.

#### En conclusión, El modelo logístico creado para predecir la probabilidad impago 
#### (no pago odefault) a partir del ingreso, edad y cantidad del préstamo es en conjunto
#### significativo acorde al Likelihood ratio (p-value = 1.702944e-257). El p-value de la
#### terna de predictores es significativo. Además, tiene un nivel o porcentaje de
#### clasificación alto (94.89%)