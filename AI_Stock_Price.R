#Instalar paqueterias

install.packages("quantmode")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("caret")

#Llamar a las librerias

library(quantmod)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

#Crear variable "hoy"
hoy<-today()
accion<-getSymbols.yahoo("NTDOY",from = "2015-01-01", to= hoy,src="yahoo",auto.assign  =F )[,6]

plot(accion)

accion<-as.data.frame(accion)
accion$Fecha <- rownames(accion)
rownames(accion)<- NULL
names(accion) = c("Precio","Fecha")
accion$Fecha = as.Date(accion$Fecha)

rango_fecha = (hoy + 1) : (hoy + 30)
Precio = as.numeric(NA)
rango_fecha = as.data.frame(cbind(Precio,rango_fecha))
rango_fecha$Fecha = as.Date(rango_fecha$rango_fecha)
rango_fecha$rango_fecha = NULL

accion <- rbind(accion,rango_fecha)

accion$Fecha_dup = accion$Fecha
accion <- accion %>% separate(Fecha, c("Año","Mes","Dia"))
accion$Año = as.numeric(accion$Año)
accion$Mes = as.numeric(accion$Mes)
accion$Dia = as.numeric(accion$Dia)


set.seed(1994)
acciones.sc <- as.data.frame(cbind(accion$Precio, accion$Fecha_dup, scale(accion[,c(2:4)])))
names(acciones.sc)[1] = "Precio"
names(acciones.sc)[2] = "Fecha"
acciones.sc$Fecha = as.Date(acciones.sc$Fecha)


set.seed(1994)
train = createDataPartition(na.omit(subset(accion, accion$Fecha_dup < today()))$Precio,
                            p = 0.7, list = F)
test = rbind(accion[-train,] , subset(accion,accion$Fecha_dup >= today()))
test.sc = as.data.frame(cbind(test$Precio, test$Fecha_dup, scale(test[ ,c(2,3,4)])))
names(test.sc)[1] = "Precio"
names(test.sc)[2] = "Fecha"
test.sc$Fecha = as.Date(test.sc$Fecha)


install.packages("neuralnet")
install.packages("NeuralNetTools")
library(neuralnet)
library(NeuralNetTools)

mod = neuralnet(formula = Precio ~ Año + Mes + Dia, data = acciones.sc[train,], hidden = 2,
                threshold = 0.05, stepmax = 1e+08, rep = 1,linear.output = TRUE)

plotnet(mod)

pred = compute(mod, test.sc)

datos = cbind(pred$net.result, test.sc)

error_abs = RMSE(datos$Precio, datos$`pred$net.result`, na.rm = TRUE)
error_por = error_abs / datos[datos$Fecha == max(na.omit(datos)$Fecha),]$Precio

ggplot() + geom_line(data = datos, aes(x = Fecha, y = Precio), color = "blue")+
  geom_line(data = datos, aes(x = Fecha, y = pred$net.result), color = "red")


#Random Forest

install.packages("randomForest")
library(randomForest)


mod_rf = randomForest(Precio ~ Año + Mes + Dia, data = accion[train,],
                      type = "regression", ntree = 300)
pred_rf = predict(mod_rf,test)
datos_rf = cbind(pred_rf, test)

error_abs_rf = RMSE(datos_rf$Precio,datos_rf$pred_rf, na.rm = TRUE)
error_por_rf = error_abs_rf / datos_rf[datos_rf$Fecha_dup == max(na.omit(datos_rf)$Fecha_dup),]$Precio


ggplot() + geom_line(data = datos_rf, aes(x = Fecha_dup, y = Precio), color = "blue") +
  geom_line(data = datos_rf, aes(x = Fecha_dup, y = pred_rf), color = "red")
