# 1. Crear un dataset artificial balanceado a partir de datos generados aleatoriamente con
# distribución normal de dimensiones [100,3]. Generar los primeros 50 patrones
# (perteneciente a la clase 1) con una distribución normal con media=8 y SD=2. Generar
# los 50 patrones restantes (perteneciente a la clase 0) con una distribución normal con
# media=11 y SD=2.
set.seed(5086)
matriz1<-cbind(rnorm(50,8,2),rnorm(50,8,2),rep(1,50));#patrones clase 1
matriz0<-cbind(rnorm(50,11,2),rnorm(50,11,2),rep(0,50)) ;#patrones clase 0
matriz<-rbind(matriz1,matriz0) ; 

# 2. Almacenar el dataset en un data.frame con el nombre “dataset_all”.
dataset_all = as.data.frame(matriz)
save(dataset_all,file= "dataset_all.Rdata")

# 3. Graficar el dataset del punto anterior. Los patrones de la clase 1 con color rojo y los
# patrones de la clase 0 con azul. Colocar a la gráfica el título “Dataset All”. Etiquetar los
# ejes de la gráfica con “X1” y “X2”. Guardar la gráfica en formato PDF con el nombre
# “Grafica_dataset_all.pdf”.
pdf("Grafica_dataset_all.pdf")
plot(3:17,3:17,type = "n",main = "Dataset All",xlab = "X1",ylab = "X2")
points(dataset_all[1:50,1],dataset_all[1:50,2],col="red")
points(dataset_all[51:100,1],dataset_all[51:100,2],col="blue")
dev.off()

# 4. Aplicar al data frame el método Hold-Out con un 70% para entrenar y el resto para
# prueba.
set.seed(5086)
indice_test1<-sample(c(1:50),15)
indice_test2<-sample(c(51:100),15)
indice_test<-c(indice_test1,indice_test2)
dataset_test<-dataset_all[indice_test,]
dataset_train<-dataset_all[-indice_test,]

# 5.Guardar los datasets de entrenamiento y test con el nombre “dataset_train.Rdata” y
# “dataset_test.Rdata” respectivamente.
save(dataset_test,file = "dataset_test.Rdata")
save(dataset_train,file = "dataset_train.Rdata")

# 6. Graficar el dataset de test (dataset_test.Rdata). Los patrones de la clase 1 con color
# rojo y los patrones de la clase 0 con azul. Colocar a la gráfica el titulo “Dataset Test”.
# Etiquetar los ejes de la gráfica con “X1” y “X2”. Guardar la gráfica en formato PDF con
# el nombre “Grafica_dataset_test.pdf”.
pdf("Grafica_dataset_test.pdf")
plot(3:17,3:17,type="n",main="Dataset Test",xlab = "X1",ylab = "X2")
points(dataset_test[1:15,1],dataset_test[1:15,2], col="red")
points(dataset_test[16:30,1],dataset_test[16:30,2], col="blue")
dev.off()

# 7. Entrenar 10 veces una red neuronal con el algoritmo Back Propagation, la red debe
# tener 10 neuronas en la capa oculta, además se debe usar la derivada de la función de
# error como criterio de parada (la suma de errores cuadráticos) con un umbral igual a
# 0.01 (umbral para las derivadas parciales de la función de error como criterio de
# parada.), función de activación logística (logistic) tanto para la capa oculta como para la
# capa de salida y learning rate =0.001.
library(neuralnet)
x1_tr = dataset_train[,1]
x2_tr = dataset_train[,2]
clase_tr = dataset_train[,3]
set.seed(5086)
net = neuralnet(clase_tr~x1_tr+x2_tr, act.fct = "logistic", data = dataset_train, hidden = 10, threshold = 0.01, rep = 10, algorithm = "backprop", learningrate = 0.001, err.fct = "sse",linear.output = FALSE)
save(net,file = "Red_aprendida.Rdata")

# 8. Graficar la red aprendida.
plot(net, rep = "best")

# 9. Realizar las predicciones sobre el conjunto de prueba. Calcular el error de
# Clasificación.
pred = compute(net, dataset_test[,-3])
predicciones = round(pred$net.result)
errorC<-sum(round(pred$net.result)!=dataset_test[,3])/length(dataset_test[,3])

# 10. Graficar las predicciones obtenidas. Los patrones de la clase 1 con color rojo y los
# patrones de la clase 0 con azul. Colocar a la gráfica el título “Predicciones Dataset”.
# Etiquetar los ejes de la gráfica con “X1” y “X2”. Guardar la gráfica en formato PDF con
# el nombre “Grafica_predicciones_dataset.pdf”.
predic_test<-cbind(dataset_test[,-3],predicciones)
predic_test_ordenado<-predic_test[order(-predic_test$predicciones),]
p_ordenado<- as.matrix(predic_test_ordenado)
pdf("Grafica_predicciones_dataset.pdf")
plot(3:17,3:17,type = "n",main = "Predicciones Dataset",xlab = "X1",ylab = "X2")
points(p_ordenado[1:22,1],p_ordenado[1:22,2], col="red")
points(p_ordenado[23:30,1],p_ordenado[23:30,2], col="blue")
dev.off()

# 11. Compare la gráfica “Grafica_predicciones_dataset.pdf” con “Grafica_dataset_test.pdf” y
# escriba sus conclusiones.

###Se puede apreciar que para el conjunto de prueba la red se quivoco en 7 valores.
###Predijo correctamente los valores de la clase 1, no asi los de la clase 0
###Tambien se puede observar que el predictor agrupo las clases de manera que sean
###linealmente separables.
###En si nuestro error de clasificación no es muy bueno. Para cambiar esto debemos
###mejorar nuestra red cambiando valores en los parametros libres, por ej. podriamos
###cambiar el valor de la semilla, añadir mas capas ocultas, modificar el learning 
###rate, establecer un tstep max,etc.