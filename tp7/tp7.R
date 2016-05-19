## 1. Crear un dataset artificial balanceado a partir de datos generados aleatoriamente con
## distribución normal de dimensiones [100,3]. Generar los primeros 50 patrones
## (perteneciente a la clase 1) con una distribución normal con media=8 y SD=2. Generar
## los 50 patrones restantes (perteneciente a la clase 0) con una distribución normal con
## media=11 y SD=2.
set.seed(134678)
matriz1<-cbind(rnorm(50,8,2),rnorm(50,8,2),rep(1,50));#patrones clase 1
matriz0<-cbind(rnorm(50,11,2),rnorm(50,11,2),rep(0,50)) ;#patrones clase 0
matriz<-rbind(matriz1,matriz0) ;
#guardo mi dataset
dataset_all = as.data.frame(matriz)
save(dataset_all,file= "dataset_all.Rdata")
#
## 2. Aplicar al dataset el método Hold-Out con un 70% para entrenar y el resto para prueba.
set.seed(134678)
indice_test1<-sample(c(1:50),15)
indice_test2<-sample(c(51:100),15)
indice_test<-c(indice_test1,indice_test2)
dataset_test<-dataset_all[indice_test,]
dataset_train<-dataset_all[-indice_test,]
#guardo mi conjunto de entrenamiento y prueba
save(dataset_test,file = "dataset_test.Rdata")
save(dataset_train,file = "dataset_train.Rdata")
#
## 3. Graficar el dataset de test. Los patrones de la clase 1 con color rojo y los patrones de la
## clase 0 con azul. Colocar a la gráfica el título “Dataset Test”. Etiquetar los ejes de la
## gráfica con “X1” y “X2”. Guardar la gráfica en formato PDF con el nombre
## “Grafica_dataset_test.pdf”

pdf("Grafica_dataset_test.pdf")
plot(2:17,2:17,type="n",main="Dataset Test",xlab = "X1",ylab = "X2")
points(dataset_test[1:15,1],dataset_test[1:15,2], col="red")
points(dataset_test[16:30,1],dataset_test[16:30,2], col="blue")
dev.off()

## 4. Entrenar 10 veces una red neuronal con el algoritmo Back Propagation, la red debe
## tener 10 neuronas en la capa oculta, además se debe usar la derivada de la función de
## error como criterio de parada (la suma de errores cuadráticos) con un umbral igual a
## 0.01, función de activación logística (logistic) tanto para la capa oculta como para la
## capa de salida y learning rate =0.001. (En caso de no converger ajuste los parámetros
## necesarios para asegurar la convergencia).

library(neuralnet)
x1_tr = dataset_train[,1]
x2_tr = dataset_train[,2]
clase_tr = dataset_train[,3]
set.seed(134678)
net = neuralnet(clase_tr~x1_tr+x2_tr, act.fct = "logistic", data = dataset_train, hidden = 10, threshold = 0.01, rep = 10, algorithm = "backprop", learningrate = 0.001, err.fct = "sse",linear.output = FALSE)
save(net,file = "Red_aprendida.Rdata")

## 5. Graficar la red aprendida.
plot(net, rep = "best")

## 6. Realizar las predicciones sobre el conjunto de prueba. Calcular el error de Clasificación
## de la red
pred = compute(net, dataset_test[,-3])
predicciones = round(pred$net.result)
errorC<-sum(round(pred$net.result)!=dataset_test[,3])/length(dataset_test[,3])

## 7. Graficar las predicciones obtenidas. Los patrones de la clase 1 con color rojo y los
## patrones de la clase 0 con azul. Colocar a la gráfica el título “Predicciones RNA”.
## Etiquetar los ejes de la gráfica con “X1” y “X2”. Guardar la gráfica en formato PDF con
## el nombre “Grafica_predicciones_RNA.pdf”.
predic_test<-cbind(dataset_test[,-3],predicciones)
predic_test_ordenado<-predic_test[order(-predic_test$predicciones),]
p_ordenado<- as.matrix(predic_test_ordenado)
pdf("Grafica_predicciones_RNA.pdf")
plot(2:17,2:17,type = "n",main = "Predicciones RNA",xlab = "X1",ylab = "X2")
points(p_ordenado[1:14,1],p_ordenado[1:14,2], col="red")
points(p_ordenado[15:30,1],p_ordenado[15:30,2], col="blue")
dev.off()

## 8. Entrenar una SVM con los mismos patrones de entrenamiento usados para entrenar la
## RNA del punto 4.
library(e1071)
dataset_train_SVM<-dataset_train
modeloSVM<-svm(dataset_train_SVM [,-3], dataset_train_SVM [,3],type="C")

## 9. Realizar las predicciones sobre el mismo conjunto de prueba usado en el punto 6.
## Calcular el error de clasificación de la SVM.
predicciones_SVM<-predict(modeloSVM,dataset_test[,-3])
errorC_SVM<-sum(predicciones_SVM != dataset_test [,3])/length(dataset_test [,3])

## 10. Graficar las predicciones obtenidas. Los patrones de la clase 1 con color rojo y los
## patrones de la clase 0 con azul. Colocar a la gráfica el título “Predicciones SVM”.
## Etiquetar los ejes de la gráfica con “X1” y “X2”. Guardar la gráfica en formato PDF con
## el nombre “Grafica_predicciones_SVM.pdf”.
pdf("Grafica_predicciones_SVM.pdf")
plot(2:17,2:17,type="n", main="Predicciones SVM ", xlab="X1", ylab="X2")
points(dataset_test[predicciones_SVM==1,1],dataset_test[predicciones_SVM==1,2],col="red")
points(dataset_test[predicciones_SVM==0,1],dataset_test[predicciones_SVM==0,2],col="blue")
dev.off()

## 11. Compare las tres gráficas:  “Grafica_predicciones_RNA.pdf”,“Grafica_predicciones_SVM.pdf”
## y “Grafica_dataset_test.pdf”. Escriba sus conclusiones.

#En predicciones de RNA predice 16 datos para la clase 0 y 14 para la clase 1
#En predicciones de RNA predice 14 datos para la clase 0 y 16 para la clase 1
# Ambas predicciones tienen el mismo error de clasificación sobre el conjunto de prueba
