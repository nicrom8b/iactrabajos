# 1. Crear un dataset artificial balanceado a partir de datos generados aleatoriamente con
# distribución normal de dimensiones [100,3]. Generar los primeros 50 patrones
# (perteneciente a la clase 1) con una distribución normal con media=8 y SD=2. Generar
# los 50 patrones restantes (perteneciente a la clase 0) con una distribución normal con
# media=11 y SD=2. Use de semilla 134678.
set.seed(134678);
X1 <- cbind(c(rnorm(50,8,2)), c(rnorm(50,8,2)), c(rep(1,50)));
X2 <- cbind(c(rnorm(50,11,2)), c(rnorm(50,11,2)), c(rep(0,50)));
matriz <- rbind(X1,X2)

# 2. Aplicar al dataset el método Hold-Out con un 70% para entrenar y el resto para prueba.
indice_test <- c(sample(c(1:50),15), sample(c(51:100),15))
dataset_train <- matriz[-indice_test,]
dataset_test <- matriz[indice_test,]


# 3. Graficar el dataset de test. Los patrones de la clase 1 con color rojo y los patrones de la
# clase 0 con azul. Colocar a la gráfica el titulo “Dataset Test”. Etiquetar los ejes de la
# gráfica con “X1” y “X2”. Guardar la gráfica en formato PDF con el nombre
# “Grafica_dataset_test.pdf”.

pdf("Grafica_dataset_test.pdf")
plot(0:20,0.:20,type='n',main='Dataset Test',xlab = 'X1',ylab = 'X2')
points(dataset_test[1:15,1],dataset_test[1:15,2], col='red')
points(dataset_test[16:30,1],dataset_test[16:30,2], col='blue')
dev.off()
# 4. Entrenar un ensemble de 1, 10, 100, 1000, 10000, 100y 1000 y 1000000 árboles de
# decisión usando Random Forest sobre el dataset de entrenamiento. Realizar las
# predicciones sobre el conjunto de test usando el ensemble de árboles (RF) y guarde
# los errores de clasificación de los ensembles en un vector. Use de semilla 134678 para
# cada ensemble.


library(randomForest);
modeloRF <- list();
errorC <- array();
numtree <- 1;
for (i in 1:7){
  set.seed(134678);
  modeloRF[[i]] <- randomForest(dataset_train[,-3], as.factor(dataset_train[,3]),dataset_test[,-3], as.factor(dataset_test[,3]), norm.votes=FALSE, ntree=numtree)           
  errorC[i] <- mean(modeloRF[[i]]$test$err.rate[,1])
  numtree <- numtree * 10;
}


# 5. Compare los errores de generalización obtenidos con los ensembles y realice sus
# conclusiones.

errorC
# [1] 0.2000000 0.2033333 0.1420000 0.1650333 0.1665033 0.1666503 0.1666650

# 6. Graficar las predicciones obtenidas del ensemble que posea el menor error. Los
# patrones de la clase 1 con color rojo y los patrones de la clase 0 con azul. Colocar a la
# gráfica el título “Predicciones RF”. Etiquetar los ejes de la gráfica con “X1” y “X2”.
# Guardar la gráfica en formato PDF con el nombre “Grafica_predicciones_RF.pdf”.


pdf("Grafica_predicciones-RF.pdf")
plot(0:20,0.:20,type='n',main='Predicciones RF',xlab = 'x1',ylab = 'x2')
points(dataset_test[modeloRF[[7]]$test$predicted == 1,1],dataset_test[modeloRF[[7]]$test$predicted == 1,2],col="red");
points(dataset_test[modeloRF[[7]]$test$predicted == 0,1],dataset_test[modeloRF[[7]]$test$predicted == 0,2],col="blue");
dev.off()

