# 1. Crear un dataset artificial balanceado a partir de datos generados aleatoriamente con
# distribución normal de dimensiones [100,3]. Generar los primeros 50 patrones
# (perteneciente a la clase 1) con una distribución normal con media=8 y SD=2. Generar
# los 50 patrones restantes (perteneciente a la clase 0) con una distribución normal con
# media=11 y SD=2. Use de semilla 134678.
set.seed(134678);
x1 <- c(rnorm(50,8,2), rnorm(50,11,2));
x2 <- c(rnorm(50,8,2), rnorm(50,11,2));
clase <- c(rep(1,50), rep(0,50));

dataset <- cbind(x1,x2,clase);

# 2. Aplicar al dataset el método Hold-Out con un 70% para entrenar y el resto para prueba.
indice_test1<-sample(1:50,15)
indice_test2<-sample(51:100,15)
indice_test<-c(indice_test1,indice_test2)

dataset_test <- dataset[indice_test];
dataset_train <- dataset[-indice_test];

# 3. Graficar el dataset de test. Los patrones de la clase 1 con color rojo y los patrones de la
# clase 0 con azul. Colocar a la gráfica el titulo “Dataset Test”. Etiquetar los ejes de la
# gráfica con “X1” y “X2”. Guardar la gráfica en formato PDF con el nombre
# “Grafica_dataset_test.pdf”.

pdf("Grafica_dataset_test.pdf")
plot(0:16,0.:16,type='n',main='Dataset Test',xlab = 'x1',ylab = 'x2')
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
  modeloRF <- randomForest(dataset[,-3], as.factor(dataset_train(,3)),dataset_test[,-3], as.factor(dataset_test(,3)), norm.votes=FALSE, ntree=numtree);             
  errorC[i] <- mean(modeloRF[[i]]$test$err.rate[,1])
  numtree <- numtree * 10;
}


# 5. Compare los errores de generalización obtenidos con los ensembles y realice sus
# conclusiones.

errorC
# 6. Graficar las predicciones obtenidas del ensemble que posea el menor error. Los
# patrones de la clase 1 con color rojo y los patrones de la clase 0 con azul. Colocar a la
# gráfica el título “Predicciones RF”. Etiquetar los ejes de la gráfica con “X1” y “X2”.
# Guardar la gráfica en formato PDF con el nombre “Grafica_predicciones_RF.pdf”.


pdf("Grafica_dataset_diagonal.pdf")
plot(0:16,0.:16,type='n',main='Dataset Diagonal',xlab = 'x1',ylab = 'x2')
points(dataset_test[modeloRF[[7]]$test$predicted==1,i],dataset_test[modeloRF[[7]]$test$predicted==i,2],col="red");
points(diagonal[501:1000,1],diagonal[501:1000,2], col='blue');
dev.off()