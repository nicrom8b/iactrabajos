library(e1071)
# 1. Crear un dataset artificial balanceado a partir de datos generados aleatoriamente con distribución
# ormal de dimensiones [1000,3]. Generar los primeros 500 patrones (perteneciente a la clase 1)
# con una distribución normal con media=(5,5) y SD=2. Generar los 500 patrones restantes
# (perteneciente a la clase 0) con una distribución normal con media=(9,5) y SD=2. Guardar el
# dataset con el nombre “Vertical.Rdata”. (1,0)

vertical1 <- cbind(c(rnorm(500,5,2)),c(rnorm(500,5,2)),c(rep(1,500)));
vertical0 <- cbind(c(rnorm(500,9,2)),c(rnorm(500,5,2)),c(rep(0,500)));
vertical <- rbind(vertical1, vertical0);
save(vertical,file = "Vertical.Rdata");


# 2. Graficar el dataset del punto anterior. Los patrones de la clase 1 con color rojo y los patrones de
# la clase 0 con azul. Colocar a la gráfica el título “Dataset Vertical”. Etiquetar los ejes de la gráfica
# con “X1” y “X2”. Guardar la gráfica en formato PDF con el nombre “Grafica_Vertical.pdf”

pdf("Grafica_Vertical.pdf")
plot(-2:16,-2:16, type='n',main='Dataset Vertical',xlab = 'x1',ylab = 'x2')
points(vertical[1:500,1],vertical[1:500,2], col='red')
points(vertical[501:1000,1],vertical[501:1000,2], col='blue')
dev.off()

# 3. Realizar 30 corridas aplicando al dataset “Vertical” el método Hold-Out con un 70% para entrenar
# y el resto para prueba. Guardar los datasets de entrenamiento y test generados en dos listas
# “Vertical_train” y “Vertical_test” respectivamente.

indice_t<-c(1:1000)
vertical_test <- list();
vertical_train <- list();
for(i in 1:30){
  indice_test <-c(sample(c(1:500),150),sample(c(501:1000),150));
  vertical_test[[i]] <- vertical[indice_test,];
  vertical_train[[i]] <- vertical[-indice_test,];
}

# 4. Entrenar en cada corrida una SVM con su correspondiente conjunto de entrenamiento. Realizar
# en cada corrida las predicciones sobre el conjunto de prueba correspondiente. (1,0)

modelos<-list()
predicciones<-list()
for(i in 1:30){
  modelos[[i]] <- svm(vertical_train[[i]][,-3],vertical_train[[i]][,3], type = "C",kernel = "radial");
  predicciones[[i]] <- predict(modelos[[i]],vertical_test[[i]][,-3]);
}
# 5. Calcular el error de Clasificación de cada corrida. (1,0)

errorC<-array()
for(i in 1:30){
  errorC[i]<-sum(predicciones[[i]] != vertical_test[[i]][,3])/length(vertical_test[[i]][,3])
}

# 6. Guardar las predicciones las corridas en una lista con el nombre “predicciones” y los errores de
# clasificación de las corridas en un array con el nombre “errorC”. (0,5)

save(predicciones,file = "predicciones.Rdata");
save(errorC,file = "errorC.Rdata");
# 7. Calcular el error medio de clasificación de las corridas y escriba sus conclusiones.
ErrorDeClasificacion<-mean(errorC)
# ¿Podrías afirmar que el error medio obtenido es estadísticamente representativo? Justifique.
# Si. Por que los datos de entrenamiento y testeo son muestras representativas del dataset (balanceado)
 
# ¿Que podrías hacer para mejorar el rendimiento del clasificador?
# si bien mucho depende de la semilla del con la que se realizo el sampleo, se ha identificado que con un kernel radial se mejoro el rendimiento. 
# estos calculos fueron realizadon con en mismo sampleo en los dataset de vertical_train y vertical_test

# Error con kernel liner= 0.16033  
# Error con kernel polynomial= 0.175222
# Error con kernel radial basis = 0.12144444  <== Mejor rendimiento
# Error con kernel sigmoid = 0.2327777

# pero para lograr una mejora considereable se deberia trabajar sobre los datos de los dataset. Tratando de eliminar 
# ruidos, aumentando la cantidad de datos correctos. Pero bueno dado que es un dataset de "juguete" esto no es muy viable dado
# que no tenemos en concreto datos del contexto del problema.