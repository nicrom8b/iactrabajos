# 1. Crear un dataset artificial de train balanceado a partir de datos generados
# aleatoriamente con distribución normal de dimensiones [100,3]. Generar los primeros
# 50 patrones (perteneciente a la clase 1) con una distribución normal con media=8 y
# SD=2. Generar los 50 patrones restantes (perteneciente a la clase 0) con una
# distribución normal con media=11 y SD=2. Use de semilla 134678.

t <- proc.time() #medir el tiempo de ejecucion del algoritom

set.seed(134678)
X1 <- cbind(c(rnorm(50,8,2)), c(rnorm(50,8,2)), c(rep(1,50)));
X2 <- cbind(c(rnorm(50,11,2)), c(rnorm(50,11,2)), c(rep(0,50)));
dataset_train <- rbind(X1,X2)


# 2. Con la misma semilla crear un dataset de test a partir de datos generados
# aleatoriamente con distribución normal de dimensiones [80,3]. Generar los primeros 40
# patrones (perteneciente a la clase 1) con una distribución normal con media=8 y SD=2.
# Generar los 40 patrones restantes (perteneciente a la clase 0) con una distribución
# normal con media=11 y SD=2.

set.seed(134678)
X1_test <- cbind(c(rnorm(40,8,2)), c(rnorm(40,8,2)), c(rep(1,40)));
X2_test <- cbind(c(rnorm(40,11,2)), c(rnorm(40,11,2)), c(rep(0,40)));
dataset_test <- rbind(X1_test,X2_test)



# 3. Aplicar 3 veces el método Hold-Out al dataset de train con un 70% para entrenar y el
# resto para prueba y entrene 10 veces las tres RNA una en cada subconjunto de
# entrenamiento, para ello se debe utilizar las semillas 134678*9,134678*7,134678*4
# respectivamente.
# Las redes deben ser entrenadas con el algoritmo Back Propagation, cada red debe
# tener 10 neuronas en la capa oculta, además se debe usar la derivada de la función de
# error como criterio de parada (la suma de errores cuadráticos) con un umbral igual a
# 0.01, función de activación logística (logistic) tanto para la capa oculta como para la
# capa de salida y learning rate =0.001.
# Utilice para realizar las predicciones de cada red el dataset de test generado en el
# punto 2, utilice también junto a este dataset los patrones que no hayan sido usados en
# el entrenamiento de ninguna de las 3 redes (OOB).

library(neuralnet)
seeds <- c(134678*9, 134678*7, 134678*2)
datos_train <- list();
datos_test <- list();
indice_test <- list();
ensemble_rna <- list();
for (i in  1:3){
  set.seed(seeds[i]);
  indice_test[[i]] <- c(sample(c(1:50),15), sample(c(51:100),15));
  datos_train[[i]] <- dataset_train[-indice_test[[i]],];
  datos_test[[i]] <- dataset_train[indice_test[[i]],];
  
  x1_tr = datos_train[[i]][,1]
  x2_tr = datos_train[[i]][,2]
  clase_tr = datos_train[[i]][,3]
  set.seed(seeds[i]);
  ensemble_rna[[i]] = neuralnet(clase_tr~x1_tr+x2_tr, act.fct = "logistic", data = datos_train[[i]], hidden = 10, threshold = 0.005, rep = 10, algorithm = "backprop", learningrate = 0.001, err.fct = "sse",linear.output = FALSE)
  print(ensemble_rna[[i]])
  
}


# 4. Graficar el dataset de test. Los patrones de la clase 1 con color rojo y los patrones de la
# clase 0 con azul. Colocar a la gráfica el título “Dataset Test”. Etiquetar los ejes de la
# gráfica con “X1” y “X2”. Guardar la gráfica en formato PDF con el nombre
# “Grafica_dataset_test.pdf”.

pdf("Grafica_dataset_test.pdf")
plot(0:20,0.:20,type='n',main='Dataset Test',xlab = 'X1',ylab = 'X2')
points(dataset_test[1:40,1],dataset_test[1:40,2], col='red')
points(dataset_test[41:80,1],dataset_test[41:80,2], col='blue')
dev.off()

# 5. Calcular la predicción del ensemble de las tres redes mediante votación.

# procedemos a juntar los indices de entrenamiento sin repetir y restarlo del total de indices. para obtener los indices que no fueron entrenados
# a estos indices no entrenados lo sumamos al dataset de Test (el de 80)

indice_all <- c(1:100)
indice_all_train <- unique(indice_all[- indice_test[[1]]],indice_all[-indice_test[[2]]], indice_all[-indice_test[[3]]])
datos_test_oob <- dataset_train[-indice_all_train,]
dataset_test <- rbind(dataset_test,datos_test_oob)

pred <- list();
predicciones <- list();
for (j in 1:3){
  pred[[j]] <- compute(ensemble_rna[[j]], dataset_test[,-3])
  predicciones[[j]] <- round(pred[[j]]$net.result)
}

matriz_pred = cbind(predicciones[[1]],predicciones[[2]],predicciones[[3]])
pred_sum <- colSums(t(matriz_pred))

pred_ensembleRNA <- array()
for (k in 1:length(pred_sum)){
  if(pred_sum[k]>=2){
    pred_ensembleRNA[k] <- 1
  }else{
    pred_ensembleRNA[k] <- 0
  }
}
  
# 6. Calcular los errores de clasificación de la primera red y del ensemble.

ErrorC_RNA1 <- sum (predicciones[[1]] != dataset_test[,3])/length(dataset_test[,3])
ErrorC_RNA2 <- sum (predicciones[[2]] != dataset_test[,3])/length(dataset_test[,3])
ErrorC_RNA3 <- sum (predicciones[[3]] != dataset_test[,3])/length(dataset_test[,3])
ErrorC_ensamble <- sum (pred_ensembleRNA != dataset_test[,3])/length(dataset_test[,3])
  

# 7. Graficar las predicciones obtenidas de la primera red. Los patrones de la clase 1 con
# color rojo y los patrones de la clase 0 con azul. Colocar a la gráfica el titulo
# “Predicciones RNA1”. Etiquetar los ejes de la gráfica con “X1” y “X2”. Guardar la gráfica
# en formato PDF con el nombre “Grafica_predicciones_RNA1.pdf”.


pdf("Grafica_predicciones_RNA1.pdf")
plot(0:20,0.:20,type='n',main='Predicciones RNA1',xlab = 'x1',ylab = 'x2')
points(dataset_test[predicciones[[1]]==1,1],dataset_test[predicciones[[1]]==1,2], col='red')
points(dataset_test[predicciones[[1]]==0,1],dataset_test[predicciones[[1]]==0,2], col='blue')
dev.off()

pdf("Grafica_predicciones_RNA2.pdf")
plot(0:20,0.:20,type='n',main='Predicciones RNA2',xlab = 'x1',ylab = 'x2')
points(dataset_test[predicciones[[2]]==1,1],dataset_test[predicciones[[2]]==1,2], col='red')
points(dataset_test[predicciones[[2]]==0,1],dataset_test[predicciones[[2]]==0,2], col='blue')
dev.off()

pdf("Grafica_predicciones_RNA3.pdf")
plot(0:20,0.:20,type='n',main='Predicciones RNA3',xlab = 'x1',ylab = 'x2')
points(dataset_test[predicciones[[3]]==1,1],dataset_test[predicciones[[3]]==1,2], col='red')
points(dataset_test[predicciones[[3]]==0,1],dataset_test[predicciones[[3]]==0,2], col='blue')
dev.off()

# 8. Graficar las predicciones obtenidas del ensemble. Los patrones de la clase 1 con color
# rojo y los patrones de la clase 0 con azul. Colocar a la gráfica el titulo “Predicciones
# Ensembles RNA”. Etiquetar los ejes de la gráfica con “X1” y “X2”. Guardar la gráfica en
# formato PDF con el nombre “Grafica_predicciones_ERNA.pdf”.

pdf("Grafica_predicciones_ERNA.pdf")
plot(0:20,0.:20,type='n',main='Predicciones Ensembles RNA',xlab = 'x1',ylab = 'x2')
points(dataset_test[pred_ensembleRNA == 1,1],dataset_test[pred_ensembleRNA == 1,2], col='red')
points(dataset_test[pred_ensembleRNA == 0,1],dataset_test[pred_ensembleRNA == 0,2], col='blue')
dev.off()
# 9. Compare los errores de generalización obtenidos con el ensemble y la primera red
# neuronal, también compare la gráfica “Grafica_predicciones_ERNA.pdf” con las
# gráficas “Grafica_predicciones_RNA1.pdf” y “Grafica_dataset_test.pdf”. Escriba sus
# conclusiones.  
#
# Conclusiones:
#   Comparando las graficas de las predicciones del ensemble con las prediciones del RNA1 vemos que son las mismas, por lo cual podemos deducir que
#   estan cometiendo el mismo error. Y esto lo podemos observar en los errores de clasificacion obtenidos, al ser iguales.
#   ErrorC_ensamble = ErorC_RNA1 = 0.1 
#   
#   Lo mismo ocurre analizando la RNA2. 
#   
#   ErrorC_ensamble = ErorC_RNA1 = ErrorC_RNA2 = 0.1 
#   
#   Pero comparandola con la RNA3 vemos que varian las graficas y tambien se ve reflejado en el error.
#   
#   ErrorC_ensamble = 0.1
#   ErrorC_RNA3 = 0.1272727273
#   
#   Podemos concluir que esto ocurre por la cantidad de convergencias a la que se llego en cada RNA, por el uso de difentes semillas.
#     RNA1 convergió en 3 
#     RNA2 convergió en 2 
#     RNA3 convergió en 2 

#   Como Nota: hemos realizado ajustes en el parametro threshold = 0.005 y para la tercera RNA cambiamos la semilla a 134678*2, 
#   dado que con las configuraciones que se propusieron no logramos que las tres redes converjan.
#   
#   Comparando las predicciones en general con los datos de test (dataset_test) vemos que un error de 10% es debido a que los clusters estan solapados, es decir,
#   no son del todo linealmente separables.
  
proc.time()-t