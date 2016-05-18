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

pdf("Grafica_dataset_test.pdf")
plot(0:20,0.:20,type='n',main='Dataset Test',xlab = 'X1',ylab = 'X2')
points(dataset_test[1:40,1],dataset_test[1:40,2], col='red')
points(dataset_test[41:80,1],dataset_test[41:80,2], col='blue')
dev.off()

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
seeds <- c(134678*9, 134678*7, 134678*4)
datos_train <- list();
datos_test <- list();
indice_test <- list();
ensamble_rna <- list();
for (i in  1:3){
  set.seed(seeds[i]);
  indice_test[[i]] <- c(sample(c(1:50),15), sample(c(51:100),15));
  datos_train[[i]] <- dataset_train[-indice_test[[i]],];
  datos_test[[i]] <- dataset_train[indice_test[[i]],];
  
  x1_tr = datos_train[[i]][,1]
  x2_tr = datos_train[[i]][,2]
  clase_tr = datos_train[[i]][,3]
  set.seed(seeds[i]);
  ensamble_rna[[i]] = neuralnet(clase_tr~x1_tr+x2_tr, act.fct = "logistic", data = datos_train[[i]], hidden = 10, threshold = 0.005, rep = 10, algorithm = "backprop", learningrate = 0.001, err.fct = "sse",linear.output = FALSE)
  print(ensamble_rna[[i]])
  
}

for (j in 1:3){
  pred <- list();
  pred[[j]] <- compute(ensambe_rna[[j]], datos_test_test[[i]][,-3])
  predicciones[[j]] <- round(pred[[j]]$net.result)
  # errorC<-sum(round(pred$net.result)!=dataset_test[,3])/length(dataset_test[,3])
}

matriz_pred = cbind(predicciones[[1]],predicciones[[2]],predicciones[[3]])


# juntar los indices de entrenamiento sin repetir y restarlo del total de indices. para obtener los indices que no fueron entrenados
# a estos indices no entrenados lo sumamos al dataset de Test (el de 80)


# votacion



proc.time()-t