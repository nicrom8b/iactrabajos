####Punto 1
#install.packages("e1071") # este comando se utiliza una sola vez para instalar la libreria
library(e1071) # siempre debemos cargar en memoria las librerias que vamos a usar 

####Punto 2
Matriz1<-matrix(rnorm(2500),50,50)
Matriz1<-cbind(Matriz1,c(rep(1,25),rep(0,25)))

datos_entrenamiento<-list() #para guardar los ejemplos de entrenamiento (lista de datos entrenamientos)  
datos_test<-list( ) #para guardar los ejemplos de test (lista de datos de prueba )
indice_t<-c(1:50) # indices de las filas de la matriz Matriz1
indice_subconjunto<-list() #aqui guardamos los indices elegidos al azar de cada subconjunto
indice_entrenamiento<-list() #aqui guardamos los indices elegidos al azar de entrenamiento

####Punto 3
# Particionamos Matriz1 en los 5 subconjuntos, aqui trabajamos solo con indices 
indice_subconjunto[[1]]<-sample(indice_t,10)
indice_usados <- indice_subconjunto[[1]]
for(i in 2:5){
indice_subconjunto[[i]]<-sample(indice_t[-indice_usados],10)
indice_usados<-c(indice_usados, indice_subconjunto[[i]])
}

# Armamos los 5 conjuntos de entrenamiento y los 5 de test
for (i in 1:5){
indice_entrenamiento[[i]]<- indice_t[-indice_subconjunto[[i]]] 
datos_entrenamiento[[i]]<-Matriz1[indice_entrenamiento[[i]],]
datos_test[[i]]<-Matriz1[-indice_entrenamiento[[i]],]
}

####Punto 4
# Entrenamos los 5 modelos
modelos<-list()
for(i in 1:5){
modelos[[i]]<-svm(datos_entrenamiento[[i]][,-51],datos_entrenamiento[[i]][,51], type="C")
}

####Punto 5
# Realizamos las predicciones de los 5 modelos ya entrenados 
predicciones<-list()
for(i in 1:5){
predicciones[[i]]<-predict(modelos[[i]],datos_test[[i]][,-51])
}

####Punto 6
#Calculamos el error de clasificaci?n
ErroresDeClasificacion<-array()
for(i in 1:5){
ErroresDeClasificacion[i]<-sum(predicciones[[i]] != datos_test[[i]][,51])/length(datos_test[[i]][,51])
}
ErrorDeClasificacion<-mean(ErroresDeClasificacion)
