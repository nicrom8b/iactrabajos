library(e1071)

matriz1=cbind(matrix(rnorm(2500),nrow=50),c(rep(0,25),rep(1,25)))

datos_entrenamiento=list()
datos_test=list()
indice_t=c(1:50)
indice_subconjunto=list()
indice_entrenamiento=list()

indice_subconjunto[[1]]=sample(indice_t,10)
for(i in 2:5){
	indice_subconjunto[[i]]=sample(indice_t[-indice_subconjunto],10)
}






modelo=list()
for (i in 1:5){
	modelos[[i]]=svm(datos_entrenamiento[[i]][,-51],datos_entrenamiento[[i]][,51], type="c")
}