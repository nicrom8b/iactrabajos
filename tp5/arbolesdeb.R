#1
set.seed(134678)
library(randomForest)

X1 <- cbind(c(rnorm(50,8,2)), c(rnorm(50,8,2)), c(rep(1,50)));
X2 <- cbind(c(rnorm(50,11,2)), c(rnorm(50,11,2)), c(rep(0,50)));
matriz <- rbind(X1,X2)

#2
indice_test <- c(sample(c(1:50),15), sample(c(51:100),15))
dataset_train <- matriz[-indice_test,]
dataset_test <- matriz[indice_test,]

#3
pdf("Grafica_dataset_test.pdf")
plot(0:14,0.:14,type='n',main='Dataset Test',xlab = 'X1',ylab = 'X2')
points(dataset_test[1:15,1],dataset_test[1:15,2], col='red')
points(dataset_test[16:30,1],dataset_test[16:30,2], col='blue')
dev.off()
#4
#dataset_train[,3]
numTree=1
#errors <- list()
errorMean <- array()
models <- list()
for(i in 1:7){
  set.seed(134678)
  models[[i]] <- randomForest(x=dataset_train[,-3], y=as.factor(dataset_train[,3]), xtest=dataset_test[,-3],ytest=as.factor(dataset_test[,3]),ntree=numTree)
  #errors[[i]] <- models[[i]]$err.rate[,1]
  errorMean[i] <- mean(models[[i]]$err.rate[,1])
  numTree <- numTree*10
}
#4
#Se puede observar que el promedio de errores de cada ensamble es:
#[1] 0.1250000 0.1647143 0.1383286 0.1363614 0.1404561 0.1419866 0.1431814
#
#6

