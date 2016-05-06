set.seed(5086)

x1 <- c(rnorm(50,8,2),rnorm(50,11,2));
data0 <- cbind(c(rnorm(50,11,2)),c(rnorm(50,11,2)),c(rep(0,50)));
data1 <- cbind(c(rnorm(50,8,2)),c(rnorm(50,8,2)),c(rep(1,50)));
dataset_all <- rbind(data1, data0);
save(dataset_all,file = "dataset_all.Rdata");

pdf("Grafica_dataset_all.pdf")
plot(0:20,0:20,type='n',main='Dataset All',xlab = 'x1',ylab = 'x2')
points(dataset_all[1:50,1],dataset_all[1:50,2], col='red')
points(dataset_all[51:100,1],dataset_all[51:100,2], col='blue')
dev.off()

set.seed(5086)
indice_test <- c(sample(c(1:50),15),sample(c(51:100),15));
dataset_test <- dataset_all[indice_test,]
dataset_train <- data.frame(dataset_all[-indice_test,])
save(dataset_test,file = "dataset_test.Rdata")
save(dataset_train,file = "dataset_train.Rdata")


pdf("Grafica_dataset_test.pdf")
plot(0:20,0:20,type='n',main='Dataset Test',xlab = 'x1',ylab = 'x2')
points(dataset_test[1:15,1],dataset_test[1:15,2], col='red')
points(dataset_test[16:30,1],dataset_test[16:30,2], col='blue')
dev.off()

 
# Entrenar 10 veces una red neuronal con el algoritmo Back Propagation, la red debe
# tener 10 neuronas en la capa oculta, además se debe usar la derivada de la función de
# error como criterio de parada (la suma de errores cuadráticos) con un umbral igual a
# 0.01 (umbral para las derivadas parciales de la función de error como criterio de
# parada.), función de activación logística (logistic) tanto para la capa oculta como para la
# capa de salida y learning rate =0.001.
X1tr <- dataset_train[,1]
X2tr <- dataset_train[,2]
clase_tr <-dataset_train[,3]
library(neuralnet)
set.seed(5086)
red <- neuralnet(clase_tr~X1tr+X2tr,dataset_train, hidden=10,rep=10,algorithm = 'backprop',threshold = 0.01, learningrate = 0.001)


plot(red,rep='best')


pred <- compute(red,dataset_test[,-3])
round(pred$net.result)
sum(round(pred$net.result) != dataset_test[,3]/length(dataset_test[,3]))
