library(e1071)

## punto 1
x11<- c(rnorm(500,6,2))
x21<- c(rnorm(500,5,2))
et1<- c(rep(1,500))  
diagonal1<-cbind(x11,x21,et1)

x10<- c(rnorm(500,12,1.5))
x20<- c(rnorm(500,11,1.5))
et2<- c(rep(0,500))  
diagonal0<-cbind(x10,x20,et2)

diagonal <- rbind(diagonal1,diagonal0)

save(diagonal,file = "Diagonal.Rdata")

pdf("Grafica_dataset_diagonal.pdf")
plot(0:16,0.:16,type='n',main='Dataset Diagonal',xlab = 'x1',ylab = 'x2')
points(diagonal[1:500,1],diagonal[1:500,2], col='red')
points(diagonal[501:1000,1],diagonal[501:1000,2], col='blue')
dev.off()


## punto5
indice_t<-c(1:1000)
indice_entrenamiento<-sample(indice_t,700)

indice_test1<-sample(c(1:500),150)
indice_test2<-sample(c(501:1000),150)
indice_test<-c(indice_test1,indice_test2)
diagonal_test<-diagonal[indice_test,]
diagonal_train<-diagonal[-indice_test,]
## punt6
save(diagonal_train,file = 'Diagonal_train.Rdata')
save(diagonal_test,file = 'Diagonal_test.Rdata')


##punto7

pdf("Grafica_dataset_diagonal_train.pdf")
if(diagonal_train)
plot(0:16,0.:16,type='n',main='Dataset Diagonal Train',xlab = 'x1',ylab = 'x2')
points(diagonal_train[1:500,1],diagonal_train[1:500,2], col='red')
points(diagonal_train[501:1000,1],diagonal_train[501:1000,2], col='blue')
dev.off()

