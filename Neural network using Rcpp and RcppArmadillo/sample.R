library(Rcpp)
library(RcppArmadillo)
#setwd("C:/Users/mukul.chaware13/Desktop/N/deep learning/neural network/NN/")

data1=read.csv("train2.csv")
id=data1$PassengerId
data1$PassengerId=NULL

data=matrix(as.numeric(unlist(data1)),nrow=nrow(data1))
train=data[1:round(0.6*nrow(data)),]
gen=data[536:713,]
val=data[714:891,]

data2=read.csv("test2.csv")
t_passenger=data2$PassengerId
data2=data2[,2:ncol(data2)]

test=matrix(as.numeric(unlist(data2)),nrow=nrow(data2))

train=list(data=train[,2:15],label=matrix(train[,1]))
gen=list(data=gen[,2:15],label=matrix(gen[,1]))
val=list(data=val[,2:15],label=matrix(val[,1]))

sourceCpp("neural_network_2.cpp")

##to create neural network object
nn=new(neural_network,14,64,1)
##to train neural network
nn$train_network(train,gen,val)
##to predict on test dataset
pred=nn$predict(test)
pred=as.data.frame(pred)

