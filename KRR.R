rm(list=ls())

####################################### functions ############################################ß
KRR <- function(string,lambda,degreeOrGamma,normalization){
  Y <- train[,4]
  if( string == "polynomialKernel" && normalization == FALSE){
    x = polyKernelK(degreeOrGamma,normalization)
    for(i in 1:nrow(test)){
      y = apply(Xtrain,1,function(x) Kpol(x,Xtest[i,],degreeOrGamma))
      Ypred[i] = t(Ytrain)%*%solve(lambda*diag(nrow(Xtrain))+x)%*%as.matrix(y)
    }
  }
  else if(string == "polynomialKernel" && normalization == TRUE){
    x = polyKernelK(degreeOrGamma,normalization)
    for(i in 1:nrow(test)){
      y = apply(Xtrain,1,function(x) KpolNorm(x,Xtest[i,],degreeOrGamma))
      Ypred[i] = t(Ytrain)%*%solve(lambda*diag(nrow(Xtrain))+x)%*%as.matrix(y)
    }
  }
  else if(string == "radialKernel" && normalization == FALSE){
    x = radialKernelK(degreeOrGamma,normalization)
    for(i in 1:nrow(test)){
      y = apply(Xtrain,1,function(x) Krad(x,Xtest[i,],degreeOrGamma))
      Ypred[i] = t(Ytrain)%*%solve(lambda*diag(nrow(Xtrain))+x)%*%as.matrix(y)
    }
  }
  else if(string == "radialKernel" && normalization == TRUE){
    x = radialKernelK(degreeOrGamma,normalization)
    for(i in 1:nrow(test)){
      y = apply(Xtrain,1,function(x) KradNorm(x,Xtest[i,],degreeOrGamma))
      Ypred[i] = t(Ytrain)%*%solve(lambda*diag(nrow(Xtrain))+x)%*%as.matrix(y)
    }
  }
  else if(string == "withoutKernel"){
    for(i in 1:nrow(test)){
      Ypred[i] = t(Ytrain)%*%solve(lambda*diag(nrow(Xtrain))+as.matrix(Xtrain)%*%(t(as.matrix(Xtrain))))%*%as.matrix(Xtrain)%*%as.numeric(t(Xtest[i,]))
    }
  }
  MeanSquareError(Ypred)
}


####### calculation of K matrix for radial kernels ###########
radialKernelK <- function(degreeOrGamma,normalization){
  if(normalization == FALSE){
    for ( i in 1:n) {
      x[,i] = apply(Xtrain,1,function(x) Krad(x,Xtrain[i,],degreeOrGamma))
    }
    return (x)
  }
  else if(normalization == TRUE){
    for ( i in 1:n) {
      x[,i] = apply(Xtrain,1,function(x) KradNorm(x,Xtrain[i,],degreeOrGamma))
    }
    return (x)
  }
}

##### for calculation of K matrix in polynomial degree #######
polyKernelK <- function(degree,normalization){
  if(normalization == TRUE){
    for ( i in 1:n) {
        x[,i] = apply(Xtrain,1,function(x) KpolNorm(x,Xtrain[i,],degree))
    }
  }
  else{
    for ( i in 1:n) {
        x[,i] = apply(Xtrain,1,function(x) Kpol(x,Xtrain[i,],degree))
      }
    }
  return (x)
}

###### function for calculation of polynomial kernel for each element x1,x2 with degree d #########
Kpol<-function(x1,x2,d){
  Kpol = ( 1 + sum(x1*t(x2)))^(d);
}


##### with normalization ######
KpolNorm <- function(x1,x2,d){
  k1   = ( 1 + sum(x1*t(x1)))^(d) + 1 ;
  k2   = ( 1 + sum(x2*t(x2)))^(d) + 1 ;
  Kpol = ((1 + sum(x1*t(x2)))^(d)+1)/(sqrt(k1)*sqrt(k2));
}

###### function for calculation of radial kernel for each element x1,x2 with certain gamma value ###############
Krad<-function(x1,x2,Gamma){
  Krad = exp(-Gamma*sum((x1-x2)^2))
}

###### with normalization ########
KradNorm <- function(x1,x2,Gamma){
  Krad = (exp(-Gamma*sum((x1-x2)^2)) + 1)/2;
}

MeanSquareError <- function(Ypred){
  MSE = sum((Ypred-test[,4])^2)/length(Ypred)
  print(MSE)
}

######################## read file , create training and test data, call functions #######################

data<-read.csv("/Users/shreyaktiwari/Downloads/coursework/machine learning/assignment3/Advertising.csv",header=TRUE)
data$X<-NULL

set.seed(12048797)
training = sample(1:nrow(data),(2/3)*nrow(data))
train <- data[training,]
test  <- data[-training,]

Ytest<-test$Sales
Xtest<-test[,-4]

Ytrain<-train$Sales
Xtrain<-train[,-4]

n = nrow(train)
x = matrix(data=NA, nrow=n, ncol=n)
y = matrix(data=NA, nrow=n, ncol=1)

Ypred <- 0

KRR("polynomialKernel",1,2,TRUE)
KRR("withoutKernel",1)
KRR("radialKernel",1,0.1,FALSE)


###### different values of degree #######
for ( i in 1:10){
  KRR("polynomialKernel",1,i,TRUE)
}

###### different values of gamma #########

KRR("radialKernel",1,0.0001,TRUE)
KRR("radialKernel",1,0.001,TRUE)
KRR("radialKernel",1,0.01,TRUE)
KRR("radialKernel",1,0.1,TRUE)
KRR("radialKernel",1,0.00001,TRUE)

########## plot for MSE of radial kernel wrt Gamma ####################
yaxis = 0
xaxis = c(0.00001 ,0.00003, 0.00005, 0.00007, 0.00009, 0.0001, 0.0003,0.0005, 0.0007, 0.0009, 0.001)
for(i in 1:length(xaxis)){
  yaxis[i] = KRR("radialKernel",1,xaxis[i],FALSE)
}

plot( xaxis, yaxis, type = "l", xlab = "gamma" , ylab = "MSE" , 
      main = "RadialKernel MSE wrt gamma without normalization")

########## plot for MSE of polynomial kernel with different degrees ###########

yaxis = 0
for(i in 1:30){
  yaxis[i] = KRR("polynomialKernel",1,i,TRUE)
}

plot( 1:30, yaxis, type = "l", xlab = "degree" , ylab = "MSE" , 
      main = "PolynomialKernel MSE wrt degrees")

######## plot for different values of lambda ###########

yaxis = 0
xaxis = c(0.0001 ,0.0003, 0.0005, 0.0007, 0.0009, 0.001, 0.003,0.005, 0.007, 0.009, 0.01)
for(i in 1:length(xaxis)){
  yaxis[i] = KRR("radialKernel",xaxis[i],0.00005,TRUE)
}

plot( xaxis, yaxis, type = "l", xlab = "lambda" , ylab = "MSE" , 
      main = "RadialKernel MSE wrt lambda with normalization")

yaxis = 0
xaxis = c(0.000009,0.00001,0.00003,0.00005,0.00007,0.00009,0.0001,0.0003, 0.0005, 0.0007, 0.0009, 0.001)
for(i in 1:length(xaxis)){
  yaxis[i] = KRR("polynomialKernel",xaxis[i],20,TRUE)
}

plot( xaxis, yaxis, type = "l", xlab = "lambda" , ylab = "MSE" , 
      main = "PolynomialKernel MSE wrt lamda")

