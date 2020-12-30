library(datasets)
library(mvtnorm)
library(nnet)
library(MASS)

data(iris)
summary(iris)

##########      Q1      ##########      

plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, main="Scatter of original data")


##########      Q2      ##########      

mean_length <- tapply(iris$Sepal.Length, iris$Species, mean)
mean_width <- tapply(iris$Sepal.Width, iris$Species, mean)

iris_setosa <- subset(iris, iris$Species == 'setosa')
iris_versicolor <- subset(iris, iris$Species == 'versicolor')
iris_virginica<- subset(iris, iris$Species == 'virginica')

prior_probabilities <- 0.333333  
cov_setosa <- cov(iris_setosa[,1:2]) #* prior_probabilities
cov_versicolor <- cov(iris_versicolor[,1:2]) #* prior_probabilities
cov_virginica <- cov(iris_virginica[,1:2]) #* prior_probabilities

pooled_cov <-(cov_setosa +  cov_versicolor + cov_virginica ) / 3


mean_setosa = c(mean(iris_setosa$Sepal.Length), mean(iris_setosa$Sepal.Width)) #1x2
w0_setosa = (-1/2)*t(mean_setosa)%*%solve(pooled_cov)%*%mean_setosa + log(1/3)
w1_setosa <- solve(pooled_cov)%*%mean_setosa

mean_versi = c(mean(iris_versicolor$Sepal.Length), mean(iris_versicolor$Sepal.Width)) #1x2
w0_versi = (-1/2)*t(mean_versi)%*%solve(pooled_cov)%*%mean_versi + log(1/3)
w1_versi <- solve(pooled_cov)%*%mean_versi

mean_virginica = c(mean(iris_virginica$Sepal.Length), mean(iris_virginica$Sepal.Width)) #1x2
w0_virginica = (-1/2)*t(mean_virginica)%*%solve(pooled_cov)%*%mean_virginica + log(1/3)
w1_virginica <- solve(pooled_cov)%*%mean_virginica

w0_setosa_versi <- w0_setosa - w0_versi
w1_setosa_versi <- w1_setosa - w1_versi

w0_setosa_vergin <- w0_setosa - w0_virginica
w1_setosa_vergin <- w1_setosa - w1_virginica

w0_versi_vergin <- w0_versi - w0_virginica
w1_versi_vergin <- w1_versi - w1_virginica

##########      Q3      ##########      
Y <- data.frame(sepal.length=numeric(), sepal.width=numeric(),Species=character(), stringsAsFactors=TRUE)

for(i in 1:nrow(iris)) {
  x<-matrix(as.numeric(iris[i,1:2]), nrow=2)
  #x <- as.matrix(x)
  if (t(w1_setosa) %*% x + w0_setosa >= t(w1_versi) %*% x + w0_versi && t(w1_setosa) %*% x + w0_setosa >= t(w1_virginica) %*% x + w0_virginica) {
    x<- data.frame(sepal.length=iris[i,1], sepal.width=iris[i,2], Species='setosa', stringsAsFactors=TRUE)
    Y <- rbind(Y,x)
  }else if (t(w1_versi) %*% x + w0_versi >= t(w1_setosa) %*% x + w0_setosa && t(w1_versi) %*% x + w0_versi >= t(w1_virginica) %*% x + w0_virginica) {
    x<- data.frame(sepal.length=iris[i,1], sepal.width=iris[i,2], Species='versicolor', stringsAsFactors=TRUE)
    Y <- rbind(Y,x)
    
  } else {
    x<- data.frame(sepal.length=iris[i,1], sepal.width=iris[i,2], Species='virginica', stringsAsFactors=TRUE)
    Y <- rbind(Y,x)
  }
}

plot(Y$sepal.length, Y$sepal.width, col = Y$Species)
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species)

missclass_setosa <- 0 

for (i in 1:50 ) {
  if (Y[i,3] != iris[i,5]) {
    missclass_setosa = missclass_setosa +1
  }
}
misclassrate_setosa <- missclass_setosa/ 50 

missclass_versi <- 0
for (i in 51:100 ) {
  if (Y[i,3] != iris[i,5]) {
    missclass_versi = missclass_versi +1
  }
}
missclassrate_versi <- missclass_versi /50

missclass_virginica <- 0
for (i in 101:150 ) {
  if (Y[i,3] != iris[i,5]) {
    missclass_virginica = missclass_virginica +1
  }
}
missclassrate_virginica <- missclass_virginica /50
misclassrate_setosa
missclassrate_versi
missclassrate_virginica
total.mc.rate <- (missclass_setosa + missclass_versi + missclass_virginica) / 150
total.mc.rate

lda.fit <- lda(iris[,1:2], iris$Species)
lda.pred = predict(lda.fit)
lda.class <- lda.pred$class

lda.Missclassrate<- mean(lda.class != iris$Species)



##########      Q4      ##########      

sampled_data <- data.frame(sepal.length=numeric(), sepal.width=numeric(),Species=character(), stringsAsFactors=TRUE)
sample <- as.character(sample(unique(iris$Species), 150, replace=TRUE))
for (i in 1:150) {
  if (sample[i] == "setosa"){
    x<-rmvnorm(n= 1, mean=c(mean_setosa), sigma=pooled_cov)
    rmv <- data.frame(sepal.length = x[,1], sepal.width = x[,2], Species='setosa')
    sampled_data <- rbind(sampled_data, rmv)
  }
  if (sample[i] == "versicolor") {
    x <- rmvnorm(n= 1, mean=c(mean_versi), sigma=pooled_cov)
    rmv <- data.frame(sepal.length = x[,1], sepal.width = x[,2], Species='versicolor')
    sampled_data <- rbind(sampled_data, rmv)
  } 
  if (sample[i] == "virginica") {
    x <-rmvnorm(n= 1, mean=c(mean_virginica), sigma=pooled_cov)
    rmv <- data.frame(sepal.length = x[,1], sepal.width = x[,2], Species='virginica')
    sampled_data <- rbind(sampled_data, rmv)
  } 
 

}

plot(sampled_data$sepal.length, sampled_data$sepal.width, col= sampled_data$Species, main='sampled data')
plot(iris$Sepal.Length, iris$Sepal.Width, col=lda.class, main='predicted data')
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species, main='Iris true data')










##########      Q5      ##########      

model.logistic <- multinom(Species~. , iris[,-3:-4])

logistic.pred <- predict(model.logistic)

plot(iris$Sepal.Length, iris$Sepal.Width, col=logistic.pred, main='Logistic Regression')
plot(iris$Sepal.Length, iris$Sepal.Width, col=lda.class, main='predicted data')


logistic.Missclassrate<- mean(logistic.pred != iris$Species)























