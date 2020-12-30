library(cvTools)
library(kknn)

#1 Devide and import the data
data=read.csv("optdigits.csv")
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

# 2 Fit 30-nearest neighbor
# Make last column into factor
data$X0.26 = as.factor(data$X0.26)
train$X0.26 = as.factor(train$X0.26)
test$X0.26 = as.factor(test$X0.26)
valid$X0.26 = as.factor(valid$X0.26)

# Create model and predict for test and train data
kknn_test=kknn(X0.26~., train=train, test=test, k = 30, kernel = "rectangular")
kknn_pred_test = predict(kknn_test)

kknn_train=kknn(X0.26~., train=train, test=train, k = 30, kernel = "rectangular")
kknn_pred_train = predict(kknn_train)

#Create confusion matrices
confusion_matrix_test=table(test$X0.26 , kknn_pred_test)
confusion_matrix_train=table(train$X0.26 , kknn_pred_train)
confusion_matrix_test
confusion_matrix_train


# Number sum matrix - diagonal (answer in %)
missclass_train = sum(diag(confusion_matrix_train))/sum(confusion_matrix_train)
missclass_train
missclass_test = sum(diag(confusion_matrix_test))/sum(confusion_matrix_test)
missclass_test
#3 Find any 2 cases of digit “8” which were easiest to classify and 3 cases which were hardest
#Two cases of digit 8 that were easy to classify (found in the probability data in kknn): row number 34 and 55 
A=data.matrix(train[34, 1:64]) 
AA=matrix(A, nrow=8, ncol = 8)
B=data.matrix(train[55, 1:64]) 
BB=matrix(B, nrow=8, ncol = 8)

heatmap(t(AA), Rowv=NA, Colv=NA)
heatmap(t(BB), Rowv=NA, Colv=NA)

#Three cases of digit 8 that were difficult to classify (found in the probability data in kknn): row number 74, 69 and 89
C = data.matrix(train[81, 1:64]) 
CC = matrix(C, nrow=8, ncol=8)
E = data.matrix(train[89, 1:64]) 
EE = matrix(E, nrow=8, ncol=8)
G = data.matrix(train[94, 1:64]) 
GG = matrix(G, nrow=8, ncol=8)
heatmap(t(GG), Rowv=NA, Colv=NA)
heatmap(t(CC), Rowv=NA, Colv=NA)
heatmap(t(EE), Rowv=NA, Colv=NA)

#4 Choosing optimal K based on missclassification-error
#create a matrix for missclassvalues for validation
missclass_valid_values=c()
for (i in c(1:30)) {
kknn_val=kknn(X0.26~., train=train, test=valid, k = i, kernel = "rectangular")
kknn_pred_val = predict(kknn_val)
confusion_matrix_val=table(valid$X0.26 , kknn_pred_val)
missclass_val = 1-sum(diag(confusion_matrix_val))/sum(confusion_matrix_val)
missclass_valid_values=append(missclass_valid_values, missclass_val)
}

#create a matrix for missclassification errors for training
missclass_train_values=c()

for (i in c(1:30)) {
  kknn_train=kknn(X0.26~., train=train, test=train, k=i, kernel = "rectangular")
  kknn_pred_train = predict(kknn_train)
  confusion_matrix_train=table(train$X0.26, kknn_pred_train)
  missclass_train = 1-sum(diag(confusion_matrix_train))/sum(confusion_matrix_train)
  missclass_train_values=append(missclass_train_values, missclass_train)
}

plot((1:30), missclass_train_values,type="o", col="blue", main="Dependence of the training and validation misclassification errors on the value of K", ylab="Misclassification errors", xlab="k")
points((1:30), missclass_valid_values, col="red", pch="*")
lines((1:30), missclass_valid_values, col="red",lty=2)
legend(2,0.06,legend=c("Training","Validation"), col=c("blue","red"),pch=c("o","*"),lty=c(1,2,3), ncol=1)

#for test data for optimal k=7
kknn_test=kknn(X0.26~., train=train, test=test, k=7, kernel = "rectangular")
kknn_pred_test = predict(kknn_test)
confusion_matrix_test=table(test$X0.26, kknn_pred_test)
missclass_test = 1-sum(diag(confusion_matrix_test))/sum(confusion_matrix_test)

#5 Choosing optimal K based on empirical risk
train$X0.26 = as.numeric(train$X0.26)
valid$X0.26 = as.numeric(valid$X0.26)

loss_train = c()
for (i in c(1:30)) {
  kknn = kknn(X0.26~., train=train, test=train, k = i, kernel = "rectangular") 
  kknn_pred = predict(kknn)
  loss = mspe(train$X0.26, kknn_pred)
  logloss = log(loss + exp(-15))
  loss_train = append(loss_train, loss) }
plot((1:30),loss_train, main="Plot predicted loss for training")


loss_valid = c()
for (i in c(1:30)) {
  kknn = kknn(X0.26~., train=train, test=valid, k = i, kernel = "rectangular") 
  kknn_pred = predict(kknn)
  loss = mspe(valid$X0.26, kknn_pred)
  logloss = log(loss + exp(-15))
  loss_valid = append(loss_valid, loss) }
plot((1:30),loss_valid, main="Plot predicted loss for validation")
