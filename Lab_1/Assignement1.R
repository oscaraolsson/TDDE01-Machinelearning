# Sconfucet working directory. We have to do this in order to refer to certain files in our directory

# Import libraries
library(cvTools)
library(kknn)
library(MLmetrics)

# 1.1 Import the data into R:
data = read.csv("optdigits.csv")

# 1.2 Devide data into three groups: (code from lecture 1e, page 20)
# group 1 (50%) = training 
# group 2 (25%) = validation
# group 3 (25%) = test
n =  dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = data[id,]

id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.25))
valid = data[id2,]

id3 = setdiff(id1, id2)
test = data[id3,]
# Question: nr of valid  = 955, nr of  test = 956, will it make a difference?

#############################################################################

# 2 Fit 30-nearest neighbor
# Make last column into factor
data$X0.26 = as.factor(data$X0.26)
train$X0.26 = as.factor(train$X0.26)
test$X0.26 = as.factor(test$X0.26)

# Create model and predict
kknn_test = kknn(X0.26~., train=train, test=test, k = 30, kernel = "rectangular")
kknn_pred_test = predict(kknn_test)

kknn_train = kknn(X0.26~., train=train, test=train, k = 30, kernel = "rectangular")
kknn_pred_train = predict(kknn_train)

confusion_matrix_train = table(kknn_pred_train, train$X0.26)
confusion_matrix_test = table(kknn_pred_test, test$X0.26)

# Missclassification-rate
missclass_train = 1 - sum(diag(confusion_matrix_train))/sum(confusion_matrix_train)
missclass_test = 1 - sum(diag(confusion_matrix_test))/sum(confusion_matrix_test)

# Two cases of digit 8 that were easy to classify 
A = data.matrix(train[34, 1:64]) # 100%
AA = matrix(A, nrow=8, ncol=8)
B = data.matrix(train[55, 1:64]) #97%
BB = matrix(B, nrow=8, ncol=8)

# Three cases of digit 8 that were hard to classify
C = data.matrix(train[74, 1:64]) #66%
CC = matrix(C, nrow=8, ncol=8)
D = data.matrix(train[69,1:64]) #73%
DD = matrix(D, nrow=8, ncol=8)
E = data.matrix(train[89, 1:64]) #40%
EE = matrix(E, nrow=8, ncol=8)

heatmap(t(AA), Rowv=NA, Colv=NA)
heatmap(t(BB), Rowv=NA, Colv=NA)
heatmap(t(CC), Rowv=NA, Colv=NA)
heatmap(t(DD), Rowv=NA, Colv=NA)
heatmap(t(EE), Rowv=NA, Colv=NA)

# Nr 4. kknn for validation 

kknn_val = kknn(X0.26~., train=train, test=valid, k = 30, kernel = "rectangular") 
kknn_pred_val = predict(kknn_val)
confusion_matrix_val = table(kknn_pred_train, train$X0.26)
missclass_val = 1 - sum(diag(confusion_matrix_val))/sum(confusion_matrix_val)

# Choosing optimal K based on missclassification-error
mc_train = c()
for (i in c(1:30)) {
  kknn = kknn(X0.26~., train=train, test=train, k = i, kernel = "rectangular") 
  kknn_pred = predict(kknn)
  confusion_matrix = table(kknn_pred, train$X0.26)
  missclass = 1 - sum(diag(confusion_matrix))/sum(confusion_matrix)
  mc_train = append(mc_train, missclass) }
plot((1:30),mc_train)

mc_valid = c()
for (i in c(1:30)) {
  kknn = kknn(X0.26~., train=train, test=valid, k = i, kernel = "rectangular") 
  kknn_pred = predict(kknn)
  confusion_matrix = table(kknn_pred, valid$X0.26)
  missclass = 1 - sum(diag(confusion_matrix))/sum(confusion_matrix)
  mc_valid = append(mc_valid, missclass) }
plot((1:30),mc_valid)

# Choosing optimal K based on emperical risk
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



# Same as above but choose K emperical risk 
# Expected loss, uniform loss.