# Import libraries
library(cvTools)
library(MLmetrics)
library(kknn)
library(DMwR)

# Import the data into R:
data = read.csv("parkinsons.csv")

# Scale the data
data = scale(data)

# Divide data into training- and testdata (60/40)
n =  dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.6))
train = data[id,]
test = data[-id,]

# Q1: Baysian model: y ~ N(y|w0 + Xw, sigma^2 * I)
#                    w ~ N(0, sigma^2/lambda * I)

# Make training- and test-sets
Y = train[,5]
Y_test = test[,5]
X = train[,7:22]
N = length(Y)
I = as.matrix(diag(dim(t(X)%*%X)[1]))

# Loglikelihood
loglikelihood = function(sigma, W) {
  return(-N/2*log(2*pi*sigma**2) - ((t(Y-X%*%W) %*% (Y-X%*%W))/(2*sigma**2)))
}

# Ridge function
ridge = function(opt, lambda) {
  W = opt[-1]
  sigma = opt[1]
  return(lambda*sum(W**2) - loglikelihood(sigma, W))
  }

# RidgeOpt
ridge_opt = function(lambda) {
  optim(rep(1, 17), fn = ridge, lambda = lambda, method = "BFGS")
  }

# DF-function
df = function(lambda, a) {
  X = a[,7:22]
  df_matrix = X %*% solve(t(X) %*% X + lambda*I) %*% t(X)
  return(sum(diag(df_matrix)))
}

ridge_1 = ridge_opt(1)
ridge_100 = ridge_opt(100)
ridge_1000 = ridge_opt(1000)

# Predict motor_UPDRS
train_pred1 =  X %*% ridge_1$par[-1]
test_pred1 = test[,7:22] %*% ridge_1$par[-1]

train_pred100 =  X %*% ridge_100$par[-1]
test_pred100 = test[,7:22] %*% ridge_100$par[-1]

train_pred1000 =  X %*% ridge_1000$par[-1]
test_pred1000 = test[,7:22] %*% ridge_1000$par[-1]

# (1/n) * Σ(Y – train_pred1)**2
mse_train1 = MSE(train_pred1, Y)                 # 0.873277
mse_train100 = MSE(train_pred100, Y)             # 0.879063     
mse_train1000 = MSE(train_pred1000, Y)           # 0.9156267     
mse_test1 = MSE(test_pred1, Y_test)              # 0.9290357   
mse_test100 = MSE(test_pred100, Y_test)          # 0.9263215       
mse_test1000 = MSE(test_pred1000, Y_test)        # 0.9479166        

# AKAIKE INFORMATION CRITERION
AIC1 = 2*df(1, test) - 2*loglikelihood(sigma=ridge_1$par[1], ridge_1$par[-1])
AIC100 = 2*df(100, test) - 2*loglikelihood(sigma=ridge_100$par[1], ridge_100$par[-1])
AIC1000 = 2*df(1000, test) - 2*loglikelihood(sigma=ridge_1000$par[1], ridge_1000$par[-1])


loglike = loglikelihood(sigma, )
