library(glmnet)

data = read.csv("tecator.csv")
n =  dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
test = data[id,]
train = data[-id,]

########        Q1        ########        

model1 = lm((Fat)~., data=train[,2:102])
lm_yhat_test <- predict(model1, test[,2:101])
lm_yhat_train <- predict(model1, train[,2:101])

train_err <- mean((train$Fat - lm_yhat_train)^2)
test_err <- mean((test$Fat - lm_yhat_test)^2)


summary(model1)
plot(model1)
plot(pred, test$Fat)

#1 Y = β0 + β1X1 + β2X2 + · · · + βpXp + ε = β^T*X + ε, = probabalistic function. 

pred_train <- predict(model1, data = train)
pred_test <- predict(model1, data = test)
pred_train
pred_test

########        Q2        ########        

#  Objective function: w_hat(lasso) =argmin (sum([i=1..n](y(i) −w0 −w(1)x(1,j) −...−w(p)x(p,j) )^2 +λsum[j=1..p](abs wj))


########        Q3        ########        

covariates=(train[,2:101])
response=(train[,102])
model_lasso=glmnet(as.matrix(covariates), response, alpha=1,family="gaussian")
plot(model_lasso, xvar="lambda", label=TRUE, main="LASSO Regression\n")

########        Q4        ########        

plot(model_lasso[['lambda']], model_lasso[['df']])

########        Q5        ########        

model_ridge=glmnet(as.matrix(covariates), response, alpha=0,family="gaussian")
plot(model_ridge, xvar="lambda", label=TRUE, main="RIDGE Regression\n")


########        Q6        ########        

cv_lasso_model=cv.glmnet(as.matrix(covariates), response, alpha=1,family="gaussian",type.measure = "class")
plot(cv_lasso_model)
coef(cv_lasso_model, s="lambda.min")
print(log(cv_lasso_model$lambda.min)) 


yhat=predict(cv_lasso_model, newx=as.matrix(test[,2:101]), s = "lambda.min")
plot(yhat, test$Fat)
test_err <- mean((test$Fat - yhat)^2)
test_err

########        Q7        ########   


sd <- sqrt(sum(test$Fat - yhat)^2/107)
rnorm_test <- rnorm(107,yhat, sd)
plot(rnorm_test, test$Fat)
