library(datasets)
library(ggplot2)

########################## QUESTION 1 ############################

# Reading and scaling the data except ViolentCrimesPerPop
set.seed(12345)
data = read.csv("communities.csv")
set.seed(12345)
ViolentCrimesPerPop = as.data.frame(data$ViolentCrimesPerPop)
data = scale(data[,-101]) 

#Implementing PCA using eigen, PC's are eigenvectors sample covariance
cov_matrix = (1/nrow(data))*t(data)%*%data 
eigen_ = eigen(cov_matrix)

# Finding the % of the eigenvectors, where you can see how much 
# of the variance is explained by each PC
eigen_value_procent = eigen_$values/sum(eigen_$values)  

# Check when cumsum gets to 0,95. After 35 features the cumulative
# sum is 0.95
cumsum_ = cumsum(eigen_value_procent)


########################## QUESTION 2 ############################

#Repeat PCA analysis by using princomp()
set.seed(12345)
princomp_ = princomp(data)

# Printing the contribution of each feature to PC1
PC1 = princomp_$loadings[,1] 

# Sorting these contributions (distance from PC1=0) to see which
# ones are the five that contribute the most
sort(abs(PC1)) 

# Score plot of PC1
plot(PC1, main = "Feature contribution to first principle component")
abline(0,0, col='red')

# Creating a dataframe with 3 columns: PC1 scores, PC2 scores and
# ViolentCrimesPerPop
dataFrame_1 = as.data.frame(princomp_$scores[,1:2])
dataFrame_1 = cbind(dataFrame_1, ViolentCrimesPerPop)

#Plotting the scores of PC1 and PC2  
p2 = ggplot(dataFrame_1, aes(Comp.1, Comp.2, color=dataFrame_1[,3])) + 
  geom_point() + 
  ggtitle("PC Scores") +
  ylab("PC2") + 
  xlab("PC1") +
  scale_color_gradient(name = "ViolentCrimesPerPop")
p2


########################## QUESTION 3 ############################

#creating a dataframe with PC1 and ViolentCrimesPerPop
dataFrame_2 = as.data.frame(princomp_$scores[,1])
dataFrame_2 = cbind(dataFrame_2, ViolentCrimesPerPop)

# Assuming a polynomial regression model in which 
# ViolentCrimesPerPop is target and PC1 is the feature
linear_model = lm(dataFrame_2[,2]~poly(dataFrame_2[,1], 2), data = dataFrame_2)

#Scatterplot of the target versus the feature and the predicted values
pred = predict(linear_model, dataFrame_2)
plot(dataFrame_2[,1], dataFrame_2[,2], xlab="PC1", ylab="ViolentCrimesPerPop")
points(dataFrame_2[,1], pred, col='red')


########################## QUESTION 4 ############################

y=dataFrame_2[,2]
x=dataFrame_2[,1]

# Generates new data
rng=function(data, mle) {
  data1=data.frame(PC1_scores = data[,1], ViolentCrimesPerPop = data[,2])
  n=length(data[,1])
  data1$ViolentCrimesPerPop=rnorm(n,predict(mle, newdata=data1),sd(mle$residuals))
  return(data1)
}

# Function for confidence bands, that depend on the generated data
# and should return the estimator
f1=function(data1){
  res=lm(data1[,2]~poly(data1[,1], 2), data = data1)  
  pred_ = predict(res, newdata=dataFrame_2) 
  return(pred_)
}

f2=function(data1){
  res2=lm(data1[,2]~poly(data1[,1], 2), data = data1) 
  pred_2=predict(res2,newdata=dataFrame_2)
  n=length(dataFrame_2[,1])
  predictedP=rnorm(n,pred_2, sd(linear_model$residuals))
  return(predictedP)
}

#Computing 95\% confidence bands using parametric bootrap
boot_ = boot(dataFrame_2, statistic=f1, R=1000, mle=linear_model, ran.gen=rng, sim="parametric") 
e = envelope(boot_) 

#Computing 95\% prediction bands using parametric bootrap
boot_2 = boot(dataFrame_2, statistic=f2, R=1000, mle=linear_model, ran.gen=rng, sim="parametric") 
e_ = envelope(boot_2)


p3 = ggplot(dataFrame_2, aes(x, y)) + geom_point() + 
  geom_ribbon(aes(ymin=e_$point[2,], ymax=e_$point[1,]),color='orange', alpha = 0.4) +
  geom_ribbon(aes(ymin=e$point[2,], ymax=e$point[1,]),color='blue', fill='gray') +
  geom_point(aes(x, pred), color='red') +
  ggtitle("Confidence and Prediction Bands") +
  xlab("PC1 Scores") + 
  ylab("ViolentCrimesPerPop")