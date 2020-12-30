# Libraries
library(tree)
library(rpart)
library(MASS)
library(e1071)

# Q1: Import data to R
# remove "duration" and 
# divide the data into training, validation and test 
# 40/30/30
data = read.csv2("bank-full.csv", stringsAsFactors = T)
data = subset(data, select = -c(duration) )

# Training
n =  dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.4))
train = data[id,]
# Validation
id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.3))
valid = data[id2,]
# Test
id3 = setdiff(id1, id2)
test = data[id3,]

# Q2: Fit decision trees
tree_default = tree(as.factor(y)~., data=train) # Tree with default settings, no minimum size or deviance
tree_nodesize = tree(as.factor(y)~., data=train, minsize=7000) #minimum node size 7000
tree_deviance = tree(as.factor(y)~., data=train, mindev=0.0005) #low diviation, tar hänsyn till fler variabler, overfitted, 

# Q3: Optimal tree depth
train_score = rep(0,50)
valid_score = rep(0,50)
for (i in 2:50) {
  pruned_tree <- prune.tree(tree_deviance, best=i)
  prediction <- predict(pruned_tree, newdata = valid, type = "tree")
  train_score[i] <- deviance(pruned_tree) 
  valid_score[i] <- deviance(prediction)
}
#score är deviance, poängsätter modeller/trädet
misCl = function(fit, x) {
  set.seed(12345)
  Yfit = predict(fit, newdata=x, type="class")
  confMa = table(x$y, Yfit)
  return(1-sum(diag(confMa))/sum(confMa))
}
misCl_train_default = misCl(tree_default, train)
misCl_train_nodesize = misCl(tree_nodesize, train)
misCl_train_deviance = misCl(tree_deviance, train)
misCl_valid_default = misCl(tree_default, valid)
misCl_valid_nodesize = misCl(tree_nodesize, valid)
misCl_valid_deviance = misCl(tree_deviance, valid)

# Plot training and validation scores
plot(2:50, train_score[2:50], type="b", col="red", ylab="Deviance", xlab="No. of leaves",ylim=c(8000,12000))
points(2:50, valid_score[2:50], type="b", col="blue")
legend(25, 11900, legend=c("Training score", "Validation score"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
title("Training- and validation scores")

#Optimal number of leaves
opt_no_leaves = match(min(valid_score[2:50]),valid_score) 
opt_pruned_tree <- prune.tree(tree_deviance, best=opt_no_leaves)
plot(opt_pruned_tree, sub="asd")
text(opt_pruned_tree, pretty=0, cex = 0.5)
title("Pruned tree with 22 leaves")

# Estimate confusion matrix for and misClification rate for test data
set.seed(12345)
pred_optTree = predict(opt_pruned_tree, newdata=test, type="class")
confMa_opt_test = table(test$y, pred_optTree)
misCl_optTree_test = (1-sum(diag(confMa_opt_test))/sum(confMa_opt_test))

# Q4: Decision tree with loss function
set.seed(12345)
# om sannolikheten för ett no är 5 gånger mindre än sannolikheten för att klassificeras som ett ja så klass som ja
pred_treeDev_test = predict(tree_deviance, newdata=test, type="vector")
p_no = pred_treeDev_test[,1]
p_yes = pred_treeDev_test[,2]
confMa_lossTree_test = table(test$y, ifelse(p_no > p_yes*5, "no", "yes"))
misCl_lossTree_test <- 1-(sum(diag(confMa_lossTree_test))/sum(confMa_lossTree_test))

# Q5 
## FOR THE OPTIMAL TREE ##
tree_model = opt_pruned_tree
#prediction på testdatan
tree_pred = predict(tree_model, newdata=test, type="vector")
tree_TPR = c(rep(0,length(pi)))
tree_FPR = c(rep(0,length(pi)))
tree_p_yes = tree_pred[,2]
i=1
#loppar över pi
for (pi in seq(from=0.00, to=1, by=0.05)) {
#sätter 
  tree_Y = ifelse(tree_p_yes > pi, 1, 0)
  confMa = table(test$y, tree_Y)
# vår confMa bara en column, så vi lägger till noll,noll för att få en 4x4 matrix
  if (is.na(table(tree_Y)[2])) {
    if (colnames(confMa)[1] == "yes") {
      confMa = cbind(c(0,0), confMa)
    } else {
      confMa = cbind(confMa, c(0,0))
    }
  }
  #för varje pi sparar jag true positive rate
  tree_TPR[i] = confMa[2,2] / (confMa[2,1] + confMa[2,2])
  tree_FPR[i] = confMa[1,2] / (confMa[1,1] + confMa[1,2])
  i=i+1
}

## FOR THE NAIVE MODEL ##
# ny modell, prediction som tar fram sannolikheter iställer för vilken klass den tillhör
naive_model = naiveBayes(train$y~., train)
naive_pred = predict(naive_model, newdata=test, type="raw")
naive_TPR = c(rep(0,length(pi)))
naive_FPR = c(rep(0,length(pi)))
naive_p_yes = naive_pred[,2]
i=1
for (pi in seq(from=0.00, to=1, by=0.05)) {
  naive_Y = ifelse(naive_p_yes > pi, "yes", "no")
  confMa = table(test$y, naive_Y)
  if (is.na(table(naive_Y)[2])) {
    if (colnames(confMa)[1] == "yes") {
      confMa = cbind(c(0,0), confMa)
    } else {
      confMa = cbind(confMa, c(0,0))
    }
  }
  naive_TPR[i] = confMa[2,2] / (confMa[2,1] + confMa[2,2])
  naive_FPR[i] = confMa[1,2] / (confMa[1,1] + confMa[1,2])
  i=i+1
}

# Plot ROC-curves for both models
plot(tree_FPR, tree_TPR, type = "l", col="green", main="Green = tree, Blue = naive", xlab="FPR", ylab="TPR")
points(naive_FPR, naive_TPR, type = "l", col="blue")