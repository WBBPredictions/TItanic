##Travis Playground
library(readr)
library(e1071)
library(class)
library(beepr)
train <- read_csv("train.csv")
#data prep
set.seed(69)
dat <-Cross_val_maker(train[,-c(1, 4, 9, 11)], .1)
Test <- (dat$Test)
Test.Com <- Test[complete.cases(Test),]
Test.Com[,3] <- factor(Test.Com[,3], labels = c(1, 0))
Test.Com[,3] <- as.numeric(Test.Com[,3])
Test.Com[,8] <- factor(Test.Com[,8], labels = c(0, 1, 2))
Test.Com[,8] <- as.numeric(Test.Com[,8])

Train <- (dat$Train)
Train.Com <- Train[complete.cases(Train),]
Train.Com[,3] <- factor(Train.Com[,3], labels = c(1, 0))
Train.Com[,3] <- as.numeric(Train.Com[,3])
Train.Com[,8] <- factor(Train.Com[,8], labels = c(0, 1, 2))
Train.Com[,8] <- as.numeric(Train.Com[,8])

fit <- glm(Survived~ ., data = Train.Com, family = binomial("logit"))
fitKnn <- knn(train = Train.Com[,-1], test = Test.Com[,-1], cl = factor(Train.Com[,1]), k = 4)
fitSVM <- svm(x = Train.Com[,-1], y = Train.Com[,1], type = "C-classification", kernel = "linear")
pred <- predict.glm(fit, Test.Com[,-1], type = "response")
SVMpred <- predict(fitSVM, Test.Com[,-1])
v <- which(as.numeric(pred) > .85 | as.numeric(pred) < .05)

finalpred <- {}
for(i in 1:nrow(Test.Com))
{
  if(pred[i] > .85 | pred[i] < .15)
  {
    finalpred[i] <- round(pred[i])
  }
  else
  {
    #finalpred[i] <- as.numeric(fitKnn) - 1
    finalpred[i] <- as.numeric(SVMpred[i]) - 1
  }
}
#finalpred
#Combo ()
table(finalpred, Test.Com[,1])
sum(diag(table(finalpred, Test.Com[,1])))/sum(table(finalpred, Test.Com[,1]))

#KNN
table(fitKnn, Test.Com[,1])
sum(diag(table(fitKnn, Test.Com[,1])))/sum(table(fitKnn, Test.Com[,1]))

#SVM
table(SVMpred, Test.Com[,1])
sum(diag(table(SVMpred, Test.Com[,1])))/sum(table(SVMpred, Test.Com[,1]))

#Logistic
table(round(pred), Test.Com[,1])
sum(diag(table(round(pred), Test.Com[,1])))/sum(table(round(pred), Test.Com[,1]))


#Where did it go wrong?
Wrong <- cbind(round(pred), Test.Com[,1])
index2 <- which(Wrong[,1] != Wrong[,2])
cbind(pred[index2], Test.Com[index2, 1])
