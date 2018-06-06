##Travis Playground
library(readr)
library(class)
train <- read_csv("~/Desktop/Projects/Titanic/train.csv")
set.seed(69)
dat <-Cross_val_maker(train[,-c(1, 4, 9, 11)], .1)
Test <- (dat$Test)
Test.Com <- Test[complete.cases(Test),]
Test.Com[,3] <- factor(Test.Com[,3], labels = c(1, 0))
Test.Com[,8] <- factor(Test.Com[,8], labels = c(0, 1, 2))
Train <- (dat$Train)
Train.Com <- Train[complete.cases(Train),]
Train.Com[,3] <- factor(Train.Com[,3], labels = c(1, 0))
Train.Com[,8] <- factor(Train.Com[,8], labels = c(0, 1, 2))
Train.Com[662, 8]


fit <- glm(Survived~ ., data = Train, family = binomial("logit"))
fitKnn <- knn(train = Train.Com[,-1], test = Test.Com[,-1], cl = factor(Train.Com[,1]))
pred <- predict.glm(fit, temp[,-1], type = "response")
v <- which(as.numeric(pred) > .85 | as.numeric(pred) < .05)

finalpred <- {}
for(i in 1:nrow(Test.Com))
{
  if(pred[i] > .85 | pred[i] < .15)
  {
    finalpred <- c(finalpred, round(pred[i]))
  }
  else
  {
    finalpred <- c(finalpred, fitKnn)
  }
}
finalpred


max(fitKnn)




