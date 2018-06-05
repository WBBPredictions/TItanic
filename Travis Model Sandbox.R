##Travis Playground
library(readr)
train <- read_csv("~/Desktop/Projects/Titanic/train.csv")

dat <-Cross_val_maker(train[,-c(1, 4, 9, 11)], .1)
Test <- (dat$Test)
Train <- (dat$Train)
fit <- glm(Survived~ ., data = Train, family = binomial("logit"))

temp <- Test[which(is.na(Test$Age) == F),]
pred <- predict.glm(fit, temp[,-1], type = "response")
for(i in 1:length(pred))
{
  if(pred[i] > .5)
    pred[i] = 1
  else
    pred[i] = 0
}
pred
table(pred, temp$Survived) #73%, not terrible



