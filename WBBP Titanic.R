library(leaps)
library(class)
titanic = read.csv(file.choose(),header = T)
testt=read.csv(file.choose(),header=T)


####remove na in test
testtage1=na.omit(testt$Age)
testt2=testt
test_removed=attributes(testtage1)$na.action
test_removed=as.vector(test_removed)
testt2=testt[-test_removed,]
testt3=testt[test_removed,]

test4=testt[complete.cases(testt),]

###remove na in train
titanictrain=na.omit(titanic)
titanictrain1=attributes(titanictrain)$na.action
titanictrain1=as.vector(titanictrain1)
titanic1=titanic[-titanictrain1,]

train4=titanic[complete.cases(titanic),]

colnames(titanic)
attach(titanic)








#####full model
titanic_fullmodel=glm(Survived~Age+as.factor(Sex)+SibSp+Parch+Fare+
               as.factor(Embarked),data=titanic,family=binomial("logit"))

###reduced from bremers
titanic_red1=glm(Survived~Age+Pclass+as.factor(Sex),data=titanic,
                 family = binomial("logit"))

###reduced minus age
titanic_red2=glm(Survived~Pclass+as.factor(Sex)+SibSp+Parch+Fare+
                   as.factor(Embarked),data=titanic,
                 family=binomial("logit"))
bw2=step(titanic_red2)

####bremer minus age
titanic.lm2=glm(Survived ~ Fare+ as.factor(Sex) + Pclass, 
               family = binomial("logit"), data = titanic)

test_survived2=predict(titanic.lm2,testt3, type="response")

round(test_survived2)

######## bremer model
titanic.lm=glm(Survived ~ Age + as.factor(Sex) + Fare + Pclass, 
               family = binomial("logit"), data = titanic)
bw1=step(titanic.lm)

test_survived=predict(titanic_red1,testt2,type="response")


titanic_red3=glm(Survived.Com~Age+Pclass+as.factor(Sex),data=Train.Com,
                 family = binomial("logit"))
test_survived3=predict(titanic_red3,Test.Com,type="response")



round(test_survived)

total_tests=c(test_survived,test_survived2)
total_tests=round(total_tests)
test_removed2=c(test_removed,rep(0,332))
output=rep(0,length(testt))
output2=rep(0,length(testt))

j=1
k=1
for(i in 1:nrow(testt)){
  if(i == test_removed2[j]){
    output[i]=test_survived2[j]
      j=j+1
  }else{
    output[i]=test_survived[k]
    k=k+1
  }
}

output

survived4=train4$Survived
train5=train4[,-2]
train6=train5[,-8]

test5=test4[,-8]

Test.Com <- testt[complete.cases(testt),]
### sex
Test.Com[,4] <- factor(Test.Com[,4], labels = c(1, 0))
###embarked
Test.Com[,11] <- factor(Test.Com[,11], labels = c(0, 1, 2))
Test.Com=Test.Com[,-8]
Test.Com=Test.Com[,-3]
Test.Com=Test.Com[,-8]



Train.Com <- titanic[complete.cases(titanic),]
Survived.Com=Train.Com[,2]
Train.Com=Train.Com[,-2]
Train.Com[,4] <- factor(Train.Com[,4], labels = c(1, 0))
Train.Com[,11] <- factor(Train.Com[,11], labels = c(0, 1, 2,3))
Train.Com=Train.Com[,-8]
Train.Com=Train.Com[,-3]
Train.Com=Train.Com[,-8]
k=knn(Train.Com,Test.Com,Survived.Com)
output2=rep(0,nrow(Test.Com))
j=1
for(i in 1:length(output)){
  if(output[i]>=.85||output[i]<=.15){
    output2[i]=round(test_survived3[i])
  }else{
    output2[i]=k[j]
    j=j+1
  }
}

conpred=which(output>=.85|output<=.15)
nconpred=which(output<.85&output>.15)

mean(output[nconpred])




##########EM Alg#######
testass5=cbind(testt,output)
ass5(as.data.frame(cbind(output,testt$Fare[-153,])),2)
ass5(as.data.frame(cbind(test_survived,testt2$Age)),2)
###################



submission=cbind(testt$PassengerId,round(output))
colnames(submission)=c("PassengerId","Survived")

write.csv(submission,file="titanic_submission.csv",row.names=F)
#####find a good model



####predict Age NA's using other predicters
#then use the full age to predict survived


x=which(is.na(titanic$Age))
titanic_age_NA=titanic[x,]
titanic_age_full=titanic[-x,]


age.lm=lm(Age~Pclass+Survived+SibSp,
          data=titanic_age_full)
summary(age.lm)
age_predict=predict(age.lm,titanic_age_NA,type="response")
table(round(age_predict))
hist((round(titanic$Age)))
hist(round(age_predict))
hist((age_predict[age_predict>0]))
length(age_predict)
