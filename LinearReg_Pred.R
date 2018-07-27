dat<- read.csv(file.choose()) #original Titanic data
dat.1<- read.csv(file.choose()) #titanic data with age distribution imputations

#changing the variable Sex 
dat$sex1<- factor(dat$Sex, labels = c(0,1))

#add imputation coloumns for age to the original data 

#MEAN imputations
#copy of age coloumn for mean imputations
age.mean<- dat$Age
#add the age.mean col in the dataset for the mean impuation proceedure
dat$age.mean<- age.mean
#extract the available ages
age.1<- dat$age.mean[!is.na(dat$age.mean)]
mean<- mean(age.1)
mean
#mean impuations
dat$age.mean[is.na(dat$age.mean)]<- mean
round(dat$age.mean, 2)

#MEDIAN imputations
#copy of age coloumn for median imputations
age.median<- dat$Age
#add the age.mean col in the dataset for the median impuation proceedure
dat$age.median<- age.median
#extract the available ages
age.2<- dat$age.median[!is.na(dat$age.median)]
median<- median(age.2)
median
#median impuations
dat$age.median[is.na(dat$age.median)]<- median
round(dat$age.median, 2)

#AGE DISTRIBUTION Imputations
age.dis<- dat.1$Age
dat$age.dis<- age.dis
round(dat$age.dis, 2)

#Using Cross_Val_Maker
dat.cvm<- Cross_val_maker(data= dat, 0.05)
dat.cvm1<- dat.cvm$Train
dat.cvm2<- dat.cvm$Test

#Survival predictions: Linear Regression (Sex, Pclass, age.mean)
lm1<- lm(dat.cvm1$Survived~ dat.cvm1$sex1+dat.cvm1$Pclass+ dat.cvm1$age.mean)
lm1

filter1<- dat.cvm2[,c(3,13,14)] #test data
attach(dat.cvm1)
survival.pred1<-predict.lm(lm1, data =filter1)
detach(dat.cvm1)
round(survival.pred1)
ans <- {}
for(i in 1:nrow(filter1))
{
  temp <- -0.496132537*(as.numeric(filter1$sex1[i])-1) + -0.184218054*filter1$Pclass[i] - 0.004764364*filter1$age.mean[i] + 1.273115154
  ans <- c(ans, temp)
}
ans = round(ans)
#Accuracy
tab1<- table(ans, dat.cvm2[,2])
accuracy1<- sum(diag(tab1)/ sum(tab1))
accuracy1


#Survival predictions: Linear Regression (Sex, Pclass, age.median)
lm2<- lm(dat.cvm1$Survived~ dat.cvm1$sex1+dat.cvm1$Pclass+ dat.cvm1$age.median)
lm2

filter2<- dat.cvm2[,c(3,13,15)] #test data
ans2 <- {}
for(j in 1:nrow(filter2))
{
  temp2 <- -0.496403 *(as.numeric(filter2$sex1[j])-1) + -0.184731*filter2$Pclass[j] -0.004745 *filter2$age.median[j] +  1.272285 
  ans2 <- c(ans2, temp2)
}
ans2 = round(ans2)
#Accuracy
tab2<- table(ans2, dat.cvm2[,2])
accuracy2<- sum(diag(tab2)/ sum(tab2))
accuracy2


#Survival predictions: Linear Regression (Sex, Pclass, age.dis)
lm3<- lm(dat.cvm1$Survived~ dat.cvm1$sex1+dat.cvm1$Pclass+ dat.cvm1$age.dis)
lm3

filter3<- dat.cvm2[,c(3,13,16)] #test data
ans3 <- {}
for(k in 1:nrow(filter3))
{
  temp3 <- -0.504652  *(as.numeric(filter3$sex1[k])-1) + -0.172818*filter3$Pclass[k] -0.002559 *filter3$age.dis[k] +  1.186120 
  ans3 <- c(ans3, temp3)
}
ans3 = round(ans3)
#Accuracy
tab3<- table(ans3, dat.cvm2[,2])
accuracy3<- sum(diag(tab3)/ sum(tab3))
accuracy3
