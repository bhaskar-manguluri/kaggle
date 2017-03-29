#set wd
setwd("~/mywork/kaggle/Titanic/")
#load libraries
library(corrplot)
library(dplyr)
library(ggplot2)
library(boot)

train = read.csv(file = "train.csv")
test = read.csv(file="test.csv")
sampleSubmission = read.csv(file="gender_submission.csv")
head(train)
names(train)
# pclass for passenger class 1,2,3
# sibsp sibling or spouse
# parch parent or children
# embarked = port of embarkation : cherbourg, queenstown, southampton
str(train)
str(test)

sapply(test,function(x){sum(is.na(x))})
sapply(test,function(x){length(levels(as.factor(x)))})
levels(as.factor(test$SibSp));levels(as.factor(train$SibSp))
levels(as.factor(test$Parch));levels(as.factor(train$Parch))
table(as.factor(test$Parch))
table(as.factor(train$Parch))
# change parch 9 in test set too highest Parch(6) in train
test[test$Parch == 9,"Parch"] = 6
levels(as.factor(test$Parch));levels(as.factor(train$Parch))
test$Pclass = as.factor(test$Pclass)

test$SibSp = as.factor(test$SibSp)
test$Parch = as.factor(test$Parch)
test[is.na(test$Fare),]
 train %>% filter(Pclass==3,Embarked=="S") %>% summarise(meanFare = mean(Fare,na.rm=T))
 test %>% filter(Pclass==3,Embarked=="S") %>% summarise(meanFare = mean(Fare,na.rm=T))
 # impute mean value for missing fare value in testset
 test[is.na(test$Fare),"Fare"] = 13.91303




##plots
barplot(table(train$Pclass))

barplot(table(train$Survived))
# no severe class imbalance

barplot(table(train$Survived,train$Pclass),col=train$Survived)
#there seems to be a relation between Pclass and survival

barplot(table(train$Survived,train$Sex))
#more females survived, relation between females and survived
barplot(table(train$Survived,train$SibSp))
# difference in suvived proportions in sibsp

plot(train$Age,train$Survived,col=c("red","blue"))
proportion = function(x){
        sum(x==1)/length(x)
}
t = train %>% group_by(Age) %>% summarise(prop = proportion(Survived))
t
head(t,20)

boxplot(train$Age~train$Survived)


summary(train$Parch)
train$Parch = as.factor(train$Parch)
barplot(table(train$Parch))

#model
tanic = glm(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked,family = "binomial",data = train)


#submission
submission$Survived = yhat
write.csv(x = submission,file="crudeSubmission.csv",row.names = F)