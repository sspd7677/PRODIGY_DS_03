#LOADING LIBRARIES
library(rpart)
library(tree)
library(corrgram)
library(rpart.plot)

#Loading Dataset
bank<-read.csv("D:/Prodigy Internship/bank-marketing.csv")
str(bank)
dim(bank)
head(bank)
tail(bank)
bank<-na.omit(bank)
corrgram(bank)

#Converting numeric variables into factors 
bank$deposit<-as.factor(bank$deposit)
bank$housing<-as.factor(bank$housing)
bank$marital<-as.factor(bank$marital)
bank$education<-as.factor(bank$education)
bank$loan<-as.factor(bank$loan)
bank$job<-as.factor(bank$job)
bank$default<-as.factor(bank$default)
bank$contact<-as.factor(bank$contact)
bank$poutcome<-as.factor(bank$poutcome)
	
#Dividing the data as train and test 
library(caret)
set.seed(123)
index<-createDataPartition(bank$deposit,times=1,p=.75,list=FALSE)
train<-bank[index,]
dim(train)
test<-bank[-index,]
dim(test)

#Decision tree
tree_model <- rpart(deposit ~ age+job+marital+education+balance+housing+loan+duration+default+contact+poutcome , data = train, method="class")
summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 0)
predictions <- predict(tree_model, newdata = test, type="class")
confusionMatrix(test$deposit,predictions)
rpart.plot(tree_model, type = 3, box.palette = "RdBu", branch.lty = 3)
