
setwd("E:/")
data<-read_excel("Churn.xls",sheet = 1)
head(data)
class(data)
data<-as.data.frame(data)
head(data)
str(data)

#to find na's 
sum(is.na(data))

#to order columns
library(tibble)
colnames(data)

my_data<-data[,c(19,20,21,1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,8)]
head(my_data)

str(my_data)

my_data$Phone<-gsub("-", "", paste(my_data$Phone))

table(my_data$State)

my_data$Phone<-as.numeric(my_data$Phone)
my_data$State<-as.factor(my_data$State)
my_data$Churn<-as.factor(my_data$Churn)

ind <- sample(2, nrow(my_data), replace = TRUE, prob = c(0.8, 0.2))
train <- my_data[ind==1,]
test <- my_data[ind==2,]
head(train)

#cross validation

library(caret) 

control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3)


library(nnet)
seed <-7
metric <- 'Accuracy'
set.seed(seed)
my_modle<- train(Churn~., 
                 data = train,
                 method = 'multinom',
                 metric = metric,
                 trControl = control,
                 maxit=100)

#predictions
p1<-predict(my_modle ,train)
head(p1)
#accuracy
tab1<-table(p1,train$Churn)
accuracy1<-sum(diag(tab1))/sum(tab1)

table(train$Churn)

train$p1<-p1
str


#prediction for test
p2<-predict(my_modle,test)
head(p2)

#accuracy
tab2<-table(p2,test$Churn)
accuracy2<-sum(diag(tab2))/sum(tab2)

#model performance evaluatiion
library(ROCR)

pred<-predict(my_modle ,train,type='prob')
head(pred$`1`)
head(train$Churn)

hist(pred$`0`)
hist(pred$`1`)

train$pred<-pred$`1`

pred1<-prediction(pred$`1`,train$Churn)
eval<-performance(pred1,"acc")
plot(eval)
abline(h=0.87,v=0.38)



#identify bestvalues

max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
cut<-slot(eval,"x.values")[[1]][max]

#ROC (reciver operating characteristic) curve
pred1<-prediction(pred$`1`,train$Churn)
roc<-performance(pred1,"tpr","fpr")
plot(roc)
abline(a=0,b=1)

#can add some more things
plot(roc,
     colorize=T,
     main="ROC curve",
     ylab="sensitivity",
     xlab="1-specficity")
abline(a=0,b=1)

#AUC (area under curve)
auc<-performance(pred1,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
legend(.8,.2,auc,title = "AUC",cex = 1.2)

#model performance evaluatiion for test
library(ROCR)

pred<-predict(my_modle ,test,type='prob')
head(pred$`1`)
head(test$Churn)

hist(pred$`0`)
hist(pred$`1`)

pred1<-prediction(pred$`1`,test$Churn)
eval<-performance(pred1,"acc")
plot(eval)
abline(h=0.87,v=0.38)

#identify bestvalues

max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
cut<-slot(eval,"x.values")[[1]][max]

#ROC (reciver operating characteristic) curve
pred1<-prediction(pred$`1`,test$Churn)
roc<-performance(pred1,"tpr","fpr")  # tpr-true positives and fpr-false positives
plot(roc)
abline(a=0,b=1)

#can add some more things
plot(roc,
     colorize=T,
     main="ROC curve",
     ylab="sensitivity",
     xlab="1-specficity")
abline(a=0,b=1)

#AUC (area under curve)
auc<-performance(pred1,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
legend(.8,.2,auc,title = "AUC",cex = 1.2)


library(Rserve);Rserve()
