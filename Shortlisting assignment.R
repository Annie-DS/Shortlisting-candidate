
#By using Function of excel, extracted the value of Column Machine learning,Deep learning,NLP,SDA,AWS,SQL,Excel


#LOADING PACKAGES
library(readxl)
library(ROSE)
library(caret)
library(dplyr)          
library(pROC)  
library(e1071)        
library(ROCR)
library(nnet)

#Importing Dataset
DS<-readxl::read_xlsx (path = 'C:/Users/Abhishek/Downloads/Data_Science_2020_v2.xlsx')

#Deleting redundant Variables
DS$'Current City' <-NULL
DS$Institute <-NULL
DS$Performance_PG<-NULL
DS$Performance_UG<-NULL
DS$Performance_12<-NULL
DS$Performance_10<-NULL
DS$Stream<-NULL
DS$`Other skills`<-NULL

#Applying shortlisting criteria
DS$`Python (out of 3)`<-ifelse(DS$`Python (out of 3)`==1,3,ifelse(DS$`Python (out of 3)`==2,7,
ifelse(DS$`Python (out of 3)`==3,10,0)))

DS$`R Programming (out of 3)`<-ifelse(DS$`R Programming (out of 3)`==1,3,ifelse(DS$`R Programming (out of 3)`==2,7,
  ifelse(DS$`R Programming (out of 3)`==3,10,0)))

DS$`Data Science (out of 3)`<- ifelse(DS$`Data Science (out of 3)`==1,3,ifelse(DS$`Data Science (out of 3)`==2,7,
   ifelse(DS$`Data Science (out of 3)`==3,10,0)))

DS$Degree[is.na(DS$Degree)]<-0
DS$Degree<-factor(ifelse(DS$Degree=='Bachelor of Engineering (B.E)',1,ifelse(DS$Degree=='Bachelor of Technology (B.Tech)'
      ,1,ifelse(DS$Degree=='Master of Science (M.Sc)',2,ifelse(DS$Degree=='Master of Technology (M.Tech)',2,0)))))

DS$Qualification<- ifelse(DS$Degree==1 & DS$`Current Year Of Graduation`=='2020',10,ifelse(DS$Degree==1 & 
        DS$`Current Year Of Graduation`=='2019',8,ifelse(DS$Degree==1 & DS$`Current Year Of Graduation`<='2018',5,
        ifelse(DS$Degree==2 & DS$`Current Year Of Graduation`=='2020',7,
        ifelse(DS$Degree==2 & DS$`Current Year Of Graduation`<= '2019',3,0)))))

DS_2020<-c('R Programming (out of 3)','Python (out of 3)','Data Science (out of 3)','Machine learning','Deep Learning','NLP','SDA','AWS','SQL','Excel',
           'Qualification')
 
#Applicant Total scores
DS$total_scores<-apply(DS[,DS_2020],1,sum)  

#Dividing data into training and testing
set.seed(35)
train<-sample(1:nrow(DS),size = floor(0.70*nrow(DS)))
DS_train<-DS[train,]
DS_test<-DS[-train,]



#Finding the value of shortlisted for training purpose
DS_train$Shortlisted<- factor(ifelse(DS_train$total_scores >=40 ,'Yes','No'))



#Deleting columns no longer in use  
DS_train$SDA<-NULL
DS_train$`Current Year Of Graduation`<-NULL
DS_train$Degree<-NULL
DS_train$Application_ID<-NULL

DS_test$`Current Year Of Graduation`<-NULL
DS_test$Degree<-NULL

write.csv(DS_train,file='C:\\Users\\Abhishek\\Desktop\\Shortlisting/Training.csv')
write.csv(DS_test,file='C:\\Users\\Abhishek\\Desktop\\Shortlisting/Testing.csv')

#MODEL 1-SVM
mysvm<-svm(Shortlisted~.,data = DS_train,type='C',kernel='polynomial',degree=2)
#confusion Matrix
pred = predict(mysvm,DS_train[,-12])
table(pred, DS_train$Shortlisted)
mean(pred==DS_train$Shortlisted)

#MODEL 2-ANN
table(DS_train$Shortlisted)
myann1 <- multinom(Shortlisted~., data=DS_train, maxit=500, trace=T)
predan1 = predict(myann1,DS_train[,-12])
table(predan1, DS_train$Shortlisted)
mean(predan1==DS_train$Shortlisted)


#Predicting on testing data using MODEL 1
predShort = predict(mysvm,DS_test)
predfinal<-cbind.data.frame(DS_test$Application_ID,predShort)

write.csv(predfinal,file='C:\\Users\\Abhishek\\Desktop\\Shortlisting/validation.csv')
