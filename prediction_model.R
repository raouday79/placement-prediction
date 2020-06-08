#Reading the excel
location <- "E:\\ML\\Data Science Project\\Placement Prediction\\Dataset\\"
setwd(location)
getwd()
library(xlsx)
cse = read.xlsx(file.choose(), sheetIndex =1)
etc = read.xlsx(file.choose(), sheetIndex =1)
it = rbind(cse,etc)

table(it$Placed)
barplot(table(it$Gender,it$Placed))
barplot(table(it$Gender))

View(it)
str(it)
#Removing unwanted columns
#sr|sr|First|Last|
#only selected required columns
#data_cse = cse[-c(1)]
data_model <- subset(it,select = c(Placed,Gender,Branch,Past.Backlog,Internship,Paper.Presentation,Project,X1st.Semester....,
                                   X2nd.Semester....,X3rd.Semester....,X4th.Semester....,
                                   X5th.Semester....,X6th.Semester....,
                                   SSC....,HSC....,Dipolma....
                                   ))

View(data_model)
str(data_model)
summary(data_model)
#Data conversion
data_model$Paper.Presentation = factor(data_model$Paper.Presentation)
data_model$Project = factor(data_model$Project)
data_model$Placed = factor(data_model$Placed)

data_model$Internship = factor(data_model$Internship)
data_model$Internship  = as.numeric(data_model$Internship)

#COnverting from factor to numberic data type
marks_column <- c("X1st.Semester....","X2nd.Semester....","X3rd.Semester....",
                  "X4th.Semester....","X5th.Semester....","X6th.Semester....",
                  "SSC....","HSC....","Dipolma....")


#data_model$SSC.... = as.numeric(as.character(data_model$SSC....))
data_model[marks_column] <- sapply(data_model[marks_column],as.character)
data_model[marks_column] <- sapply(data_model[marks_column],as.numeric)

str(data_model)

#Scaling the marks cloumn
data_model[marks_column] = scale(data_model[marks_column])
#View(data_model)
write.xlsx(data_model,file = "placement_prediction.xlsx")
#Spliting the data in training and testing
library(caTools)
set.seed(222)
split = sample.split(data_model,SplitRatio = 0.8)
traning_data = subset(data_model,split==TRUE)
testing_data = subset(data_model,split==FALSE)
#View(testing_data)

str(traning_data)
str(testing_data)
summary(data_model)

#Building KNN Classification model
library("class")
library("caret")
library("gmodels")

knn_classifier <- knn(train = traning_data,test = testing_data,cl=traning_data$Placed,k = 3)
#View(traning_data$Dipolma....)


#Bulding SVM Classification Model
library(e1071)
svm_classifier = svm(formula=traning_data$Placed~.,data=traning_data,type="C-classification",kernal="radial")
y_pred = predict(svm_classifier,newdata = testing_data[-1])
cm = table(testing_data[,1],y_pred)
accuracy =sum(diag(cm)/sum(cm))
accuracy
cm
#Accuracy 0.6086957


#Building Random Forest Classification model

library(randomForest)
rf_classifier = randomForest(x=traning_data[,-1],y=traning_data[,1],data=traning_data,ntree = 5)
y_pred  = predict(rf_classifier,testing_data[,-1])
cm = table(testing_data[,1],y_pred)
cm
accuracy = sum(diag(cm)/sum(cm))
accuracy
#Accuracy 0.6956522 T=20
#Accuracy 0.77391304 T=5
















