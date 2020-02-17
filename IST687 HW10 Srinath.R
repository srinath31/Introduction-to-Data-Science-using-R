#Course Number: IST687
#Srinath Ramachandran
#Homework#10
#Date the assignment is due: 11/15/2018
#Date in which assignment is submitted: 11/10/2018

#Installing package kernlab
install.packages("kernlab")
library(kernlab)


#Part A: Load and condition the data 
#1.	The data is available on blackboard (hotelSurveyBarriot), as a JSON file.
#Setting up working directory
setwd("F:\\Srinath_Syracuse")

#Extracting the json data and converting it to a dataframe
library("RJSONIO")
mypath<-"F:\\Srinath_Syracuse\\hotelSurveyBarriot.json"
mydata2<-fromJSON(mypath,simplify=TRUE,nullValue=NA)
View(mydata2)

hotelSurvey1 <- data.frame(mydata2)
View(hotelSurvey1)


#Part B: Create a happy customer variable
#2.	To focus on predicting happy customers, we need to generate a new column (where overallCustSat is 8 or higher).
hotelSurvey1$happyCustomer<-hotelSurvey1$overallCustSat>=8
View(hotelSurvey1)

#Part C: Create training and test data sets
#3. Creation of Train and Test Data
#Creating Train Data
cutPoint2_3 <- floor(2 * dim(hotelSurvey1)[1]/3)
cutPoint2_3
randIndex<-sample(1:dim(hotelSurvey1)[1])
trainData <- hotelSurvey1[randIndex[1:cutPoint2_3],]

#Creating Test Data
testData <-hotelSurvey1[randIndex[(cutPoint2_3+1):dim(hotelSurvey1)[1]],]
View(testData)

#4.	Use the dim( ) function to demonstrate that the resulting training data set and test data set contain the appropriate number of cases.
dim(trainData)
dim(testData)

#Part D: Build a Model using ksvm( ) 
#5.	Build a support vector model using the ksvm( ) function using two or three of the variables to predict a happy customer.
svmoutput<-ksvm(happyCustomer~checkInSat+hotelClean+hotelFriendly+whenBookedTrip,data=trainData,kernel="rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)

#6.	Write a block comment that summarizes what you learned from the book about those parameters. The two parameters of greatest interest are C=5 and cross=3.
#C is known as cost and it determines the cost of mistakes that can be made while training the data. Higher the value of C, less is the number of mistakes that can occur and vice versa.
#Cross is known as cross validation and it s used to train the data the number of times we asked it to do. For example, if cross=3, then it will train the data 3 times with different set of data. 
#Therfore, higher the value of cross, more the model would be trained which is not advised since the alogrithm will be biased and can't be used elsewhere.

#7.	Store the output of kvsm( ) in a variable and then echo that variable to the console.
svmoutput

#Part E: Predict Values in the Test Data and Create a Confusion Matrix
#8.	Use the predict( ) function to validate the model against test data.
svmPred<-predict(svmoutput,testData,type="votes")

#9.	Now the svmPred object contains a list of votes in each of its rows. The votes are either for "happy" or "notHappy". Review the contents of svmPred using str( ) and head( ).
str(svmPred)
head(svmPred)

#10.	Create a confusion matrix (a 2 x 2 table) that compares the second row of svmPred to the contents of testData$happy variable.
View(svmPred)
svmPred[svmPred[,1]>0.8]<-1
svmPred[svmPred[,1]<=0.8]<-0

comptable<-data.frame(testData$happyCustomer,svmPred)
table(comptable)
#The output shows that out of 3334 rows in test data, 1651 cases were predicted false and it was actually false whereas 1210 were predicted true and were actually true. Accuracy is 85 percent.

#11.	Calculate an error rate based on what you see in the confusion matrix. See pages 243-244 for more information.
#The number of cases which were predicted wrongly:397+76=473
#Error rate:473/3334=0.14 which is approximately 14%

#12.	Repeat Parts C and D to try and improve your prediction
#Creating Train Data
cutPoint3_4 <- floor(3 * dim(hotelSurvey1)[1]/4)
cutPoint3_4
randIndex<-sample(1:dim(hotelSurvey1)[1])
trainData1 <- hotelSurvey1[randIndex[1:cutPoint3_4],]

#Creating Test Data
testData1 <-hotelSurvey1[randIndex[(cutPoint3_4+1):dim(hotelSurvey1)[1]],]
View(testData1)

dim(trainData1)
dim(testData1)

#Building svm model
svmoutput1<-ksvm(happyCustomer~checkInSat+hotelClean+hotelFriendly+whenBookedTrip,data=trainData,kernel="rbfdot",kpar="automatic",C=50,cross=3,prob.model=TRUE)
svmoutput1

#Predicting the svm model
svmPred1<-predict(svmoutput1,testData1,type="votes")

svmPred1[svmPred1[,1]>0.8]<-1
svmPred1[svmPred1[,1]<=0.8]<-0

comptable1<-data.frame(testData1$happyCustomer,svmPred1)
table(comptable1)

#Number of cases predicted wrongly:67+279=346
#By increasing the training data to 3/4th and by increasing the value of c from 5 to 50, I was able to reduce the error rate from 14 percent to 10 percent.

#13.	Explain, in a block comment, why it is valuable to have a "test" dataset that is separate from a "training" dataset?
#Test data that is separate from training data is valuable because it helps us understand whether the model which we built using training data was accurate enough or not.

