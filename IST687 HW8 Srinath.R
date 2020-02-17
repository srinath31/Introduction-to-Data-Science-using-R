#Course Number: IST687
#Srinath Ramachandran
#Homework#8
#Date the assignment is due: 10/25/2018
#Date in which assignment is submitted: 10/23/2018


#Step A: Load and condition the data

#1) Read the json file
library("RJSONIO")
path<-"F:\\Srinath_Syracuse\\hotelSurveySherison.json"
mydata<-fromJSON(path,simplify=TRUE,nullValue=NA)
View(mydata)
df <- data.frame(mydata, stringsAsFactors = FALSE)

#2.	Use the str command to make sure you can see the following attributes
str(df)

#Step B: Explore the data

#3.	Create bivariate plots for each of the attributes.
library(ggplot2)

myplot1<-ggplot(df,aes(y=overallCustSat,x=hotelSize))+geom_point()
myplot1
x1<-jitter(df$hotelSize)
y1<-jitter(df$overallCustSat)
myjitterplot1<-ggplot(df,aes(y=y1,x=x1))+geom_point()
myjitterplot1<-myjitterplot1+xlab("hotelSize")+ylab("overallCustSat")
myjitterplot1


myplot2<-ggplot(df,aes(y=overallCustSat,x=checkInSat))+geom_point()
myplot2
x2<-jitter(df$checkInSat)
y2<-jitter(df$overallCustSat)
myjitterplot2<-ggplot(df,aes(y=y2,x=x2))+geom_point()
myjitterplot2<-myjitterplot2+xlab("checkInSat")+ylab("overallCustSat")
myjitterplot2

myplot3<-ggplot(df,aes(y=overallCustSat,x=hotelClean))+geom_point()
myplot3
x3<-jitter(df$hotelClean)
y3<-jitter(df$overallCustSat)
myjitterplot3<-ggplot(df,aes(y=y3,x=x3))+geom_point()
myjitterplot3<-myjitterplot3+xlab("hotelClean")+ylab("overallCustSat")
myjitterplot3

myplot4<-ggplot(df,aes(y=overallCustSat,x=hotelFriendly))+geom_point()
myplot4
x4<-jitter(df$hotelFriendly)
y4<-jitter(df$overallCustSat)
myjitterplot4<-ggplot(df,aes(y=y4,x=x4))+geom_point()
myjitterplot4<-myjitterplot4+xlab("hotelFriendly")+ylab("overallCustSat")
myjitterplot4

myplot5<-ggplot(df,aes(y=overallCustSat,x=guestAge))+geom_point()
myplot5
x5<-jitter(df$guestAge)
y5<-jitter(df$overallCustSat)
myjitterplot5<-ggplot(df,aes(y=y5,x=x5))+geom_point()
myjitterplot5<-myjitterplot5+xlab("guestAge")+ylab("overallCustSat")
myjitterplot5

myplot6<-ggplot(df,aes(y=overallCustSat,x=lengthOfStay))+geom_point()
myplot6
x6<-jitter(df$lengthOfStay)
y6<-jitter(df$overallCustSat)
myjitterplot6<-ggplot(df,aes(y=y6,x=x6))+geom_point()
myjitterplot6<-myjitterplot6+xlab("lengthOfStay")+ylab("overallCustSat")
myjitterplot6


myplot7<-ggplot(df,aes(y=overallCustSat,x=whenBookedTrip))+geom_point()
myplot7
x7<-jitter(df$whenBookedTrip)
y7<-jitter(df$overallCustSat)
myjitterplot7<-ggplot(df,aes(y=y7,x=x7))+geom_point()
myjitterplot7<-myjitterplot7+xlab("whenBookedTrip")+ylab("overallCustSat")
myjitterplot7

#4.	What do you observe from the plots? Note via a block comment.
#Jitter function gives us more insights in terms of visualization as it adds more noise. 

#Step C: Generate a linear model
#5.	Next, create one regression model predicting the overall customer satisfaction from the other variables (but not the freeText response). 
model1<-lm(formula=overallCustSat~hotelSize+checkInSat+hotelState+hotelClean+hotelFriendly+gender+guestAge+lengthOfStay+whenBookedTrip,data=df)
summary(model1)

#6.	Report the R-Squared in a comment. Which of the predictors are statistically significant in the model? In a comment, report the coefficients (AKA slopes or B-weights) for each predictor that is statistically significant. 

#The Multiple R-squared value is 0.6702

#The predictors that are statistically significant in the model are:
#guestAge,lengthOfStay,whenBookedTrip,hotelClean,hotelFriendly,checkInSat

#The co-efficients of each predictor that are statistically significant are:
#guestAge->        -1.205e-01   
#lengthOfStay->    -3.284e-01
#whenBookedTrip->   6.421e-03
#hotelClean->       4.042e-02
#hotelFriendly->    1.122e+00
#checkInSat->      -2.381e-01

#7.	Write a block comment that explains in a narrative your overall interpretation of the model. Make sure to refer to each variable (one dependent and three independent) by a descriptive name

#Dependent Variables used(Denoted by Xi)
#X1=hotelSize
#X2=checkInSat
#X3=hotelState
#X4=hotelClean
#X5=hotelFriendly
#X6=gender
#X7=guestAge
#X8=lengthOfStay
#X9=whenBookedTrip

#Independent Variable(Denoted by Y)
#Y=overallCustSat

#In the above model, there were many independent variables used(9 independent variables). But the output of the model suggested that 6
#of those 9 independent variables were significant in determining the outcome of the dependent variable(Y i.e. overallCustSat).
#Also an R-squared value of 0.6702 suggests that the model is pretty good as the R-squared value is near to 1.


#Step D: Generate a different linear model 
#8.	Next, create a different regression model predicting the overall customer satisfaction from the one variable you think is best.  Then create another using two variables.
model2<-lm(formula=overallCustSat~hotelClean,data=df)                 #Creating a linear model using the variable hotelClean
summary(model2)

model3<-lm(formula=overallCustSat~hotelClean+hotelFriendly,data=df)   #Creating a linear model using two variables i.e. hotelClean & hotelFriendly
summary(model3)

#9.	Write a block comment comparing the two lm models in #8.
#It can be observed that when the number of independent variables is increased the R-squared value becomes healthier meaning the value
#increases towards reaching 1. In case of this example the R-squared value when only one varibale was used was 0.125 whereas 
#when 2 variables were used the the R-squared value increased to 0.3919.
