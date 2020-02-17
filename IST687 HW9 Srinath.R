#Course Number: IST687
#Srinath Ramachandran
#Homework#9
#Date the assignment is due: 11/08/2018
#Date in which assignment is submitted: 11/04/2018

#Part A: Explore Data Set
#1)	Load the dataset: hotelSurveyBarriot.json (similar to HW8, but a different dataset)
library("RJSONIO")
path<-"F:\\Srinath_Syracuse\\hotelSurveyBarriot.json"
mydata1<-fromJSON(path,simplify=TRUE,nullValue=NA)
View(mydata1)

#2)	Name the dataframe hotelSurvey
hotelSurvey <- data.frame(mydata1)


#Part B: Explore Data Set
#1)	Ensure hotelSurvey is a dataframe, and look at the structure via the str() command
View(hotelSurvey)                                 #Checking whether hotelsurvey is a dataframe
str(hotelSurvey)

#2)	Map each numeric attribute to a category  - Since we want to create rules, we should convert the attributes that have a numeric range into buckets (ex. low or high)
hotelSurvey<-hotelSurvey[,-11]                    #Removing non-useful columns
#Creating function for numeric values ranging from 1 to 10
createBucketSurvey<-function(vec){
  vBuckets<-replicate(length(vec),"Average")
  vBuckets[vec>7]<-"High"
  vBuckets[vec<7]<-"Low"
  return(vBuckets)
}
#Creating function for numeric values not ranging from 1 to 10
createBuckets<-function(vec){
  q<-quantile(vec,c(0.4,0.6))
  vBuckets<-replicate(length(vec),"Average")
  vBuckets[vec<=q[1]]<-"Low"
  vBuckets[vec>q[2]]<-"High"
  return(vBuckets)
}

#Converting the ratings of all the columns from numbers to (Average, Low and High)
happyCust<-createBucketSurvey(hotelSurvey$overallCustSat)
checkIn<-createBucketSurvey(hotelSurvey$checkInSat)
clean<-createBucketSurvey(hotelSurvey$hotelClean)
lenOfStay<-createBucketSurvey(hotelSurvey$lengthOfStay)
whenBooked<-createBuckets(hotelSurvey$whenBookedTrip)
age<-createBuckets(hotelSurvey$guestAge)
friend<-createBucketSurvey(hotelSurvey$hotelFriendly)

#3)	Count the people in each category of for the age and friendliness attributes
#Age Attribute
t<-table(age)
t
#Friendliness Atrribute
t1<-table(friend)
t1

#4)	Express the results of problem 3 as percentages by sending the results of the table() 
prop.table(t)
prop.table(t1)

#5)	Show a "contingency table" of percentages for the age and the overall satisfaction variables together. Write a block comment about what you see.
t2<-table(age,happyCust)
prop.table(t2)
#I see a table having inter connection between ages and customer satisfaction.


#Part C: Coerce the data frame into transactions
#6)	Install and library two packages: arules and arulesViz.
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

#7)	Coerce the hotelSurvey data frame into a sparse transactions matrix using:
ruledf<-data.frame(happyCust,checkIn,clean,friend,age,lenOfStay,whenBooked)          #Creating a Dataframe
hotelSurveyX <- as(ruledf,"transactions")
View(ruledf)
View(hotelSurveyX)

#8)	Use the inspect( ), itemFrequency( ), and itemFrequencyPlot( ) commands to explore the contents of hotelSurveyX.
inspect(hotelSurveyX)
itemFrequency(hotelSurveyX)
itemFrequencyPlot(hotelSurveyX)


#Part D: Use arules to discover patterns
#9)	Run the apriori command to try and predict happy customers (as defined by their overall satisfaction being high - above 7).
ruleset<-apriori(hotelSurveyX,parameter=list(support=0.1,confidence=0.5),appearance=list(default="lhs",rhs=("happyCust=High")))

#10)Once you have a reasonable number of rules, use inspect( ) to view the ruleset.
summary(ruleset)
inspect(ruleset)

#11)If you had to provide two rules to the hotel owner (in terms of what helps drive high overall customer satisfaction, what would those two rules be?  Use a block comment to explain your answer.
# From Summary of ruleset I got the maximum lift value as 2.089. So The two rules that would be recommendeded to the hotel owner would be the rules with lift value
#around 2.089. Therfore the 2 rules are:
#Rule 46:{checkIn=High,clean=High,friend=Average,whenBooked=High}----------->Lift value is 2.089386
#Rule 57:{checkIn=High,clean=High,friend=Average,lenOfStay=Low,whenBooked=High}------->Lift Value is 2.089386