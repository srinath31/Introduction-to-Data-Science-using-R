#Course Number: IST687
#Srinath Ramachandran
#Homework#5
#Date the assignment is due: 10/03/2018
#Date in which assignment is submitted: 10/03/2018

#Installing Rcurl package
install.packages("RCurl")
library(RCurl)

#Installing RJSONIO package
install.packages("RJSONIO")
library(RJSONIO)

#Step A: Load the data

#1. Reading the JSON Dataset.
jsonLocation <- "http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD"
datasetURL <- getURL(jsonLocation)
mydata <- fromJSON(datasetURL, simplify = FALSE, nullValue = NA)                      
View(mydata)
myList <- mydata[[2]]
View(myList)
numRows <- length(myList)
df <- data.frame(matrix(unlist(myList), nrow=numRows, byrow=T), stringsAsFactors = FALSE)
View(df)

#Step B: Clean the data

#2. Removing the first 8 columns
df_new<-df[,-1:-8]
View(df_new)

#3. Changing the column names.
colnames(df_new)<-c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD",
                    "INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME",
                    "VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")
View(df_new)

#Step C: Explore the data - using the dataframe you created.
#Installing sqldf package
install.packages("sqldf")
library("sqldf")

#4. What is the total number of accidents with injuries?
acci_injury<-sqldf("select INJURY from df_new where INJURY='YES'")
acc_count_wth_injury<-length(acci_injury$INJURY)
acc_count_wth_injury

#5. How many accidents happened on Sunday? 
acc_Sunday<-sqldf("select count(*) from df_new where DAY_OF_WEEK like '%SUNDAY%'")
acc_Sunday

#6.	How many injuries occurred each day of the week?
eachday<-sqldf("select DAY_OF_WEEK,count(*) from df_new where INJURY='YES' group by DAY_OF_WEEK")
eachday


#Step D: Explore the data - using dplyr
#Installing dplyr package
install.packages("dbplyr")
library(dplyr)

#7.	What was the total number of accidents with injuries?
injuries<-filter(df_new,INJURY=="YES")
nrow(injuries)

#8.	How many accidents happened on Sunday?
df.GroupBydays <- group_by(df_new, DAY_OF_WEEK)
accidents <- summarize(df.GroupBydays, count = n())
accidents[4,]

#9.	How many injuries occurred each day of the week?
injuries.groupbydays<-group_by(injuries,DAY_OF_WEEK)
eachday_injuries<-summarize(injuries.groupbydays,count=n())
eachday_injuries

#10. In a block comment, explain if you find doing the analysis with the dataframe directly, or using dplyr easier.
# I found sqldf simple than dplyr since I could query directly using SQL queries and get the results.


#Step E: Explore the distribution of the number of vehicles in accidents

#11.	What is the distribution of the number of vehicles in accidents on Friday? (use a histogram and quantile)
distribute_Friday<-sqldf("select VEHICLE_COUNT,DAY_OF_WEEK from df_new where DAY_OF_WEEK like '%FRIDAY%'")
distribute_Friday<-na.omit(distribute_Friday)                              #Removing NA values
distribute_Friday
hist(as.numeric(distribute_Friday$VEHICLE_COUNT))                          #Plotting Histogram. Used as.numeric since the column was non-numeric
quantile(as.numeric(distribute_Friday$VEHICLE_COUNT),probs=c(0.05,0.95))   #Calculating Quantile

#12.	How does this distribution compare with the distribution of the number of vehicles in accidents on Sunday?(use a histogram and quantile)
distribute_Sunday<-sqldf("select VEHICLE_COUNT,DAY_OF_WEEK from df_new where DAY_OF_WEEK like '%SUNDAY%'")
distribute_Sunday<-na.omit(distribute_Sunday)                              #Removing NA values
distribute_Sunday
hist(as.numeric(distribute_Sunday$VEHICLE_COUNT))                          #Plotting Histogram. Used as.numeric since the column was non-numeric  
quantile(as.numeric(distribute_Sunday$VEHICLE_COUNT),probs=c(0.05,0.95))   #Calculating Quantile

#More number of single vehicles have been involved in accidents on sunday as compared to friday. While the involvement of 2 vehicles in 
#accidents is more on friday as compared to sunday.

