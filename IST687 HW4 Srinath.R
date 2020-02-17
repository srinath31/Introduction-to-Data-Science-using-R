#Course Number: IST687
#Srinath Ramachandran
#Homework#4
#Date the assignment is due: 9/26/2018
#Date in which assignment is submitted: 9/24/2018


#Part A: Write a function to reveal the distribution of a vector of numeric values

#1.	Create a new function 'printVecInfo' and have it take one numeric vector as its input argument. 
printVecInfo<-function(numbers)
#2.	Make the function print the following information for the vector supplied in the argument:
{
#Add labels to each element of the function's output.  
  print(paste("Mean: ",mean(numbers)))                             #Print mean          
  print(paste("Median: ",median(numbers)))                         #Print median
  print(paste("Min: ",min(numbers)))                               #Print min
  print(paste("Max: ",max(numbers)))                               #Print max   
  print(paste("Standard Deviation: ",sd(numbers)))                 #Print standard deviation
  print(paste("0.05 & 0.95 Quantiles are: "))
  print(quantile(numbers,probs=c(0.05,0.95)))                      #Print 0.05 and 0.95 quantiles   
}

#3.	Test the function with this vector: testVector <- 1:10.
testVector<-c(1:10)
printVecInfo(testVector)


#Part B: Read the census dataset

#5. Reading and cleaning the census dataset
#url_to_read<-"https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"
dfstates<-read.csv("states.csv")      #Assigning the csv file to a variable named dfstates



#Step B: Clean the dataframe

View(dfstates)                            #Viewing the dataframe dfstates      
head(dfstates)                            #Viewing the dataframe from the top
tail(dfstates)                            #Viewing the data from the bottom

#Obtaining summary and structure of the dataframe dfstates
summary(dfstates)
str(dfstates)


#Step B & C: Cleaning the dataframe by creating a function

#A function named states is written which does all the cleaning and returns the cleansed dataframe.
states<-function()
{
  dfstates_clean<-dfstates[-53,]          #Removing the last row as it has non numeric values
  dfstates_clean                          #Displaying after removing the last row
  dfstates_clean<-dfstates_clean[-1,]     #Removing the first row since it has information of US as a country whereas the column has US states information
  dfstates_clean                          #Displaying after removing the first row
  
  dfstates_clean<-dfstates_clean[,-1:-4]  #Removing first four columns as they are not needed
  dfstates_clean                          #Displaying after removing first four columns
  
  #Changing the column names
  colnames(dfstates_clean)<-c("StateName","Population","PopOver18","PercentOver18")     
  
  dfstates_clean                          #Displaying after changing the column names
  return(dfstates_clean)                  #Returning the cleansed dataframe
  
}
states<-states()                          #Assigning the cleansed dataframe to a new variable named states
states                                    #Displaying states



#Part C: Sample from the state population data frame

#6.	Sample 20 observations from states$population and use printVecInfo( ) to display the characteristics of the resulting sample, and then display the results as a histogram.
s<-sample(states$Population,size=20)
printVecInfo(s)
hist(s)

#7.	Repeat step six two more times. Each time that you create a sample, run the resulting vector through printVecInfo( ) and create a histogram. 
s1<-sample(states$Population,size=20)
printVecInfo(s1)
hist(s1)

s2<-sample(states$Population,size=20)
printVecInfo(s2)
hist(s2)

#8.	Using a block comment, explain in a comment why each result is different.
# The reason why each result is different is because when each time the sample function is run, 
#it takes randomly 20 different samples from a set of 51 rows. Hence all the values are different.


#Part D: Replicate the sampling

#9.	Use the replicate function, to replicate the sampling (described in step 6 above). Replicate the sampling 2000 times, then use printVecInfo( ) to display the characteristics of the resulting replicated sample, and then display the results as a histogram.
r<-replicate(2000,sample(states$Population,size=20))
printVecInfo(r)
hist(r)

#10.	Repeat step 8 two more times. Each time that you create the replicated sample, run the resulting vector through printVecInfo( ) and create a histogram. 
r1<-replicate(2000,sample(states$Population,size=20))
printVecInfo(r1)
hist(r1)

r2<-replicate(2000,sample(states$Population,size=20))
printVecInfo(r2)
hist(r2)

#11.	 Using a block comment,  explain why the histograms generated in Part C are different than the histograms generated in Part D
#In replicate function different samples of size=20 are taken and replicated. This results in closer grouping of values which were otherwise very spread out 
#while using the sampling function. Also the grouping happens closer towards the mean value if the number of replications done is high.

