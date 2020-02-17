#Course Number: IST687
#Srinath Ramachandran
#Homework#3
#Date the assignment is due: 9/20/2018
#Date in which assignment is submitted: 9/15/2018



#Step A: Use read.csv( ) and url( ) to read a CSV file form the web into a data frame

url_to_read<-"https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"
dfstates<-read.csv(url(url_to_read))      #Assigning the csv file to a variable named dfstates



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


#Step D: Explore the dataframe

#Calculating Average of population
avgpop<-mean(states$Population)           
avgpop                                    #Displaying Average

#Finding the state with maximum population
Maxstatepop<-states[which.max(states$Population),]
Statename<-Maxstatepop$StateName
states[Statename,]

#Creating a histogram of the state populations
hist(states$Population,breaks=20)
#OBSERVATION: There are more number states which have population under 10 million while a few states have population more than 10 million.

#Sorting the dataframe
sortedstates<-states[order(states$Population),]
sortedstates

#Showing the 10 states with lowest populations
head(sortedstates,n=10)

#Use barplot( ) to create a plot of each of the population from the sorted dataframe.  
barplot(sortedstates$Population)
#OBSERVATION: The graph is ascending upwards since we have used the sorted dataframe. Also more number of states have a population under 10 million.
