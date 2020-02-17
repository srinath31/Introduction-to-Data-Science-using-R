#Course Number: IST687
#Srinath Ramachandran
#Homework#6
#Date the assignment is due: 10/11/2018
#Date in which assignment is submitted: 10/10/2018

#Step A: Load and Merge datasets
#1)	Read in the census dataset (using the function created in HW 3)
url_to_read<-"https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"
dfstates<-read.csv(url(url_to_read))                  #Assigning the csv file to a variable named dfstates
dfstates

#Cleaning the dataframe.
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
View(states)                              #Displaying states

#2)	Copy the USArrests dataset into a local variable (similar to HW 2)
arrests<-USArrests
View(arrests)

#3)	Create a merged dataframe -- with the attributes from both dataset
arrests$statename<-row.names(arrests)
View(arrests)
states_arrests<-merge(arrests,states,by.x="statename",by.y="StateName")
View(states_arrests)


#Step B: Explore the Data - Understanding distributions
str(states_arrests)
summary(states_arrests)

#Installing ggplot2 package
install.packages("ggplot2")
library(ggplot2)

#4)	Create a histogram using ggplot2() for the population and a different histogram for the murder rate
#Popuation
histogram_Population<-ggplot(states_arrests,aes(x=Population))                                     #Providing X-axis value using aesthetics function
histogram_Population<-histogram_Population+geom_histogram(bins=20,color="black",fill="white")      #Creating Histogram using geom_histogram function
histogram_Population<-histogram_Population+ggtitle("Histogram of Population of states in the US")  #Providing title to the histogram
histogram_Population                                                                               #Viewing Histogram

#Murder
histogram_Murder<-ggplot(states_arrests,aes(x=Murder))
histogram_Murder<-histogram_Murder+geom_histogram(bins=5,color="black",fill="white")
histogram_Murder<-histogram_Murder+ggtitle("Histogram of Murder rates of states in the US")
histogram_Murder

#Building Histograms of remaining three variables. We need to change the value of bins in order to make the histograms look right.
#Assault
histogram_Assault<-ggplot(states_arrests,aes(x=Assault))
histogram_Assault<-histogram_Assault+geom_histogram(bins=20,color="black",fill="white")
histogram_Assault<-histogram_Assault+ggtitle("Histogram of Assault rates of states in the US")
histogram_Assault

#Rape
histogram_Rape<-ggplot(states_arrests,aes(x=Rape))
histogram_Rape<-histogram_Rape+geom_histogram(bins=10,color="black",fill="white")
histogram_Rape<-histogram_Rape+ggtitle("Histogram of Rape rates of states in the US")
histogram_Rape

#Urban Population
histogram_urbanpop<-ggplot(states_arrests,aes(x=UrbanPop))
histogram_urbanpop<-histogram_urbanpop+geom_histogram(bins=10,color="black",fill="white")
histogram_urbanpop<-histogram_urbanpop+ggtitle("Histogram of Urban Population of states in the US")
histogram_urbanpop

#5)	Create a boxplot for the population, and a different boxplot for the murder rate.
#Population
boxplot_Population<-ggplot(states_arrests,aes(x=factor(0),y=Population))
boxplot_Population<-boxplot_Population+geom_boxplot()
boxplot_Population

#Murder
boxplot_Murder<-ggplot(states_arrests,aes(x=factor(0),y=Murder))
boxplot_Murder<-boxplot_Murder+geom_boxplot()
boxplot_Murder

#6)	Create a block comment explaining which visualization (boxplot or histogram) you thought was more helpful (explain why)
#I found histogram more helpful because end users can easily deduce results with the help of histograms while boxplot might be better understood by an analyst.
#Also histogram offers distribution in a better way than boxplot.


#Step C: Which State had the Most Murders - bar charts

#7)	Calculate the number of murders per state
states_arrests$numMurders<-(states_arrests$Population*states_arrests$Murder)/100000
View(states_arrests)


#8)	Generate a bar chart, with the number of murders per state
barcharts_nummurders_state<-ggplot(states_arrests,aes(x=statename,y=numMurders))
barcharts_nummurders_state<-barcharts_nummurders_state+geom_col()
barcharts_nummurders_state

#9)	Generate a bar chart, with the number of murders per state. Rotate text (on the X axis), so we can see x labels, also add a title named "Total Murders".
barcharts_nummurders_state<-barcharts_nummurders_state+theme(axis.text.x = element_text(angle=90,hjust=1))
barcharts_nummurders_state<-barcharts_nummurders_state+ggtitle("Total Murders")
barcharts_nummurders_state

#10) Generate a new bar chart, the same as in the previous step, but also sort the x-axis by the murder rate
sorted_barchart<-ggplot(states_arrests,aes(reorder(statename,numMurders),y=numMurders))
sorted_barchart<-sorted_barchart+geom_col()
sorted_barchart<-sorted_barchart+ggtitle("Total Murders")
sorted_barchart<-sorted_barchart+theme(axis.text.x = element_text(angle=90,hjust=1))
sorted_barchart

#11) Generate a third bar chart, the same as the previous step, but also showing percentOver18 as the color of the bar
color_barchart<-ggplot(states_arrests,aes(reorder(statename,numMurders),y=numMurders))
color_barchart<-color_barchart+geom_col()
color_barchart<-color_barchart+ggtitle("Total Murders")
color_barchart<-color_barchart+theme(axis.text.x = element_text(angle=90,hjust=1))
color_barchart<-color_barchart+aes(fill=PercentOver18)
color_barchart

#Step D: Explore Murders - scatter chart
#12)	 Generate a scatter plot - have population on the X axis, the percent over 18 on the y axis, and the size & color represent the murder rate.
scatter_plot<-ggplot(states_arrests,aes(x=Population,y=PercentOver18,color=Murder,size=Murder))
scatter_plot<-scatter_plot+geom_point()
scatter_plot
