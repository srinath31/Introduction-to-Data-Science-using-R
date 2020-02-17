#Course Number: IST687
#Srinath Ramachandran
#Homework#2
#Date the assignment is due: 9/12/2018
#Date in which assignment is submitted: 9/9/2018

#Step A: Initialize an 'arrests' dataframe
#Copy USArrests into a new variable (called 'arrests').  
arrests<-USArrests
#Displaying arrests
arrests
#Obtaining the structure and summary of the datframe arrests
str(arrests)
summary(arrests)

#Step B: Explore the assault rate
#Is a higher or lower assault rate best? Obtaining the state with lowest i.e with the best assault rate
row.names(arrests[which.min(arrests$Assault),])

#Step C: Explore the murder rate  
#Obtaining the state with highest murder rate
row.names(arrests[which.max(arrests$Murder),])

#Create a sorted dataframe, based on descending murder rate
sorted.arrests<-arrests[order(-arrests$Murder),]

#Show the 10 states with the highest murder rate
head(arrests[order(-arrests$Murder),],10)

#What is the value of the 20th row, third column (in the sorted dataframe)? 
sorted.arrests[20,3]


#Step D: Which state is the least safe? Explain your logic
#Description of my Approach
'In this code
1. firstly I have obtained the names of the states with highest murder, assault and rape cases. 
   I have done this by taking the values of assault, murder and rape in the range between 3rd quartile and max and 
   storing them in 3 vectors.
2. Now I found the intersection of all the three vectors which I got in step 1. By doing so I got a list of 5 most 
   unsafe states in USA.
3. I obtained the rape, assault and murder data of these 5 common states and stored them in a variable unsafedata.
4. Now I calculated the weights of each state. I have done this by (assault+murder+rape rates)/urban population of
   each state. I have assumed the numbers to be as they are, meaning I have not multiplied urban population 
   by 10 or 100 or 1000. The concept over here is for a small set of population in a particular state, 
   if there are more number of cases then that state is the most unsafe state.
5. After obtaining the weights, I took the index of the max weight.
6. I used this index to find out the statename in unsafedata and found out that Maryland is the most unsafe state.'


murderstates<-row.names(subset(arrests,arrests$Murder>=11.250 & arrests$Murder<=17.400))
assaultstates<-row.names(subset(arrests,arrests$Assault>=249.0 & arrests$Assault<=337.0))
rapestates<-row.names(subset(arrests,arrests$Rape>=26.18 & arrests$Rape<=46.0))

states1<-intersect(murderstates,assaultstates)
unsafestates<-intersect(states1,rapestates)
unsafestates
unsafedata<-arrests[unsafestates,]

unsafestateweights<-c((unsafedata[1,1]+unsafedata[1,2]+unsafedata[1,4])/unsafedata[1,3],
(unsafedata[2,1]+unsafedata[2,2]+unsafedata[2,4])/unsafedata[2,3],
(unsafedata[3,1]+unsafedata[3,2]+unsafedata[3,4])/unsafedata[3,3],
(unsafedata[4,1]+unsafedata[4,2]+unsafedata[4,4])/unsafedata[4,3],
(unsafedata[5,1]+unsafedata[5,2]+unsafedata[5,4])/unsafedata[5,3])
unsafestateweights
index<-which.max(unsafestateweights)

sprintf("The most unsafe state according to my calculations should be %s",unsafestates[index])