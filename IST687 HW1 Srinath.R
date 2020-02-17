#Course Number: IST687
#Srinath Ramachandran
#Homework#1
#Date the assignment is due: 9/5/2018
#Date in which assignment is submitted: 9/1/2018


#Define a vector 'grades', which contains the numbers 4.0, 3.3 and 3.7
grades<-c(4.0,3.3,3.7)
#Define a vector 'courseName', which contain the strings "Bio", "Math", "History".
courseName<-c("Bio","Math","History")
#Define a variable 'BetterThanB', that is equal to 3
BetterThanB<-3

#Calculating Average of grades using mean function
mean(grades)

#Calculating the number of observations using lenght() function and storing it in variable total.length
total.length<-length(grades)

#Printing the value of total.length
print(total.length)

#Calculating the sum of grades and storing the result in the variable total
total=sum(grades)

#Calculating the average of grades by dividing total/total.length and printing that average
average<-total/total.length
print(average)

#Computing max grades and storing it in maxG
maxG<-max(grades)

#Computing min grades and storing it in minG
minG<-min(grades)

#Create a new vector called betterGrades, which is the grades + 0.3 (each grade improved each grade by  0.3 points)
betterGrades<-grades+0.3

#Computing the average of betterGrades
mean(betterGrades)

#Test if maxG is greater than3.5 (output "yes" or "no")
if(maxG>3.5) "yes" else "no"

#Test if minG is greater than the variable 'BetterThanB'' (output "yes" or "no")
if(minG>BetterThanB) "yes" else "no"

#Output the name of the second class, in the 'courseName' vector
print(courseName[2])