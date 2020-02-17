#Course Number: IST687
#Srinath Ramachandran
#Homework#7
#Date the assignment is due: 10/18/2018
#Date in which assignment is submitted: 10/18/2018


#Step A: Load and Merge datasets:

#1)	Read in the census dataset and the USArrests and merge them
#a)	Read in the census dataset (using the function created in HW 3)
url_to_read<-"https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv"
dfstates<-read.csv(url(url_to_read))                  #Assigning the csv file to a variable named dfstates
dfstates

#b) Cleaning the dataframe.
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

#c)	Copy the USArrests dataset into a local variable (similar to HW 2)
arrests<-USArrests
View(arrests)

#d)	Create a merged dataframe -- with the attributes from both dataset
arrests$statename<-row.names(arrests)
View(arrests)
states_arrests<-merge(arrests,states,by.x="statename",by.y="StateName")
View(states_arrests)


#2)	Add the area of each state, and the center of each state, to the merged dataframe, using the 'state.center', 'state.center' and 'state.name' vectors
state_name<-states_arrests$statename
state_center<-state.center
state_area<-state.area
state_measure<-data.frame(state_name,state_center,state_area)
state_measure

new_df<-merge(states_arrests,state_measure,by.x="statename",by.y="state_name")
View(new_df)


#Step B: Generate a color coded map

#3)	Create a color coded map, based on the area of the state 
#Installing ggmap package
install.packages("ggmap")
library(ggmap)
library(ggplot2)

new_df$statename<-tolower(new_df$statename)
View(new_df)
us<-map_data('state')
View(us)

map.areaColor<-ggplot(new_df,aes(map_id=statename))
map.areaColor<-map.areaColor+geom_map(map=us,aes(fill=new_df$state_area))
map.areaColor<-map.areaColor+expand_limits(x=us$long,y=us$lat)
map.areaColor<-map.areaColor+coord_map()+ggtitle("Area of Each State in the US")
map.areaColor


#Step C: Create a color shaded map of the U.S. based on the Murder rate for each state

#4)	Repeat step B, but color code the map based on the murder rate of each state.
map.murder<-ggplot(new_df,aes(map_id=statename))
map.murder<-map.murder+geom_map(map=us,aes(fill=new_df$Murder))
map.murder<-map.murder+expand_limits(x=us$long,y=us$lat)
map.murder<-map.murder+coord_map()+ggtitle("Murder Rate at Each State in the US")
map.murder


#5)	Show the population as a circle per state (the larger the population, the larger the circle), using the location defined by the center of each state

map.popColor<-ggplot(new_df,aes(map_id=statename))
map.popColor<-map.popColor+geom_map(map=us,aes(fill=new_df$Murder)) #fill the map based on murder rate.)
map.popColor<-map.popColor+expand_limits(x=us$long,y=us$lat)
map.popColor<-map.popColor+geom_point(data=new_df, aes(x=new_df$x, y=new_df$y, size=new_df$Population))
map.popColor<-map.popColor+coord_map()+ggtitle("Population and Murder per state")
map.popColor



#Step D: Zoom the map

#6)	Repeat step C, but only show the states in the north east
latlon <- geocode(source="dsk","nyc, new york, ny")
latlon #longitude and latitude for nyc.
mapzoom <- ggplot(new_df, aes(map_id=statename))
mapzoom <- mapzoom + geom_map(map=us, aes(fill=new_df$Murder)) #fill the map based on area
mapzoom<- mapzoom + expand_limits(x= us$long, y=us$lat)
mapzoom <- mapzoom + ggtitle("Population and Murder Per State")
mapzoom <- mapzoom + geom_point(data=new_df, aes(x=new_df$x, y=new_df$y, size=new_df$Population))
mapzoom <- mapzoom  +  xlim(c((latlon$lon-10),(latlon$lon+10))) + ylim(c((latlon$lat-10),(latlon$lat+10)))
mapzoom


