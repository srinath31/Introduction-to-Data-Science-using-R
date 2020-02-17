# Decision Trees
# Script for IST687 project
# 12 / 2 / 2018

# Loading packages
library('rpart')
library('rpart.plot')
library('ggplot2')
library('caret')
library('e1071')

# Loading data
setwd('Your Path')
df <- read.csv("Satisfaction Survey.csv")

# Data cleaning
# Filling in some missing values
df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes) & (df$Flight.cancelled == 'Yes'))] <- 0
df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes) & (df$Flight.cancelled == 'Yes'))] <- 0
df$Flight.time.in.minutes[which(is.na(df$Flight.time.in.minutes) & (df$Flight.cancelled == 'Yes'))] <- 0

#Type cast satisfaction to numeric
df$Satisfaction <- as.numeric(as.character(df$Satisfaction))

# Dumping all the rows with missing values
ndf <- na.omit(df)

# Satisfaction bucketing
createBucketSat <- function(vec){ 
  vBuckets <- replicate(length(vec), "Unhappy")
  vBuckets[vec >= 3] <- "Happy"
  vBuckets[vec < 3] <- "Unhappy"
  return(vBuckets)
}

# Bucketing numeric numbers
createBuckets <- function(vec){
  q <- quantile(vec, c(0.4, 0.6), na.rm = "TRUE")
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}

ndf$Satisfaction <- createBucketSat(ndf$Satisfaction)

Treedf <-  data.frame(
  ndf$Satisfaction,
  ndf$Airline.Status,
  ndf$Age,
  ndf$Gender,
  ndf$No.of.Flights.p.a.,
  ndf$X..of.Flight.with.other.Airlines,
  ndf$Type.of.Travel,
  ndf$Shopping.Amount.at.Airport,
  ndf$Eating.and.Drinking.at.Airport,
  ndf$Class,
  ndf$Airline.Name
)

colnames(Treedf) <- c('Sat', 'Status', 'Age', 'Gender', 'NOF','XOF', 'Type', 'Shop', 'Eat', 'Class', 'Airline')

# Generate random numbers of the size of the whole dataset which later be used as indexes
randIndex <- sample(1:dim(Treedf)[1])
head(randIndex,10)

# Cutting point that seperates training data and testing data
cutPoint2_3 <- floor(2 * dim(Treedf)[1]/3)

# Generating training dataset 
trainData <- Treedf[randIndex[1 : cutPoint2_3], ]

# Generating testing dataset
testData <- Treedf[randIndex[(cutPoint2_3+1) : dim(Treedf)[1]], ]

# Generate random numbers of the size of the whole dataset which later be used as indexes
randIndex <- sample(1:dim(ndf)[1])

# Cutting point that seperates training data and testing data
cutPoint2_3 <- floor(2 * dim(ndf)[1]/3)

# Generating training dataset 
ndftrainData <- ndf[randIndex[1 : cutPoint2_3], ]

# Generating testing dataset
ndftestData <- ndf[randIndex[(cutPoint2_3+1) : dim(ndf)[1]], ]

# happy airline
West1 <- subset(ndf, Airline.Name == "West Airways Inc. ") 
randIndex <- sample(1:dim(West1)[1])
cutPoint2_3 <- floor(2 * dim(West1)[1]/3)
ndftrainData <- West1[randIndex[1 : cutPoint2_3], ]
ndftestData <- West1[randIndex[(cutPoint2_3+1) : dim(West1)[1]], ]

tree3 <- rpart(Satisfaction ~ Airline.Status + Type.of.Travel + Arrival.Delay.greater.5.Mins, data = ndftrainData, cp = .001)
rpart.plot(tree3, box.palette="RdBu", shadow.col="gray", nn=TRUE)

treepred3 <- predict(tree3, newdata = ndftestData)
pred3 <- replicate(length(treepred3)/2, 'Unhappy')
for(i in 1:(length(treepred3)/2)){
  if(treepred3[i, 1] > treepred3[i, 2]){
    pred3[i] <- 'Happy'
  }else if(treepred3[i, 1] <= treepred3[i, 2]){
    pred3[i] <- 'Unhappy'
  }
}

pred3 <- as.factor(pred3)
confusionMatrix(pred3, as.factor(ndftestData$Satisfaction))

# Unhappy airline
GoingNorth1 <- subset(ndf, Airline.Name == "GoingNorth Airlines Inc. ") 
randIndex <- sample(1:dim(GoingNorth1)[1])
cutPoint2_3 <- floor(2 * dim(GoingNorth1)[1]/3)
ndftrainData <- GoingNorth1[randIndex[1 : cutPoint2_3], ]
ndftestData <- GoingNorth1[randIndex[(cutPoint2_3+1) : dim(GoingNorth1)[1]], ]

tree4 <- rpart(Satisfaction ~ Airline.Status + Type.of.Travel + Arrival.Delay.greater.5.Mins, data = ndftrainData, cp = .0006)
rpart.plot(tree4, box.palette="RdBu", shadow.col="gray", nn=TRUE)

treepred4 <- predict(tree4, newdata = ndftestData)
pred4 <- replicate(length(treepred4)/2, 'Unhappy')
for(i in 1:(length(treepred4)/2)){
  if(treepred4[i, 1] > treepred4[i, 2]){
    pred4[i] <- 'Happy'
  }else if(treepred3[i, 1] <= treepred3[i, 2]){
    pred4[i] <- 'Unhappy'
  }
}

pred4 <- as.factor(pred4)
confusionMatrix(pred4, as.factor(ndftestData$Satisfaction))