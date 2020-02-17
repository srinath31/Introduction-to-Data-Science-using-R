#Course Number: IST687
#Srinath Ramachandran
#Homework#11
#Date the assignment is due: 11/29/2018
#Date in which assignment is submitted: 11/28/2018


#Part A: Load and condition the text file that contains the speech

#1.	The data is available on blackboard, as a JSON file (see HW8 if you need a reminder on the dataset or how to load the dataset).
install.packages("RJSONIO")
library(RJSONIO)
path2<-"F:\\Srinath_Syracuse\\hotelSurveySherison.json"
mydata2<-fromJSON(path2,simplify=TRUE,nullValue=NA)
df2 <- data.frame(mydata2, stringsAsFactors = FALSE)
View(df2)

#2.	The key column to focus on is the 'freeText' column.
View(df2$freeText)

#scan the file and read the content into "p"
pos<-"F:\\Srinath_Syracuse\\positive-words.txt"
p <- scan(pos,character(0), sep="\n")
p<-p[c(-1:-34)]
summary(p)
View(p)

#scan the file and read the content into "n"
neg <- "F:\\Srinath_Syracuse\\negative-words.txt"
n <- scan(neg,character(0), sep="\n")
n<-n[c(-1:-34)]
View(n)

#Part B: Create a list of word counts from the speech
install.packages("tm")
library(tm)
install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

createWordCounts <- function(vFreeText) {
  words.vec <- VectorSource(vFreeText)
  #create big bag of words..corpus
  words.corpus <- Corpus(words.vec)
  words.corpus <- tm_map(words.corpus, content_transformer(tolower))
  words.corpus <- tm_map(words.corpus, removePunctuation)
  words.corpus <- tm_map(words.corpus, removeNumbers)
  words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
  
  
  tdm <- TermDocumentMatrix(words.corpus)
  # create a list of counts for each word
  m<- as.matrix(tdm)
  wordCounts <- rowSums(m)
  wordCounts <- sort(wordCounts, decreasing = TRUE)
  return(wordCounts)
}

wordCounts<-createWordCounts(df2$freeText)
View(wordCounts)


#Part B: Create a list of word counts from the speech
#3.	Starting with the code at the bottom of page 180 in the text book, use a similar approach to transform the free text into a term document matrix, and then determine positive and negative word matches.
getMatched <- function(wordCounts,PorN)
{
  words<- names(wordCounts)
  
  matched<-match(words,PorN,nomatch=0)
  return(matched)
}
matchedP<-getMatched(wordCounts,p)
matchedN<-getMatched(wordCounts,n)

View(matchedP)
View(matchedN)

#4.	Calculate the percent positive words and negative words.

CalcPosNeg<- function(wordCounts,matchedP, matchedN)
{
  pTotal<- sum(wordCounts[which(matchedP!=0)])
  pTotal
  
  nTotal<- sum(wordCounts[which(matchedN!=0)])
  View(nTotal)
  totalWords <- sum(wordCounts)
  retVal <- c(pTotal, pTotal/totalWords,nTotal, nTotal/totalWords)
  return(retVal)
} 

ret<-CalcPosNeg(wordCounts,matchedP,matchedN)
ret
cat("num pos words (%)",ret[1]," ",ret[2],"num of neg words",ret[3],ret[4])


#5.	Write a block comment that summarizes what you learned from ratioPos and ratioNeg.
#Majority are positve comments

#Part C: Visualize the results
#6.	Create a word cloud

genWordCloud<-function(wordCounts){
  cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
  wordcloud(names(wordCounts),wordCounts,min.freq=2,max.words=30,rot.per=0.35,colors=brewer.pal(8,"Dark2"))
}

#7.	Create a barplot of the positive and negative words that matched (at least twice)
genBarChart<-function(wordCounts,matched){
  sortedWords<-sort(wordCounts[matched>1])
  barplot(sortedWords,las=2,cex.names=0.75)
}

genWordCloud(wordCounts)
genBarChart(wordCounts,matchedP)
genBarChart(wordCounts,matchedN)


#8.	Write a block comment on what you observe from these two barplots and the wordcloud. 
#barplots are more easier to interpret than wordcloud.

#9.	Does these results make sense to you in terms of the kinds of emotions you see?
#Many of the results make sense in terms of emotions but not all like room,front,bed etc.
#Which do you think is more informative - barplot or the wordcloud?
#I think barplots are more informative because we can obtain the frequency of the words easily from barplots. 


#Part D: Evaluate Happy and not Happy customer responses
#10.	Create two subset of the text vectors: one for happy customers and one for not happy customers 
#(based on overall customer satisfaction)
hotelSurveylow<- df2[df2$overallCustSat<7,]
wordCountslow<-createWordCounts(hotelSurveylow$freeText)

hotelSurveyhigh<- df2[df2$overallCustSat>=7,]
wordCountshigh<-createWordCounts(hotelSurveyhigh$freeText)

#11.	Redo Steps B, C & D, for these two subsets of the text strings.

matchedP <- getMatched(wordCountslow,p)
matchedN <- getMatched(wordCountslow,n)
ret <- CalcPosNeg(wordCounts, matchedP, matchedN)
cat("num pos words(%)",ret[1],"",ret[2],"num neg words",ret[3], ret[4])
genWordCloud(wordCountslow)
genBarChart(wordCountslow,matchedP)
genBarChart(wordCountslow,matchedN)

matchedP <- getMatched(wordCountshigh,p)
matchedN <- getMatched(wordCountshigh,n)
ret <- CalcPosNeg(wordCounts, matchedP, matchedN)
cat("num pos words(%)",ret[1],"",ret[2],"num neg words",ret[3], ret[4])
genWordCloud(wordCountshigh)
genBarChart(wordCountshigh,matchedP)
genBarChart(wordCountshigh,matchedN)

#12.	Compare the positive and negative ratios for these two different group of customers 
#when I observed the lower overall customer satisfaction group, there were more negative words in comparison to positive words.
#Similarly,the group with high overall customer satisfaction has more positive words in comparison with negative words.
