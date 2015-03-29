#################################################
######### LOADING AND CLEANING DATA #############
#################################################
setwd("E:\\Dev\\R\\Coursera\\Capstone\\Coursera-Capstone\\")

urlblogs <- "./data/en_US/en_US.blogs.txt"
urlnews <- "./data/en_US/en_US.news.txt"
urltwitter <- "./data/en_US/en_US.twitter.txt"

#Load data
blogs<-readLines(urlblogs)
news<-readLines(urlnews)
twitter<-readLines(urltwitter)

#Cleaning blogs
blogs <-gsub("[^a-zA-Z]", " ", blogs) #Replace non alpha-numeric characters with space
blogs <- gsub("  ", " ",blogs) #Remove double spaces
blogs <- gsub("  ", " ",blogs) #Remove double spaces
blogs <- tolower(blogs) #Create lowercase words

#Cleaning news
news <-gsub("[^a-zA-Z]", " ", news)
news <- gsub("  ", " ",news)
news <- gsub("  ", " ",news)
news <- tolower(news) #Create lowercase words

#Cleaning twitter
twitter <-gsub("[^a-zA-Z]", " ", twitter)
twitter <- gsub("  ", " ",twitter)
twitter <- gsub("  ", " ",twitter)
twitter <- tolower(twitter) #Create lowercase words

#Storing the cleaned data for fast retrieval
writeLines(blogs, "./data/blogs.txt", sep="\n")
writeLines(news, "./data/news.txt", sep="\n")
writeLines(twitter, "./data/twitter.txt", sep="\n")

#rm(blogs)
#rm(news)
#rm(twitter)

#Load data from cleaned files
urlblogs <- "./data/blogs.txt"
urlnews <- "./data/news.txt"
urltwitter <- "./data/twitter.txt"

blogs<-readLines(urlblogs)
news<-readLines(urlnews)
twitter<-readLines(urltwitter)

#Add the blog, news and twitter data together in 1 dataset
text <- rbind(blogs, news, twitter)

#Store the dataset for fast retrieval
writeLines(text, "./data/text.txt", sep="\n")

#Load complete and cleaned dataset
text <- readLines("./data/text.txt")

#################################################
######### EXPLORATORY ANALYSIS  #################
#################################################

#Load libraries
library(stylo) #For n-grams
library(stringr) #For str_count
library(stringi) #For stri_count
library(ggplot2)

####### Dataset analysis ###########

#Blogs
length(blogs) #number of lines in the dataset
mean(stri_count(blogs,regex="\\S+")) #average amount of words per line
mean(nchar(blogs)) #average amount of characters per line

#News
length(news) #number of lines in the dataset
mean(stri_count(news,regex="\\S+")) #average amount of words per line
mean(nchar(news)) #average amount of characters per line

#Twitter
length(twitter) #number of lines in the dataset
mean(stri_count(twitter,regex="\\S+")) #average amount of words per line
mean(nchar(twitter)) #average amount of characters per line



####### Word and word combination analysis #########

#Creating a sample set of 1000 lines for exploratory word / word combination analysis
textsample <- text[sample(1:length(text), 1000, replace=FALSE)]
head(textsample)


#Analysing words and characters

#Words per line plot
g <- qplot(stri_count(textsample,regex="\\S+"), geom="histogram", binwidth = 1) 
g + xlab("Words") + ylab("Number of lines") + ggtitle("Words per line")

#Characters per line plot
g <- qplot(nchar(textsample), geom="histogram", binwidth = 1) 
g + xlab("Characters") + ylab("Number of lines") + ggtitle("Characters per line")


#Analysing unique words
textsamplewords = txt.to.words(textsample)
head(textsamplewords)

textsamplewordsunique <- unique(textsamplewords)
head(textsamplewordsunique)

textsamplewordscount <- sapply(textsamplewordsunique, function (x) {as.numeric(sum(str_count(textsample, paste(x, sep="") )))}  )
unique(textsamplewordscount)

textsamplewordsfreq <- data.frame(cbind(textsamplewordsunique, textsamplewordscount))
colnames(textsamplewordsfreq) <- c("word", "freq")

textsamplewordsfreq$freq <- as.numeric(textsamplewordsfreq$freq)

textsamplewordsfreq <- textsamplewordsfreq[order(-textsamplewordsfreq$freq),]

#The top 10 words in the sample dataset
head(textsamplewordsfreq, 10)


#Analysing 2 word combinations (2 ngram)
textsample2ngramunique <- unique(make.ngrams(textsamplewords, ngram.size = 2))
length(textsample2ngramunique)
head(textsample2ngramunique)

textsample2ngramcount <- sapply(textsample2ngramunique, function (x) {as.numeric(sum(str_count(textsample, paste("[^a-zA-Z]",x,"[^a-zA-Z]", sep="") )))}  )

textsample2ngramfreq <- data.frame(cbind(textsample2ngramunique, textsample2ngramcount))
colnames(textsample2ngramfreq) <- c("2 word combinations", "freq")


textsample2ngramfreq$freq <- as.numeric(textsample2ngramfreq$freq)

textsample2ngramfreq <- textsample2ngramfreq[order(-textsample2ngramfreq$freq),]

#The top 10 2-word combinations in the sample dataset
head(textsample2ngramfreq, 10)


#Analysing 3 word combinations (3 ngram)
textsample3ngramunique <- unique(make.ngrams(textsamplewords, ngram.size = 3))
length(textsample3ngramunique)
head(textsample3ngramunique)


textsample3ngramcount <- sapply(textsample3ngramunique, function (x) {as.numeric(sum(str_count(textsample, paste("[^a-zA-Z]",x,"[^a-zA-Z]", sep="") )))}  )

textsample3ngramfreq <- data.frame(cbind(textsample3ngramunique, textsample3ngramcount))
colnames(textsample3ngramfreq) <- c("3 word combinations", "freq")

textsample3ngramfreq$freq <- as.numeric(textsample3ngramfreq$freq)

textsample3ngramfreq <- textsample3ngramfreq[order(-textsample3ngramfreq$freq),]

#The top 10 3-word combinations in the sample dataset
head(textsample3ngramfreq, 10)


