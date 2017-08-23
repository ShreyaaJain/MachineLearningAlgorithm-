install.packages("RCurl")
install.packages("ggmap")
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(wordcloud)
library(reshape2)

setwd("/Users/sjain/Downloads")
commentsData <- read.csv(file = "review.csv")

commentsData$Body = commentsData$Body[!is.na(commentsData$Body)]
corpus <- Corpus(VectorSource(commentsData$Body))
corpus$content[1]

tweet <- commentsData$Body
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
head(corpus)
dtm <- DocumentTermMatrix(corpus)
col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

positiveWords = readLines("positive_words.txt")

negativeWords = readLines("negative_words.txt")

tweets<- tweet
rm(sentiment_scores)
sentiment_scores <- function(tweets, positives, negatives, .progress){
  scores = laply(tweets,
                 function(tweet, positives, negatives){
                   tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
                   tweet = gsub("[[:cntrl:]]", "", tweet)   # remove control characters
                   tweet = gsub('\\d+', '', tweet)          # remove digits
                   tryTolower <- function(x){
                     # create missing value
                     y <- NA
                     # tryCatch error
                     try_error <- tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y <- tolower(x)
                     # result
                     return(y)
                   }
                   # using tryTolower with sapply
                   tweet <- sapply(tweet, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word_list <- str_split(tweet, "\\s+")
                   words <- unlist(word_list)
                   words
                   # compare words to the dictionaries of positive & negative terms
                   positive_matches <- match(words, positiveWords)
                   negative_matches <- match(words, negativeWords)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   
                   positive_matches <- !is.na(positive_matches)
                   negative_matches <- !is.na(negative_matches)
                   
                   total <- sum(positive_matches) + sum(negative_matches)
                   
                   positivePercent <- (sum(positive_matches) / total) * 100
                   
                   negativePercent <- (sum(negative_matches) / total) * 100
                  
                   barplot(c(positivePercent,negativePercent),beside = TRUE, col = c("green","red")
                           ,legend.text = c("Positive","Negative"),  
                           ylim = c(0,100), xlab = 'Topics(Z)',ylab = 'P(z)', sub = substr(tweet,1,60))
                   score <- c(  positivePercent,  negativePercent)
                   return(score)
                 }, positives, negatives, .progress='text' )
  return(scores)
  
}
library(lattice)

score1 <- sentiment_scores(tweet, positives, negatives, .progress='text')
head(score1)
write.csv(score1,file='assignment8.csv',row.names = FALSE)
apply(score1,2,mean)
barplot(apply(score1,2,mean),beside = TRUE, col = c("green","red")
        ,legend.text = c("Positive","Negative"),  
        ylim = c(0,100), xlab = 'Topics(Z)',ylab = 'P(z)', sub = 'Overall Sentiment')



