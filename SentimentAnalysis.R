install.packages("syuzhet")
library(syuzhet)
install.packages("tm")
library(tm)
install.packages("wordcloud")
library(wordcloud)
install.packages("wordcloud2")
library(wordcloud2)
install.packages("lubridate")
library(lubridate)
install.packages("scales")

library(reshape2)
library(dplyr)

### Import Data
tweets <- read.csv(file.choose(), header = T)
str(tweets)

# Only the tweets from Trump
tweets <- tweets[!(tweets$handle=="HillaryClinton"),]
summary(tweets$handle)

# Build corpus
library(tm) #text mining
corpus <- iconv(tweets$text, to = "utf-8-mac")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean text
corpus <- tm_map(corpus, tolower) # all lowercase
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english')) # filler words like "the" "and"
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x) #takes out the link at end of tweets
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Bar plot
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

# Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

# Sentiment analysis for Trump - could do the same for Hillary and compare sentiments
tweets2 <- read.csv(file.choose(), header = T)
tweets2 <- tweets2[!(tweets2$handle=="HillaryClinton"),]
trump <- iconv(tweets2$text, to = 'utf-8-mac')

# Obtain sentiment scores
s <- get_nrc_sentiment(trump)
head(s)
trump[5]
get_nrc_sentiment('debate')

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Trump Tweets')

### Now Sentiment for Hillary
tweets3 <- read.csv(file.choose(), header = T)
tweets3 <- tweets3[!(tweets3$handle=="realDonaldTrump"),]
clinton <- iconv(tweets3$text, to = 'utf-8-mac')

# Obtain sentiment scores
s2 <- get_nrc_sentiment(clinton)
head(s2)

# Bar plot
barplot(colSums(s2),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Clinton Tweets')
