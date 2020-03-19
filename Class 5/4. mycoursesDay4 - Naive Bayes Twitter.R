#install.packages("quanteda")#natural language processing package ?quanteda
#install.packages("RColorBrewer")
#install.packages("ggplot2")

library(quanteda)
library(RColorBrewer)
library(ggplot2)

library(twitteR)
library(tm)

#necessary file for Windows
setwd("xxxxxxxxxxxxxxxx")
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'xxxxxxxxxxxxx'
consumer_secret <- 'xxxxxxxxxxxx'
access_token <- 'xxxxxxxxxxxxxxx'
access_secret <- 'xxxxxxxxxxxxxxx'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
USA <- twitteR::searchTwitter('#USA + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(USA)
d$binary <- rep(c("1"), each=nrow(d))

EU <- twitteR::searchTwitter('#EU + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(EU)
e$binary <- rep(c("0"), each=nrow(e))

total_twitter_df <- rbind(d,e)[,c("text", "binary")]
#we need to convert the VCorpus from the previous point to
#a regular corpus using the corpus() function.
twitter_corpus <- corpus(total_twitter_df$text) #creating the corpus on the $text var
msg.dfm <- dfm(twitter_corpus, tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 3, min_docfreq = 0)
msg.dfm <- dfm_weight(msg.dfm, type = "tfidf")

head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[1:xxxxxxxxxxxx,]
msg.dfm.test<-msg.dfm[xxxxxxxxx:xxxxxxxxxxx,]

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, total_twitter_df$binary[1:xxxxxxxxx]) # we need to tell which 1 and 0 to use
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)
pred
#which of the following are false predictions? they should be all 0!