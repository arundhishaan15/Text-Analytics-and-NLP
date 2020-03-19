library(textreadr)
#Importing all .txt files from one directory # a txt works like a csv file with multiple rows
setwd("C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/txt")
nm <- list.files(path="C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/txt")
#using read document to import the data:
my_data <- read_document(file=nm[1]) #This comes out as a vector
my_data_together <- paste(my_data, collapse = " ") # This will give us a concatenated vector
my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))
View(my_txt_text)

#Importing all .doc files from one directory
#install.packages("textshape") #for some reason textreadr has issues getting textshape
#install.packages("textreadr")
library(textreadr)
setwd("C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/doc")
nm <- list.files(path="C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/doc")
my_doc_text <- do.call(rbind, lapply(nm, function(x) read_doc(file=x)))
View(my_doc_text)

#install.packages("pdftools")
# Importing all PDF files from the same folder
library(pdftools) # we need this library to use pdf_text
setwd("C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/pdf")
nm <- list.files(path="C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/pdf")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))
View(my_pdf_text)

#install.packages("magrittr")
#Scraping wesites from text
#install.packages("rvest")
library(magrittr)
library(rvest)
lego_movie <- html("http://www.imdb.com/title/tt1490017/")
lego_movie %>%
  html_node("strong span") %>%
  html_text()


###############################################################
######Querying Twitter for shares, like Trump tweets###########
###############################################################

#install the necessary packages
#install.packages("twitteR")
#install.packages("tm")

library("twitteR")
library("tm")

#necessary file for Windows
setwd("C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1")
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'dWnRxd7knSfcJ3vACjpCULBIz'
consumer_secret <- 'LBeHg5v3NbOB7YZ2Dn0hNy5qQpambr3WcZue1UJCMh8f3oBgsK'
access_token <- '2348166944-YPLBvS3KeMOmAw0GRMYF5Wz10z3vT79Csh3JewP'
access_secret <- 'uCgpH46o3f6oRdx5ENtGIdXM5NkWJc5WZhz2zC6tcyHnt'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
USA <- twitteR::searchTwitter('#USA + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(USA)
write.csv(d, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/econusa.csv')

EU <- twitteR::searchTwitter('#EU + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(EU)
write.csv(e, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/econeu.csv')

EDU <- twitteR::searchTwitter('#Education + #EdTech', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
f = twitteR::twListToDF(EDU)
write.csv(f, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/education.csv')

BANK <- twitteR::searchTwitter('#Banking + #Investment', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
g = twitteR::twListToDF(BANK)
write.csv(g, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/banking.csv')

News <- twitteR::searchTwitter('#News', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
h = twitteR::twListToDF(News)
write.csv(h, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/news.csv')

STARTUP <- twitteR::searchTwitter('#SanFrancisco + #Startup', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
i = twitteR::twListToDF(STARTUP)
write.csv(i, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/startup.csv')
