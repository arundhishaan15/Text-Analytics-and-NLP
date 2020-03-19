library(textreadr)
MBA <- read_document(file="xxxxxxxxxxxx")
MIB <- read_document(file="xxxxxxxxxxx")
class_combo <- c(MBA, MIB)

a <- ?? #how many observations to you have
b <- ?? #how many variables do you have

my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- class_combo[i*b+z-b]
  }#closing z loop
}#closing i loop

my_txt <- my_df$V6
my_txt <- substr(my_txt, start=11 , stop = 10000)

library(dplyr)
mydf <- data_frame(line=1:a, text=my_txt)
print(mydf)

##############################################################################################################################

interview <- read.csv("C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 4/tiktok.csv")
interview$Question.1 <- as.character(interview$Question.1)
interview$Question.2 <- as.character(interview$Question.2)
interview$Question.3 <- as.character(interview$Question.3)
interview$Question.4 <- as.character(interview$Question.4)
interview$Question.5 <- as.character(interview$Question.5)
str(interview)
library(dplyr)
library(stringr)
library(tidytext)

data(stop_words)

# Question 1: What kind of videos do you watch and why?

frequencies_tokens_nostop1 <- interview %>%
  unnest_tokens(word, Question.1) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)

print(frequencies_tokens_nostop1) # This is Tidy Format

# Question 2: What kind of social media apps are you using and why?

frequencies_tokens_nostop2 <- interview %>%
  unnest_tokens(word, Question.2) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)

print(frequencies_tokens_nostop2)

# Question 3: About how many hours do you spend on social media and which days?

frequencies_tokens_nostop3 <- interview %>%
  unnest_tokens(word, Question.3) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)

print(frequencies_tokens_nostop3)

# Question 4: What hashtags have you used the most?

frequencies_tokens_nostop4 <- interview %>%
  unnest_tokens(word, Question.4) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)

print(frequencies_tokens_nostop4)

# Question 5: Will you use TikTok?

frequencies_tokens_nostop5 <- interview %>%
  unnest_tokens(word, Question.5) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)

print(frequencies_tokens_nostop5)

# Tokenized Twitter Data

library(twitteR)
library(tm)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'dWnRxd7knSfcJ3vACjpCULBIz'
consumer_secret <- 'LBeHg5v3NbOB7YZ2Dn0hNy5qQpambr3WcZue1UJCMh8f3oBgsK'
access_token <- '2348166944-YPLBvS3KeMOmAw0GRMYF5Wz10z3vT79Csh3JewP'
access_secret <- 'uCgpH46o3f6oRdx5ENtGIdXM5NkWJc5WZhz2zC6tcyHnt'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

EDU <- twitteR::searchTwitter('#Education + #EdTech', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
f = twitteR::twListToDF(EDU)
write.csv(f, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 4/education.csv')

BANK <- twitteR::searchTwitter('#Banking + #Investment', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
g = twitteR::twListToDF(BANK)
write.csv(g, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 4/banking.csv')

News <- twitteR::searchTwitter('#News', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
h = twitteR::twListToDF(News)
write.csv(h, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 4/news.csv')

#this is where you tokenize all 3 twitter datasets
tidy_EDU <- f %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

tidy_BANK <- g %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

tidy_News <- h %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  count(word, sort=TRUE)

###############################################################################################################
library(wordcloud)
data("stop_words")

tidy_EDU %>%
  with(wordcloud(word, n, max.words = 100))

tidy_BANK %>%
  with(wordcloud(word, n, max.words = 100))

tidy_News %>%
  with(wordcloud(word, n, max.words = 100))

###################################################
#### Adding positive and negative sentiments ######
###################################################
#install.packages(("reshape2"))
library(reshape2)
tidy_EDU %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.5,0.5), 
                   fixed.asp = TRUE,
                   title.size = 1)

tidy_BANK %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.5,0.5), 
                   fixed.asp = TRUE,
                   title.size = 1)

tidy_News %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.5,0.5), 
                   fixed.asp = TRUE,
                   title.size = 1)

####################################################################################

####  TikTok Interview Wordclouds  ###############

####################################################################################

# Question 1: What kind of videos do you watch and why?

frequencies_tokens_nostop1 <- interview %>%
  unnest_tokens(word, Question.1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

frequencies_tokens_nostop1 #look at trump - he is positive!!! :)

frequencies_tokens_nostop1 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

library(reshape2)
#we need to use the NRC sentiments
frequencies_tokens_nostop1 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

# Question 2: What kind of social media apps are you using and why?

frequencies_tokens_nostop2 <- interview %>%
  unnest_tokens(word, Question.2) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

frequencies_tokens_nostop2 #look at trump - he is positive!!! :)

frequencies_tokens_nostop2 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

library(reshape2)
#we need to use the NRC sentiments
frequencies_tokens_nostop2 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

# Question 3: About how many hours do you spend on social media and which days?

frequencies_tokens_nostop3 <- interview %>%
  unnest_tokens(word, Question.3) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

frequencies_tokens_nostop3 #look at trump - he is positive!!! :)

frequencies_tokens_nostop3 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

library(reshape2)
#we need to use the NRC sentiments
frequencies_tokens_nostop3 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

# Question 4: What hashtags have you used the most?

frequencies_tokens_nostop4 <- interview %>%
  unnest_tokens(word, Question.4) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

frequencies_tokens_nostop4 #look at trump - he is positive!!! :)

frequencies_tokens_nostop4 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

library(reshape2)
#we need to use the NRC sentiments
frequencies_tokens_nostop4 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

# Question 5: Will you use TikTok?

frequencies_tokens_nostop5 <- interview %>%
  unnest_tokens(word, Question.5) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

frequencies_tokens_nostop5 #look at trump - he is positive!!! :)

frequencies_tokens_nostop5 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

library(reshape2)
#we need to use the NRC sentiments
frequencies_tokens_nostop5 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

#################################################################################

############ tf-idf ################

#################################################################################

my_df <- bind_rows(
  mutate(frequencies_tokens_nostop1, question = "first"),
  mutate(frequencies_tokens_nostop2, question = "second"),
  mutate(frequencies_tokens_nostop3, question = "third"),
  mutate(frequencies_tokens_nostop4, question = "fourth"),
  mutate(frequencies_tokens_nostop5, question = "fifth")
)
View(my_df)
my_df <- my_df %>%
  bind_tf_idf(word, question, n)

my_df %>%
  arrange(desc(tf_idf))

my_df %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%
  top_n(20) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()

#############################33

# Bigrams - Negation




