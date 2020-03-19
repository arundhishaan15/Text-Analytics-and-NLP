library("twitteR")
library("tm")
library(dplyr)
library(tidyr)
library(tidytext)
library(tidyverse)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'xxxxxx'
consumer_secret <- 'xxxxxx'
access_token <- 'xxxxxxx'
access_secret <- 'xxxxxxx'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
USA <- twitteR::searchTwitter('#USA + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
d = twitteR::twListToDF(USA)

EU <- twitteR::searchTwitter('#EU + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
e = twitteR::twListToDF(EU)

ASIA <- twitteR::searchTwitter('#Asia + #Economy', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
a = twitteR::twListToDF(ASIA)

print(stop_words)
cust_stop <- data_frame(word=c("http", "https", "rt", "t.co"),
                        lexicon=rep("cust", each=4)
)


tidy_usa <- d %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word)%>%
  cast_dtm(id, word, n)
#do the same for the other 2 data frames - live coding:
tidy_eu <- e %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_asia <- a %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#####################################################
### Running LDA per documnet
######################################################
usa_lda <- LDA(tidy_usa,k=2, control = list(seed=123))
usa_lda
usa_gamma <- tidy(usa_lda, matrix="gamma")
usa_gamma 

##########################################################
### Running LDA per token
##########################################################
library(tidytext)
usa_topics <- tidy(usa_lda, matrix="beta")
usa_topics
library(ggplot2)
library(dplyr)

top_terms <- usa_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

######################################################
#### Let's run LDA on USA and EU combined together
######################################################
tidy_usa <- d %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) %>%
  count(word)

eu_tidy <- e %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) %>%
  count(word)

combined <- rbind(eu_tidy, tidy_usa)

combined_dtm <- combined %>%
  cast_dtm(id, word, n)

combined_lda <- LDA(combined_dtm ,k=2, control = list(seed=123))
combined_lda

library(tidytext)
combined_topics <- tidy(combined_lda, matrix="beta")
combined_topics
library(ggplot2)
library(dplyr)
#lets view top terms
top_terms <- combined_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
