
library(twitteR)
library(tm)
library(dplyr)
library(tidyverse)
library(tidytext)
library(stringr)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions

#necessary file for Windows
setwd("C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1")
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'dWnRxd7knSfcJ3vACjpCULBIz'
consumer_secret <- 'LBeHg5v3NbOB7YZ2Dn0hNy5qQpambr3WcZue1UJCMh8f3oBgsK'
access_token <- '2348166944-YPLBvS3KeMOmAw0GRMYF5Wz10z3vT79Csh3JewP'
access_secret <- 'uCgpH46o3f6oRdx5ENtGIdXM5NkWJc5WZhz2zC6tcyHnt'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


EDU <- twitteR::searchTwitter('#Education + #EdTech', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
f = twitteR::twListToDF(EDU)
write.csv(f, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/education.csv')

BANK <- twitteR::searchTwitter('#Banking + #Investment', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
g = twitteR::twListToDF(BANK)
write.csv(g, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/banking.csv')

News <- twitteR::searchTwitter('#News', n = 1000, since = '2015-06-01', retryOnRateLimit = 1e3)
h = twitteR::twListToDF(News)
write.csv(h, file='C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/news.csv')

#this is where you tokenize all 3 twitter datasets
tidy_EDU <- f %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_BANK <- g %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_News <- h %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#################################################
### Combining all 3 tidy data frames ############
#################################################

library(tidyr)
frequency <- bind_rows(mutate(tidy_EDU, author="EDU"),
                       mutate(tidy_BANK, author= "BANK"),
                       mutate(tidy_News, author="News")
                        )%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `EDU`, `BANK`)

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`News`, 
                      color = abs(`News`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "News", x=NULL)


cor.test(data=frequency[frequency$author == "EDU",],
         ~proportion + `News`)

cor.test(data=frequency[frequency$author == "BANK",],
         ~proportion + `News`)

# Custom Stopwords
print(stop_words)
custom_stop <- data_frame(word = c("http","rt","https","t.io"),lexicon =rep("custom", each=4))
custom_stopwords <- bind_rows(stop_words,custom_stop)

# Add custom stopwords to the pipes
tidy_EDU <- f %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop)%>%


tidy_BANK <- g %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop)

tidy_News <- h %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop)

# Create DTM 

DTM_EDU <- f %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop)%>%
  count(word, sort = T)%>%
  cast_dtm(id, word, n)

  
DTM_BANK <- g %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop)%>%
  count(word, sort = T)%>%
  cast_dtm(id, word, n)

DTM_News <- h %>%
  group_by(id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop)%>%
  count(word, sort = T)%>%
  cast_dtm(id, word, n)

# Sentiment Analysis

tidy_EDU <- f %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sum(value))
  
tidy_BANK <- g %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))


tidy_News <- h %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sum(value))
