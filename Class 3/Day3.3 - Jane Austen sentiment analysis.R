
###############################################################
### Sentiment analysis with Jane Austen's Work######
###############################################################

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(lienumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE))))%>%
  ungroup() %>%
  unnest_tokens(word, text)

nrcsurprise <- get_sentiments("nrc") %>%
                  filter(sentiment == "surprise") #what is your sentiment

#inner joining the emma book and the surprise sentiments
original_books %>%
  filter(book == "Emma") %>% #which book do you want to analyze
  inner_join(nrcsurprise) %>%
  count(word, sort=T)
  
########################################################
##### Comparing different sentiment libraries on JA ####
########################################################

emma_book <- original_books %>%
  filter(book == "Emma") #which book did you select?

afinn <- emma_book %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(index=lienumber %/% 80) %>% #using integer division to define larger sections of text
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  emma_book%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  emma_book %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index=lienumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts <- original_books %>%
  filter(book == "Emma") %>% #which book did you select?
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

