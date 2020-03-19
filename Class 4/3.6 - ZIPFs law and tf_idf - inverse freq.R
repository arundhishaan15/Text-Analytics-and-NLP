######################################################
####### TF-IDF framework in Jane Austen's work #######
######################################################

library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)
#let's look at the data
original_books <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort=TRUE) %>%
  ungroup()

total_words <- original_books %>%
                  group_by(book) %>%
                  summarize(total=sum(n))

book_words <- left_join(original_books, total_words)

print(book_words)
tail(book_words)

library(ggplot2)
ggplot(book_words, aes(n/total, fill = book))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~book, ncol=2, scales="free_y")   #100s of tokens appeared only once
#what do the tails represent? 
#answer: exremely common words! 
# we are really interested in the not so common words. 

######################################
########## ZIPF's law ################
######################################

freq_by_rank <- book_words %>%
                  group_by(book) %>%
                  mutate(rank = row_number(),
                         `term frequency` = n/total)
freq_by_rank

#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=book))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

###################################################
################# TF_IDF ##########################
###################################################

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words # we get all the zeors because we are looking at stop words ... too common

book_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=book))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~book, ncol=2, scales="free")+
  coord_flip()
