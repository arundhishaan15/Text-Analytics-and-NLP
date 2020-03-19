#############################################
###### N-grams and tokenizing ###############
#############################################

library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)

my_bigrams <- austen_books() %>%
                    unnest_tokens(bigram, text, token = "ngrams", n=2)

my_bigrams #We want to see the bigrams (words that appear together, "pairs")

my_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- my_bigrams %>%
                        separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
                  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts
 
###########################################################
###### What if we are interested in the most common #######
################ 4 consecutive words - quadro-gram ########
###########################################################
quadrogram <- austen_books() %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

##### lets do the same for a quadrogram

quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  count(book, quadrogram) %>%
  bind_tf_idf(quadrogram, book, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf

######################################################
######## visualising negated words ###################
###### negated words in sentiment analysis ###########
######################################################

negation_tokens <- c("no", "never", "without", "not")#what negation tokens do you want to use?

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()
  
negated_words

#################################################
#### we can visuals the negated words ###########
#we'll create a function to plot the negations###
#################################################
negated_words_plot <- function(x){
  negated_words %>%
    filter(word1 == x) %>%
    mutate(contribution = n* value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*value, fill = n*value >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment value* number of occurences")+
      coord_flip()
}#closing the negated_words_plot function

negated_words_plot(x="not") #this is your first negation word
negated_words_plot(x="no") #this is your second negation word
negated_words_plot(x="without") #this is your third negation word

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
library(igraph)
bigram_graph <- bigram_counts %>%
                  filter(n>20) %>%  # Lower this based on model say >1
                  graph_from_data_frame()

bigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)
