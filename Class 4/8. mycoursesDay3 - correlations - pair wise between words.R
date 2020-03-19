################################################
###Pairwise correlations between words #########
################################################

#install.packages("widyr")
library(widyr)
library(janeaustenr)
library(tidyr)
library(dplyr)
my_tidy_df <- austen_books() %>%
  filter(book == "xxxxxxxx") %>% #what book do you want to use? Emma?
  mutate(section = row_number() %/% 80) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

my_tidy_df
#taking out the least common words
word_cors <- my_tidy_df %>%
  group_by(word) %>%
  filter(n() >= xxxxxxxxxx) %>% #what is the minimum frequency you want to use
  pairwise_cor(word,section, sort=TRUE)
#pairwise_cor() check correlation based on how ofter words appear in the same section

word_cors %>%
  filter(item1 == "jane")
########################################################
####### creating barcharts for correlatoins ############
########################################################

word_cors %>%
  filter(item1 %in% c(xxxxxxxxxxxxxxxx)) %>% #which words do you want to use?
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

########################################################
####### creating a correlation network #################
########################################################

#this will take some time to run, we will need to wait for the result
# feel free to adjust the geom_node_point to somehting smaller

word_cors %>%
  filter(correlation >xxxxxxxx) %>% #what correlation cutoff do you want to use
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "xxxxxxxxxxxxx", size=xxxxxxxx)+ #what color do you want to use?
  geom_node_text(aes(label=name), repel=T)+
  theme_void()
