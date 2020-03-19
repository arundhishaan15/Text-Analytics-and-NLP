#########################################
### Example with Jane Austen's Work######
#########################################
#install.packages("janeaustenr")

library(janeaustenr)
library(dplyr)
library(stringr)
#let's look at the data
original_books <- austen_books()
View(original_books[1:40,])
unique(original_books$book)
##############################
##############################
original_books <- austen_books() %>%
                        group_by(book) %>%
                        mutate(linenumber = row_number(),
                               chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                                       ignore_case = TRUE))))%>%
                        ungroup()
original_books

#We need one token per row , 
#so that the structure is simialar from our bacon tokenizing script.

library(tidytext)
tidy_books <- original_books %>%
                  unnest_tokens(word, text)
print(tidy_books)

#removing stop words
data(stop_words)
tidy_janeausten_no_stop <- tidy_books %>%
                        anti_join(stop_words)

#printing the count frequencies for each token without stop words
tidy_janeausten_no_stop %>%
  count(word, sort=TRUE)

#plotting the token frequencies:
library(ggplot2)
freq_hist <-tidy_janeausten_no_stop %>%
  count(word, sort=TRUE) %>%
  filter(n > 600) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)
