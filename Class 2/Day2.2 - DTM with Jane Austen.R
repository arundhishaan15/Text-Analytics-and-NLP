#####################################################
#### DTM object using Associated Press articles######
#####################################################

library(tm)
library(dplyr)
library(tidytext)
#install.packages("topicmodels")
data("AssociatedPress", package = "topicmodels")
AssociatedPress
#99% of the document-word pairs are zero
terms <- Terms(AssociatedPress)
terms

ap_td <- tidy(AssociatedPress)
ap_td

######################################################
#####Converting back from Tidy to DTM ###############
######################################################

ap_td %>%
  cast_dtm(document, term, count) #ID or type, word, n

######################################################
#####Putting the data in a sparse matrix ###############
######################################################

library(Matrix)
n <- ap_td %>%
      cast_sparse(document, term, count)
class(n)
dim(n)

######################################################
#####Converting Jane Austen to DTM ###############
######################################################
library(janeaustenr)
austen_dtm <- austen_books() %>%
                unnest_tokens(word, text) %>%
                count(book, word) %>%
                cast_dtm(book, word, n)

austen_dtm
