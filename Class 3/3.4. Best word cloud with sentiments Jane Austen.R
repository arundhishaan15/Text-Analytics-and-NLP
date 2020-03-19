##########################################
#### Creating sentiment wordclouds #######
##########################################
#install.packages("wordcloud")
library(wordcloud)
data("stop_words")

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(lienumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE))))%>%
  ungroup() %>%
  unnest_tokens(word, text)%>%
  filter(book == "Emma") %>%
  anti_join(stop_words) %>%
  count(word, sort=T)

original_books %>%
  with(wordcloud(word, n, max.words = 100))

###################################################
#### Adding positive and negative sentiments ######
###################################################
#install.packages(("reshape2"))
library(reshape2)
original_books %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(color = rev(RColorBrewer::brewer.pal(10, "RdBu")),
                   max.words=100, scale = c(0.5,0.5), 
                   fixed.asp = TRUE,
                   title.size = 1)

original_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("green","grey20"),
                   max.words=100, scale = c(0.5,0.5), 
                   fixed.asp = TRUE,
                   title.size = 1)

original_books %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, scale = c(0.5,0.5), 
                   fixed.asp = TRUE,
                   title.size = 1)

# Best clouds
library(reshape2)
original_books %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100, fixed.asp=TRUE, scale=c(0.6,0.6), 
                   title.size=1, rot.per=0.25)
