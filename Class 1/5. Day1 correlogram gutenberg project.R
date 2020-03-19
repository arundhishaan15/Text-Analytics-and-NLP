############
#Take a look at project Gutenberg
#http://www.gutenberg.org/wiki/Main_Page
###########
#We will use these 4 books by Wells:
# The Time machine ID:35
# The war of the worlds ID:36
#The invisible man ID:5230
# The island of Doctor Moreau ID: 159
############################################
#install.packages("gutenbergr")
data(stop_words)
library(gutenbergr)
mydata <- gutenberg_download(c(35,36,5230, 159))
tidy_mydf <- mydata %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words)
print(tidy_mydf)
#counting frequencies for tokens
tidy_mydf %>%
  count(word, sort=TRUE)
  
###########
#We will use another set of 5 books by Bronte Sisters:
# Jane Eyre ID:1260
# Wuthering Heights ID:768
# The Tenant of Wildfell Hall ID:969
# Villette ID:9182
# Agnes Grey ID: 767

data(stop_words)
library(gutenbergr)
bronte_sisters <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte_sisters %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_bronte)
#counting frequencies for tokens
tidy_bronte %>%
  count(word, sort=TRUE)

#############################################
####We want to combine all the datasets and do frequencies 
#############################################
library(tidyr)
frequency <- bind_rows(mutate(tidy_mydf, author="Wells"),
                       mutate(tidy_bronte, author= "Bronte Sista"),
                       mutate(tidy_janeausten_no_stop, author="Jane Austen")
                       )%>%#closing bind_rows
                mutate(word=str_extract(word, "[a-z']+")) %>% # The words has to be a-z and + means it should have atleast one character
                count(author, word) %>%
                group_by(author) %>%
                mutate(proportion = n/sum(n))%>% #sum(n) by group_by(author)
                select(-n) %>% #Selecting percentage, proportion cannot be negative 
                spread(author, proportion) %>%
                gather(author, proportion, `Bronte Sista`, `Wells`) # Bronte and Well are compared to Jane Austen

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`Jane Austen`, 
                      color = abs(`Jane Austen`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Jane Austen", x=NULL)

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=frequency[frequency$author == "Bronte Sista",],   #Bronte sista customers are closer relation to Jane Austen
         ~proportion + `Jane Austen`)

cor.test(data=frequency[frequency$author == "Wells",],
         ~proportion + `Jane Austen`)

