my_hult <- c("Hult is a new kind of non-profit business school that constantly innovates to meet the needs of students, employers, and society in a world that is changing faster than ever before. More than a business school, Hult is a dynamic and multicultural community that educates, inspires, and connects some of the most forward-thinking business talent from around the world.")
my_hult_df <- data_frame(line=1, text=my_hult) #This is a very bad structure

#I'm creating a better structure
my_hult_struc <- my_hult_df %>%
                    unnest_tokens(word, text)

#Removing stopwords
my_hult_struc <- my_hult_df %>%
                    unnest_tokens(word, text) %>%
                    anti_join(stop_words)

#Token Frequencies
my_hult_struc <- my_hult_df %>%
                    unnest_tokens(word, text) %>%
                    anti_join(stop_words) %>%
                    count(word, sort=TRUE)

#Twitter Data 
#Note: read.csv("xxxx", stringsasfactor=FALSE)
library(readr)
#Education
edu <- read_csv("education.csv")
edu_twitter <- edu %>%
                  unnest_tokens(word, text) %>%
                  anti_join(stop_words) %>%
                  count(word, sort=TRUE)
View(edu_twitter)

#Banking
bank <- read_csv("banking.csv")
bank_twitter <- bank %>%
                  unnest_tokens(word, text) %>%
                  anti_join(stop_words) %>%
                  count(word, sort=TRUE)
View(bank_twitter)

#Alibaba Retail
alibaba <- read_csv("aliretail.csv")
alibaba_twitter <- alibaba %>%
                  unnest_tokens(word, text) %>%
                  anti_join(stop_words) %>%
                  count(word, sort=TRUE)
View(alibaba_twitter)

# Combining datasets
library(tidyr)
frequency <- bind_rows(mutate(edu_twitter, type="Education"),
                       mutate(bank_twitter, type= "Banking"),
                       mutate(alibaba_twitter, type="Alibaba")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>% # The words has to be a-z and + means it should have atleast one character
  count(type, word) %>%
  group_by(type) %>%
  mutate(proportion = n/sum(n))%>% #sum(n) by group_by(author)
  select(-n) %>% #Selecting percentage, proportion cannot be negative 
  spread(type, proportion) %>%
  gather(type, proportion, `Banking`, `Alibaba`) # Bronte and Well are compared to Jane Austen

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`Education`, 
                      color = abs(`Education`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~type, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Education", x=NULL)

#Correlation
cor.test(data=frequency[frequency$type == "Alibaba",],   #Bronte sista customers are closer relation to Jane Austen
         ~proportion + `Education`)

cor.test(data=frequency[frequency$type == "Banking",],
         ~proportion + `Education`)



                  
