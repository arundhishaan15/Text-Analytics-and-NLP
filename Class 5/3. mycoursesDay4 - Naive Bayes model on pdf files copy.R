#install.packages("quanteda")#natural language processing package ?quanteda
#install.packages("RColorBrewer")
#install.packages("ggplot2")

library(quanteda)
library(RColorBrewer)
library(ggplot2)

#loading the pdf files:
# Importing all PDF files from the same folder
library(pdftools) # we need this library to use pdf_text
setwd("C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/pdf")
nm <- list.files(path="C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/pdf")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))
View(my_pdf_text)

# the readPDF function doesn't actually read the PDF files, 
#the read PDF creates a function to read in all the PDF files
Rpdf <- readPDF(control = list(text = "-layout"))
opinions <- Corpus(URISource(nm), 
                   readerControl = list(reader = Rpdf))
#we need to convert the VCorpus from the previous point to
#a regular corpus using the corpus() function.
msg.dfm <- dfm(corpus(opinions), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_count = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)

head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[1:5,]
msg.dfm.test<-msg.dfm[5:6,]

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, c(1,1,1,0,0))
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)
pred
