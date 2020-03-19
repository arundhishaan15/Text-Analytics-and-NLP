#####################################################
################## Corpus object ####################
#####################################################
library(tm)
data("acq")#50 articles from Reuters


#We want to convert this to a tidy format that has 
#one row per document

acq_tidy <- tidy(acq)


acq_tokens <- acq_tidy %>%
                select(-places) %>%
                unnest_tokens(word, text)
               

#####################################################
######### Corpus object with PDF files###############
#####################################################

#Import the PDF files from my courses
# we need this library to use pdf_text
library(pdftools)
library(tm)
setwd("C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/pdf") #where are your PDF files
nm <- list.files(path="C:/Users/arund/OneDrive/Desktop/Masters in Business Analytics/Text Analytics/Class 1/pdf")#were are your PDF files stored?

# the readPDF function doesn't actually read the PDF files, 
#the read PDF creates a function to read in all the PDF files
Rpdf <- readPDF(control = list(text = "-layout"))
opinions <- Corpus(URISource(nm), 
                   readerControl = list(reader = Rpdf))

opinions # I want to see the VCoprus content
opinions[[1]] # list requires double brackets
opinions[[1]]$meta$language
opinions[[1]]$meta$datetimestamp
