#Naive Bayes

#Exploring and preparing data

#Load csv file into environment
sms_raw <- read.csv("sms_spam.csv")

#Examine observations and variables using str function
str(sms_raw)

#Convert type variable into factor
sms_raw$type <- factor(sms_raw$type)

#Examine type factor using str and table functions
str(sms_raw$type)
table(sms_raw$type)

#Data preparation

#loading library tm
library(tm)

#Transforming text variable into corpus - collection of text documents
#using VCorpus function, VectorSource for transforming text attribute
#to source object
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

#Verifying corpus by printing information to console using print function
print(sms_corpus)

#Inspecting first two documents in corpus using inspect function
inspect(sms_corpus[1:2])

#Viewing a document content using as.character function
as.character(sms_corpus[[1]])

#Viewing multiple document contents using lapply function
lapply(sms_corpus[1:2], as.character)

#Standardising text to lowercase using tm_map, transforming text using
#content_transformation with tolower parameter
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

#Verifying transformation using as.character function
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

#Removing numbers from corpus using tm_map with removeNumbers parameter
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

#Removing stop words using tm_map, removeWords function with stopwords as
#parameter
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

#Removing punctuation using tm_map, removePunctuation as parameter
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

#removePunctuation example
removePunctuation("Hello, world")

#Custom punctuation removal function, replacing punctuation with spaces using
#regular expression pattern finding
replacePunctuation <- function(x) {
  gsub("[[:punct;]]+", " ", x)
}

#loading library SnowballC
library(SnowballC)

#Word stemming - transforms words to the base component (e.g. learned -> learn)
#example
wordStem(c("learn", "learning", "learned", "learns"))

#Word stemming document using tm_map, stemDocument as parameter
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

#Stripping additional whitespace using tm_map, stripWhitespace as parameter
sms_corpus_clean<- tm_map(sms_corpus_clean, stripWhitespace)
