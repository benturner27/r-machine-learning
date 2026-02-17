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

#Standardising text data

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

#Stripping additional white space using tm_map, stripWhitespace as parameter
sms_corpus_clean<- tm_map(sms_corpus_clean, stripWhitespace)

#final verification that corpus is cleaned accordingly
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

#Document to word splitting

#Creating DocumentTermMatrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

#Alternative to cleaning text data using DocumentTermMatrix with parameters
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

#Comparing the two cleaned datasets
sms_dtm
sms_dtm2

#Creating custom function for stopword removal to use with DTM
stopwords <- function(x){ removeWords(x, stopwords())}

#Creating training and testing datasets

#splitting DTM into training and testing sets
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm2[4170:5559, ]

#creating sets with labels for model evaluation
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

#verifying labelled sets are representative of complete set
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#Text data visualisation

#Loading library wordcloud
library(wordcloud)

#Creating wordcloud from cleaned corpus data
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

#Creating ham and spam subsets for separate wordclouds
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

#Creating separate wordclouds for ham and spam types
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

#Frequent word indicators

#Using findFreqTerms to find popular words used in dataset
findFreqTerms(sms_dtm_train, 5)

#Saving function as vector
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)

#using str function to see amount of frequent words
str(sms_freq_words)

#filtering only frequent words into training and testing sets
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

#Creating custom function converting numerical counts to categorical count
convert_counts <- function(x) {
  x <- ifelse(x > 0, "yes", "no")
}

#Apply convert_counts function to training and testing sets using apply function
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)
