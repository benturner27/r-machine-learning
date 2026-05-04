#Big Data

#Loading libraries and dependencies needed for image classification
library(devtools)
devtools::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow()
devtools::install_github("rstudio/keras")
library(keras3)

#Loading ResNet-50 model for image classification
model <- application_resnet50(weights = "imagenet")

#loading image into envrionment for model to classify
img <- image_load("ice_cream.jpg", target_size = c(224, 224))

#Converting image into 3-D tensor
x <- image_to_array(img)

#Examining the 3-D tensor
dim(x)
str(x)

#Retrieving colour values from image coordinates
x[1, 224, 1:3]
x[40, 145, 1:3]

#Converting to 4-D tensor for the model to read
x <- array_reshape(x, c(1, dim(x)))
dim(x)

#Normalising colour values to match what the model saw in ImageNet
x <- imagenet_preprocess_input(x)

#verifying preprocessing has worked, exploring change in values
x[1, 40, 145, 1:3]

#Running the image on Rest-50 model to predict what it is, as well as decoding
#probabilites
p_resnet50 <- predict(model, x)
c_resnet50 <- imagenet_decode_predictions(p_resnet50, top = 10)
c_resnet50

#Processing the other two image before being fed to the model
img_list <- list("cat.jpg", "pizza.jpg")
img_data <- lapply(img_list, image_load, target_size = c(224, 224))
img_arrs <- lapply(img_data, image_to_array)
img_resh <- lapply(img_arrs, array_reshape, c(1, 224, 224, 3))
img_prep <- lapply(img_resh, imagenet_preprocess_input)
img_prob <- lapply(img_prep, predict, object = model)


#Decoding the probabilities from the list
img_classes <- sapply(img_prob, imagenet_decode_predictions, top = 3)
img_classes

#Understanding word embeddings with word2vec

#Loading library and model. Model was downloaded from
#www.kaggle.com/datasets/pandey881062/googlenews-vectors-negative300-bin-gz
library(word2vec)
m_w2v <- read.word2vec(
  file = "GoogleNews-vectors-negative300.bin",
  normalize = TRUE
  )

#Verifying that the model has been loaded properly
str(m_w2v)

#Requesting vectors for words associated to a couple terms
foods <- predict(m_w2v, c("cereal", "bacon", "eggs",
                          "sandwich", "salad", "steak", "spaghetti"),
                 type = "embedding")
meals <- predict(m_w2v, c("breakfast", "lunch", "dinner"),
                 type = "embedding")

#Examining the results of the predictions
head(foods["cereal", ])
foods[, 1:5]

#Exploring relationship between the two predictions made by the model
word2vec_similarity(foods, meals)

#Using the model on real-world examples, in this case, for advertisements
user_posts = c(
  "I eat bacon and eggs in the morning for the most important meal of the day!",
  "I am going to grab a quick sandwich this afternoon before hitting the gym.",
  "Can anyone provide restaurant recommendations for my date tonight?"
)

#Processing user_posts to be used on word2vec model
library(tm)
user_posts_clean <- removeWords(user_posts, stopwords())
user_posts_clean <- txt_clean_word2vec(user_posts_clean)

#Verifying that processing has been impemented properly
user_posts_clean[1]

#Training word2vec on user_posts to for predictions on meal similarity
post_vectors <- doc2vec(m_w2v, user_posts_clean)

#Exploring the predictions made by word2vec
str(post_vectors)

#Exploring the similarity between user_posts_clean and meals vectors
word2vec_similarity(post_vectors, meals)

#PCA for Big Data and its limitations

#Finding 2 PCA components from the dataset
library(tidyverse)
sns_terms <- read_csv("snsdata.csv") |>
  select(basketball:drugs)
library(irlba)
set.seed(123456)
sns_pca <- sns_terms |>
  prcomp_irlba(n = 2, center = TRUE, scale = TRUE)

#Creating a scatterplot to to determine the relationship between the 2 PCAs
library(ggplot2)
as.data.frame(sns_pca$x) |>
  ggplot(aes(PC1, PC2)) + geom_point(size = 1, shape = 1)



