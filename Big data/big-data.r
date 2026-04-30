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
