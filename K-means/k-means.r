#Clustering and K-means

#Example of calculating Eucludian \distance between two points

#One guest who's written 5 computer science papers and 1 math paper, compared to
#another guest who has written 2 math papers and no computer science paper
sqrt((5 - 0)^2 + (1 - 2)^2)

#------------------------------------------------------------------------------

#Collecting, exploring and preparing data

#Read csv
teens <- read.csv("snsdata.csv", stringsAsFactors = TRUE)

#Examining the structure of data frame
str(teens)

#Exploring gender variable
table(teens$gender)

#Expanding search for missing data, if there is any
table(teens$gender, useNA = "ifany")

#Exploring share in age
summary(teens$age)

#Cleaning age variable by removing unreasonable ages in the distribution
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)

#Dealing with missing data

#Creating dummy variables for females and unknown gender
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

#Verifying dummy variables work as intended
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

#Finding mean age
mean(teens$age)

#Finding mean age while excluding missing data
mean(teens$age, na.rm = TRUE)

#Using aggregate function to find mean age of each year of graduation
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

#Using ave function to calculate mean age of every teen, excluding NA values
ave_age <- ave(teens$age, teens$gradyear, 
               FUN = function(x) mean(x, na.rm = TRUE))

#Imputing values from ave_age onto NA values
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

#Verifying results
summary(teens$age)

#Training model on data

#Creating data frame based only on interests
interests <- teens[5:40]

#Z-score standardising interest data using scale parameter
interests_z <- as.data.frame(lapply(interests, scale))

#Verifying standardisation is applied, using basketball variable
summary(interests$basketball)
summary(interests_z$basketball)

#Training k-means model on data
set.seed(12345)
teen_clusters <- kmeans(interests_z, 5)

#Evaluating model performance

#Examining he size of each cluster
teen_clusters$size

#Examining cluster centroids
teen_clusters$centers

#load library factoextra
library(factoextra)

#Visualising results of model clustering
fviz_cluster(teen_clusters, interests_z, geom = "point")

#Improving model performance

#Applying model cluster back to original data frame
teens$cluster <- teen_clusters$cluster

#Visualising how individual characteristic can relate to the clustering
teens[1:5, c("cluster", "gender", "age", "friends")]

#Exploring demographic characteristics of clustering
aggregate(data = teens, age ~ cluster, mean)

#Exploring gender share of clusters
aggregate(data = teens, female ~ cluster, mean)

#Exploring average amount of friends per cluster
aggregate(data = teens, friends ~ cluster, mean)
