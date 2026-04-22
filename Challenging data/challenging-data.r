#Challenging Data

#Using stepwise regression for feature selection

#Preparing dataset for stepwise regression process
library(tidyverse)
titanic_train <- read_csv("titanic_train.csv") |>
  mutate(
    Age_MVI = if_else(is.na(Age), 1, 0),
    Age = if_else(is.na(Age), mean(Age, na.rm = TRUE), Age),
    Cabin = if_else(is.na(Cabin), "X", Cabin),
    Embarked = factor(if_else(is.na(Embarked), "X", Embarked)),
    Sex = factor(Sex)
  )

#Defining the minimal model needed for the stepwise regression process
simple_model <- glm(Survived ~ 1, family = "binomial",
                    data = titanic_train)

#Defining the full model for the stepwise regression process
full_model <- glm(Survived ~ Age + Age_MVI + Embarked +
                    Sex + Pclass + SibSp + Fare,
                  family = "binomial", data = titanic_train)

#Implementing the stepwise regression model for determining best variable
sw_forward <- stats::step(simple_model,
                          scope = formula(full_model),
                          direction = "forward")

#Acquiring formula for final model and beta coefficients
formula(sw_forward)
sw_forward$coefficients

#Implementing a backwards regression model for removing the worst variables
sw_backward <- stats::step(full_model, direction = "backward")

#Using Boruta for feature selection

#Adding a random values feature to the dataset
set.seed(12345)
titanic_train$rand_vals <- runif(n = 891, min = 1, max = 100)

#Implementing Boruta to find the most important variables
library(Boruta)
titanic_boruta <- Boruta(Survived ~ PassengerId + Age +
                           Sex + Pclass + SibSp + rand_vals,
                         data = titanic_train, doTrace = 1)

#Exploring the results of the Boruta model
titanic_boruta

#Plotting the results of feature selection from Boruta model
plot(titanic_boruta)

#PCA example using social media data

#Read file to environment
sns_data <- read_csv("snsdata.csv")

#Selecting features from the data frame into a new tibble
sns_terms <- sns_data |> select(basketball:drugs)

#Building the PCA selection model using irlba
set.seed(2023)
library(irlba)
sns_pca <- sns_terms |>
  prcomp_irlba(n = 10, center = TRUE, scale = TRUE)

#Creating a scree plot to visualise 10 feature with the highest variance
screeplot(sns_pca, npcs = 10, type = "lines",
          main = "Scree Plot of SNS Data Principal components")

#exploring the results of the PCA
summary(sns_pca)
str(sns_pca$x)
head(sns_pca$x)

#Creating a tibble using the social media terms mixed with the PCA rotation data
sns_pca_long <- tibble(SNS_Term = colnames(sns_terms),
                       as_tibble(sns_pca$rotation)) |>
  pivot_longer(PC1:PC10, names_to = "PC", values_to = "Contribution")

#Visualising the most important contributing terms to PC3
sns_pca_long |>
  filter(PC =="PC3") |>
  top_n(15, abs(Contribution)) |>
  mutate(SNS_Term = reorder(SNS_Term, Contribution)) |>
  ggplot(aes(SNS_Term, Contribution, fill = SNS_Term)) +
    geom_col(show.legend = FALSE, alpha = 0.8) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,
          vjust = 0.5), axis.ticks.x = element_blank()) +
    labs(x = "Social Media Term",
         y = "Relative importance to principal component",
         title = "Top 15 Contributors to PC3")

#Repeating the visualisation for the top 5 prinicpal components
sns_pca_long |>
  filter(PC =="PC1") |>
  top_n(15, abs(Contribution)) |>
  mutate(SNS_Term = reorder(SNS_Term, Contribution)) |>
  ggplot(aes(SNS_Term, Contribution, fill = SNS_Term)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                   vjust = 0.5), axis.ticks.x = element_blank()) +
  labs(x = "Social Media Term",
       y = "Relative importance to principal component",
       title = "Top 15 Contributors to PC1")


sns_pca_long |>
  filter(PC =="PC2") |>
  top_n(15, abs(Contribution)) |>
  mutate(SNS_Term = reorder(SNS_Term, Contribution)) |>
  ggplot(aes(SNS_Term, Contribution, fill = SNS_Term)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                   vjust = 0.5), axis.ticks.x = element_blank()) +
  labs(x = "Social Media Term",
       y = "Relative importance to principal component",
       title = "Top 15 Contributors to PC2")

sns_pca_long |>
  filter(PC =="PC4") |>
  top_n(15, abs(Contribution)) |>
  mutate(SNS_Term = reorder(SNS_Term, Contribution)) |>
  ggplot(aes(SNS_Term, Contribution, fill = SNS_Term)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                   vjust = 0.5), axis.ticks.x = element_blank()) +
  labs(x = "Social Media Term",
       y = "Relative importance to principal component",
       title = "Top 15 Contributors to PC4")

sns_pca_long |>
  filter(PC =="PC5") |>
  top_n(15, abs(Contribution)) |>
  mutate(SNS_Term = reorder(SNS_Term, Contribution)) |>
  ggplot(aes(SNS_Term, Contribution, fill = SNS_Term)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                   vjust = 0.5), axis.ticks.x = element_blank()) +
  labs(x = "Social Media Term",
       y = "Relative importance to principal component",
       title = "Top 15 Contributors to PC5")

#Binding the first 4 principle components into the dataset
sns_data_pca <- cbind(sns_data[1:4], sns_pca$x)

#Building a Linear regression model to predict the number of friends using the
#features built into the new dataset
m <- lm(friends ~ PC1 + PC2 + PC3 + PC4 + PC5, data = sns_data_pca)
m












