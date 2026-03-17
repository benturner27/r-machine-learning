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
