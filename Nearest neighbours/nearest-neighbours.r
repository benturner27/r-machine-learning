#Nearest Neighbours

#Read the csv file into the environment
wbcd <- read.csv("wisc_bc_data.csv")

#Examining wbcd data frame using str function - lists off all variables in
#data frame
str(wbcd)

#Remove the id variable from data frame - does not provide any useful data
wbcd <- wbcd[-1]

#Show data of diagnosis variable using table function - allows us to evaluate
#accuracy of training later on
table(wbcd$diagnosis)

#Renaming diagnosis values for better legibility recoding them as factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

#Convert the values of each label to a percentage using round function
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#Showing summary of three variables in data frame using summary function
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
