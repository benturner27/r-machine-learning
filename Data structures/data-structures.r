#Data structures in R

#Vectors

#Creating a set of three vectors
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

#Retrieving temperature of second patient 
temperature[2]

#Retrieving temperature of second and third patients using colon indexing
temperature[2:3]

#Excluding temperature of second patient from index retrieval
temperature[-2]

#Using logical vector to exclude third patient temperature
temperature[c(TRUE, TRUE, FALSE)]

#Creating logical vector to find out name of a patient who has fever
fever <- temperature > 100
subject_name[fever]

#Alternative way of retrieving subject name
subject_name[temperature > 100]

#Factors

#Creating a factor for patient gender data
gender <- factor(c("MALE", "FEMALE", "MALE"))

#Creating another factor for patient blood type data
blood <- factor(c("O", "AB", "A"),
                levels = c("A", "AB", "B", "O"))

#Creating symptoms factor with ordered parameter
symptoms <- factor(c("SEVERE", "MILD", "MODERATE"),
                   levels = c("MILD", "MODERATE", "SEVERE"),
                   ordered = TRUE)

#Retrieving data on patient symptoms
symptoms > "MODERATE"

#Lists

#Retrieving all data from the first patient without using a list
subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]
symptoms[1]

#Retrieving all data from the first patient using a list
subject1 <- list(fullname = subject_name[1],
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1],
                 symptoms = symptoms[1])

#Calling the list object
subject1

#Retrieving a sub-list from the list object
subject1[2]

#Retrieving only the value of an index in the list object
subject1[[2]]

#Alternative way for retrieving the value of an index
#Better for data continuity
subject1$temperature

#Retrieving multiple indexes in a list object
subject1[c("temperature", "flu_status")]

#Data Frames

#Creating a data frame of all patient data
pt_data <- data.frame(subject_name, temperature, flu_status, gender, blood,
                      symptoms)

#Calling data frame
pt_data

#Retrieving data from a data frame, using the $ operator
pt_data$subject_name

#Retrieving specific data using specified coordinates
pt_data[1, 2]

#Retrieving certain rows and columns using specified coordinates
pt_data[c(1, 3), c(2, 4)]

#Retrieving a whole column, leaving the row coordinate blank
pt_data[, 1]

#Retrieving a whole row, leaving the column coordinate blank
pt_data[1, ]

#Retrieving everything from data frame
pt_data[ , ]

#retrieving data from data frame using named values
pt_data[c(1, 3), c("temperature", "gender")]

#retrieving data using index positions
pt_data[-2, c(-1, -3, -5, -6)]

#function for converting degrees Fahrenheit to degrees Celcius
pt_data$temp_c <- (pt_data$temperature - 32) * (5 / 9)

#Verifying calculation works
pt_data[c("temperature", "temp_c")]

#Matrices and Arrays

#Creating a 2x2 matrix using nrow parameter
m <- matrix(c(1, 2, 3, 4), nrow = 2)

#Calling matrix
m

#Creating a 2x2 matrix using the ncol parameter
m2 <- matrix(c(1, 2, 3, 4), ncol = 2)

#Calling matrix
m2

#Creating a 2x3 matrix using the nrow parameter
m3 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)

#Calling matrix
m3

#Creating a 3x2 matrix using the ncol parameter
m4 <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)

#Calling matrix
m4

#Retrieving rows and columns from matrix
m4[1, ]
m4[, 1]

#Managing and Saving Data

#Using ls function to search for all objects in file
ls()

#Using rm function to remove objects not wanted for saving
rm(m, m2, m3, m4, subject1)

#Removing objects using the rm function and using ls as a list
rm(list = ls())

#Reading a csv file
pt_data <- read.csv("pt_data.csv")

#Reading a csv file, using generic header format in case csv has no headers
pt_data <- read.csv("pt_data.csv", header = FALSE)

#Reading a csv file, using stringsAsFactor parameter to convert
#Character columns as factors
pt_data <- read.csv("pt_data.csv", stringsAsFactors = TRUE)

#writing a csv file, without using row names in file
write.csv(pt_data, file = "pt_data.csv", row.names = FALSE)
