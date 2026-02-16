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

#retrieving data on patient symptoms
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

#retrieving a sub-list from the list object
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

#calling data frame
pt_data