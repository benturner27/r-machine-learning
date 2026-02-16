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

#Alternative way of retriving subject name
subject_name[temperature > 100]
