library(readr)

#Load data
data <- read_csv("Pregnant_Women_Participating.csv")

#show dataset
data

# convert average participant to number
data$Average.Participation <- as.numeric(data$Average.Participation)

#Now i want to remove NA values if have any in dataset
average_participation <- na.omit(data$Average.Participation)
