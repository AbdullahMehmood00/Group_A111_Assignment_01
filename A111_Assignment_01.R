library(readr)

#Load data
data <- read_csv("Pregnant_Women_Participating.csv")

#show dataset
data

# convert average participant to number
data$Average.Participation <- as.numeric(data$"Average Participation")

#Now i want to remove NA values if have any in dataset
average_participation <- na.omit(data$Average.Participation)

#NOw creating histogram
hist(average_participation, breaks = 15, probability = TRUE, col = "blue", main = "Histogram of Average Participation with Bell Curve", xlab="Average Participaion (Number of Pregnant Women)", ylab = "Frequency")

# density curve
lines(density(average_participation), col="red", lwd=2)
