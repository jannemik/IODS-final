#Author: Janne Markus Kristian Mikkonen
#Email: janne.mk.mikkonen a helsinki.fi
#Date: 12.12.2017

##Read the previously combined student alcohol consumption data
alc <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/alc.txt", sep=",", header=TRUE)

##Explore the structure and dimensions of the data
#The data has 35 variables and 382 observations
#There are both integer and factor variables in the data
str(alc)
dim(alc)

##Transform selected integer variables into factor variables for MCA and give informative labels

#1. Free time
alc$freetime <- as.factor(alc$freetime)
levels(alc$freetime) <- c("freetime: very low", "freetime: low", "freetime: medium", "freetime: high", "freetime: very high")

#2. Absences
summary(alc$absences)
#Based on the distribution, create three classes: 0, 1-2, 3-8, over 8
alc$abs <- cut(alc$absences, breaks = c(0, 1, 4, 8, 75), include.lowest = TRUE, labels = c("0-1 absences", "2-4 absences", "5-8 absences", "over 8 absences"))

#3. Study time
table(alc$studytime)
#Use the original classification but give labels
alc$studytime <- as.factor(alc$studytime)
levels(alc$studytime) <- c("<2 h", "2 to 5 h", "5 to 10 h", ">10 h")

#4. Final grade
summary(alc$G3)
#Quantiles of the final grade
bins <- quantile(alc$G3)
alc$grade <- cut(alc$G3, breaks = bins, include.lowest = TRUE, labels = c("low grade", "med-low grade", "med-high grade", "high grade"))

#5. Alcohol use
table(alc$alc_use)
alc$alc3 <- cut(alc$alc_use, c(1, 1.5, 3, 5), include.lowest = TRUE, labels = c("low alc use", "medium alc use", "high alc use"))

#6. Going out with friends
alc$friends <- as.factor(alc$goout)
levels(alc$friends) <- c("friends: very low", "friends: low", "friends: medium", "friends: high", "friends: very high")

##Keep the 10 variables of interest
library(dplyr)
keep_columns <- c("freetime", "abs", "studytime", "grade", "alc3", "friends", "activities", "reason", "paid", "romantic")
students <- select(alc, one_of(keep_columns))

##Give more descriptive class names for "activities", "paid", and romantic

levels(students$activities) <- c("extracurricular: no", "extracurricular: yes")
levels(students$paid) <- c("paid classes: no", "paid classes: yes")
levels(students$romantic) <- c("relationship: no", "relationship: yes")

##Save the data with 9 factor variables and 382 observations
setwd("~/Documents/GitHub/IODS-final")
write.csv(students, file = "students.csv", row.names = FALSE)