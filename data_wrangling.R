#Author: Janne Markus Kristian Mikkonen
#Email: janne.mk.mikkonen a helsinki.fi
#Date: 12.12.2017

#Creating a combined data set on Portuguese secondary level students
#Original data source: https://archive.ics.uci.edu/ml/datasets/Student+Performance

#Read the two data files
setwd("~/Documents/GitHub/IODS-project/data")
math <- read.csv("student-mat.csv", header = TRUE, sep = ";")
por <- read.csv("student-por.csv", header = TRUE, sep = ";")

#Explore the structure and dimensions of these data sets
str(math)
dim(math)
str(por)
dim(por)

#Both data sets have 33 variables, which have the same names and include both factors and integers
#Math data set has 395 observations whereas por dataset has 649 observations

#Join the data sets with the help of dpylr package
library(dplyr)

#Common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

#Join the two datasets by the selected identifiers
math_por <- inner_join(math, por, by = join_by, suffix = c(".math", ".por"))

#Explore the structure and dimensions of the new data set
str(math_por)
dim(math_por)

#The joined data includes 53 varibles because we did not use all variables for joining
#The number of observations is now 382: based on the identifiers, this many students were
#included in both data sets

#Next we need to combine also those variables that were not used for joining the data sets
#Since the responses could differ, we pick either the mean (integers) or the first value (factors)

# create a new data frame with only the joined columns
students <- select(math_por, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    students[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    students[column_name] <- first_column
  }
}

#Define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

#Explore the data with glimpse
glimpse(alc)

#Our alc dataframe now contains all the original variables combined for 382 observations
#We have also created a variable measuring total alcohol consumption

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

##Give more descriptive class labels for "activities", "paid", and romantic

levels(students$activities) <- c("extracurricular: no", "extracurricular: yes")
levels(students$paid) <- c("paid classes: no", "paid classes: yes")
levels(students$romantic) <- c("relationship: no", "relationship: yes")

##Save the data with 10 factor variables and 382 observations
setwd("~/Documents/GitHub/IODS-final")
write.csv(students, file = "students.csv", row.names = FALSE)