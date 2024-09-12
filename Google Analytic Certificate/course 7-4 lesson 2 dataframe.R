library(tidyverse)

## Step 2: Create data frame
#First, create a vector of names by inserting four names
names <- c("Nasri", "Fifa", "Mangmoom", "Yoshitaka")

#Then create a vector of ages by adding four ages
age <- c(10, 20, 30, 40)

#With these two vectors, you can create a new data frame called `people`:
people <- data.frame(names, age)


## Step 3: inspect the data frame
head(people)
str(people)
glimpse(people)
colnames(people)

#use `mutate()` if you wanted to create a new variable
mutate(people, age_in_20 = age + 20)

## Step 4: Try it yourself
fruits <- c("apple", "oranges", "grape", "guava", "watermalon")

#Give a 1 to the fruit you like the most
score <- c(1, 2, 3, 4, 5)

#create data frame
fruit_ranks <- data.frame(fruits, score)