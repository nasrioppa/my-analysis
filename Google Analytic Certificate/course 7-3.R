library(ggplot2)
data(diamonds)
View(diamonds)

head(diamonds) #6 row
str(diamonds) #colname and types
colnames(diamonds)

library(tidyverse)
mutate(diamonds, carat_2 = carat*100) #add col
