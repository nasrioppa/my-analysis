library(tidyverse)
library(lubridate)
#`head()` function, which displays the columns and the first several rows of data
head(diamonds)

#`str()` and `glimpse()` functions will both return summaries of each column 
#in your data arranged horizontally
str(diamonds)
glimpse(diamonds)

#returns a list of column names from your dataset.
colnames(diamonds)

# rename more than one variable in the same `rename()` code
rename(diamonds, carat_new = carat)
rename(diamonds, carat_new = carat, cut_new = cut)

#generate a wide range of summary statistics
#wanted to know what the mean for `carat` was in this dataset
summarize(diamonds, mean_carat = mean(carat))

#To build a visualization with `ggplot2
ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point()

#you wanted to change the color of each point
ggplot(data = diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point()

#create a different plot for each type of cut
ggplot(data = diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point() +
  facet_wrap(~cut)

#


