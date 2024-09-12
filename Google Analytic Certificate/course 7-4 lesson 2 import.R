library(tidyverse)

## Step 2: Import data
bookings_df <- read_csv("hotel_bookings.csv")

## Step 3: Inspect & clean data
head(bookings_df)
str(bookings_df)
colnames(bookings_df)

#create another data frame using `bookings_df` that focuses on the average daily rate
new_df <- select(bookings_df, `adr`, adults)

#create new variables in your data frame
mutate(new_df, total = `adr` / adults)

# Step 4: Import your own data
own_df <- read_csv("<filename.csv>")



