## Step 1: Load packages
library(tidyverse)
library(skimr)
library(janitor)

## Step 2: Import data
bookings_df <- read_csv("hotel_bookings.csv")

## Step 3: Getting to know your data
head(bookings_df)
str(bookings_df)
glimpse(bookings_df)
colnames(bookings_df)
skim_without_charts(bookings_df)

## Step 4: Cleaning your data
#primarily interested in the following variables: 'hotel', 'is_canceled', and 'lead_time'
trimmed_df <- bookings_df %>% 
  select(hotel, is_canceled, lead_time)

#rename them to make them easier to understand 'hotel' to be named 'hotel_type'
trimmed_df <- bookings_df %>% 
  select(hotel, is_canceled, lead_time) %>% 
  rename(hotel_type = hotel)

#either split or combine data in different columns. In this example, 
#you can combine the arrival month and year into one column using the unite() function
example_df <- bookings_df %>%
  select(arrival_date_year, arrival_date_month) %>% 
  unite(arrival_month_year, c("arrival_date_month", "arrival_date_year"), sep = " ")

## Step 5: Another way of doing things
#use the`mutate()` function to make changes to your columns. 
#Let's say you wanted to create a new column that summed up all the adults, children, and babies 
example_df <- bookings_df %>%
  mutate(guests = adults, children, babies)

head(example_df)

#Now it's time to calculate some summary statistics! Calculate the total number of canceled bookings and the average lead time for booking
#Make a column called 'number_canceled'
#'average_lead_time' represent the average lead time. Use the `summarize()`

example_df <- bookings_df %>%
  summarize(number_canceled = sum(is_canceled), 
            average_lead_time = mean(lead_time))
head(example_df)
  