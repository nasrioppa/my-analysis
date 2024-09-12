## Step 1: Import your data
hotel_bookings <- read.csv("hotel_bookings.csv")

## Step 2: Look at a sample of your data
head(hotel_bookings)
colnames(hotel_bookings)

## Step 3: Install and load the 'ggplot2' package
library(ggplot2)

## Step 4: Begin creating a plot
#"I want to target people who book early, and 
#hypothesis that people with children have to book in advance."

ggplot(data = hotel_bookings) +
  geom_point(mapping = aes(x = lead_time, y = children))

## Step 5: Try it on your own
#She suggests that guests without children book the most weekend nights. Is this true? 
ggplot(data = hotel_bookings) +
  geom_point(mapping = aes(x = stays_in_weekend_nights, y = children))
