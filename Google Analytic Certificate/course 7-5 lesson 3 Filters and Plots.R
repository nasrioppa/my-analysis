## Step 1: Import your data
hotel_bookings <- read.csv("hotel_bookings.csv")

## Step 2: Refresh Your Memory
head(hotel_bookings)
colnames(hotel_bookings)

library(ggplot2)

## Step 4: Making many different charts
#the relationship between booking lead time and guests traveling with children
ggplot(data = hotel_bookings) +
  geom_point(mapping = aes(x = lead_time, y = children))
  #guests who make early bookings, and this plot showed that many of these guests do not have children

#which market segments generate the largest number of bookings, 
#and where these bookings are made (city hotels or resort hotels)
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = hotel, fill = market_segment))

#difficult to compare the size of the market segments at the top of the bars
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = hotel)) +
  facet_wrap(~market_segment)

## Step 5: Filtering
library(tidyverse)

#create a plot that shows the relationship between lead time and guests traveling with children 
#for online bookings at city hotels
onlineta_city_hotels <- filter(hotel_bookings, 
                               (hotel == "City Hotel" & 
                                market_segment == "Online TA"))

View(onlineta_city_hotels)

#another way to do this
onlineta_city_hotels_v2 <- hotel_bookings %>% 
  filter(hotel == "City Hotel") %>% 
  filter(market_segment == "Online TA")

View(onlineta_city_hotels_v2)

## Step 6: Use your new dataframe
ggplot(data = onlineta_city_hotels_v2) +
  geom_point(mapping = aes(x = lead_time, y = children))

#The plot reveals that bookings with children tend to have a shorter lead time, and bookings with 3 children have a significantly shorter lead time (\<200 days).


