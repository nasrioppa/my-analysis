## Step 1: Import your data
hotel_bookings <- read.csv("hotel_bookings.csv")

## Step 2: Refresh Your Memory
head(hotel_bookings)
colnames(hotel_bookings)

library(ggplot2)

## Step 4: Making a Bar Chart
#how many of the transactions are occurring for each different distribution type
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel))

## Step 5: Diving deeper into bar charts
#if the number of bookings for each distribution type is different depending on 
#whether or not there was a deposit or what market segment they represent.

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel, fill=deposit_type))

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel, fill=market_segment))

## Step 6: Facets galore
#create separate charts for each deposit type and market segment
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type)

#it's hard to read the x-axis labels add one piece of code at the end that rotates the text to 45 degrees to make it easier to read.
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type) +
  theme(axis.text.x = element_text(angle = 45))

#market segment
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~market_segment) +
  theme(axis.text.x = element_text(angle = 45))

#facet grid for deposit
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_grid(~deposit_type) +
  theme(axis.text.x = element_text(angle = 45))

#put all of this in one chart and explore the differences by deposit type and market segment
ggplot(data = hotel_bookings, aes(x = distribution_channel)) +
  geom_bar() +
  facet_wrap(~deposit_type~market_segment) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type~market_segment) +
  theme(axis.text.x = element_text(angle = 45))