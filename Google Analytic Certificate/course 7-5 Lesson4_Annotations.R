## Step 1: Import your data
hotel_bookings <- read.csv("hotel_bookings.csv")

## Step 2: Refresh Your Memory
head(hotel_bookings)
colnames(hotel_bookings)

library(ggplot2)
library(tidyverse)

## Step 4: Annotating your chart
#compares market segments between city hotels and resort hotels
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel)

#add title
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  labs(title = "Comparison of market segments by hotel type for hotel bookings")

#add another detail about what time period
#earliest year
min(hotel_bookings$arrival_date_year)
#latest year
max(hotel_bookings$arrival_date_year)

#save them as variables in order to easily use
mindate <- min(hotel_bookings$arrival_date_year)
maxdate <- max(hotel_bookings$arrival_date_year)

#add in a subtitle Then, you can use the `paste0()` function to use your newly-created variables in your labels. 
#This is really handy, if the data gets updated, you don't have to change the code below because the variables are dynamic
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       subtitle=paste0("Data from: ", mindate, " to ", maxdate))

#little too prominently
#decide to switch the `subtitle`  to a `caption` which will appear in the bottom right
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       caption=paste0("Data from: ", mindate, " to ", maxdate))

#want to clean up the x and y axis labels to make sure they are really clear
#add to the `labs()` function and use `x=` and `y=`. to change the text of the label
ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       caption=paste0("Data from: ", mindate, " to ", maxdate),
       x="Market Segment",
       y="Number of Bookings")

## Step 5: Saving your chart
ggsave('hotel_booking_chart.png', width=7,
       height=7)



ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = color, fill = cut)) + 
  facet_wrap(~clarity)
