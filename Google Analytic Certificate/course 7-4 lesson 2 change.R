library(tidyverse)
library(skimr)
library(janitor)

## Step 2: Import data
hotel_bookings <- read_csv("hotel_bookings.csv")

## Step 3: Getting to know your data
head(hotel_bookings)
str(hotel_bookings)
glimpse(hotel_bookings)
colnames(hotel_bookings)

## Manipulating your data
#arrange the data by most lead time to least lead time
arrange(hotel_bookings, -lead_time)
#or
arrange(hotel_bookings, desc(lead_time))
head(hotel_bookings)

#create a new data frame that had those changes saved
hotel_bookings_v2 <- arrange(hotel_bookings, desc(lead_time))
head(hotel_bookings_v2)

#find out the maximum and minimum lead times without sorting 
max(hotel_bookings$lead_time)
min(hotel_bookings$lead_time)

#want to know what the average lead time
mean(hotel_bookings$lead_time)

#same answer even if you use the v2 dataset
mean(hotel_bookings_v2$lead_time)

#want to know what the average lead time before booking is for just city hotels
hotel_bookings_city <- filter(hotel_bookings, hotel_bookings$hotel=="City Hotel")
head(hotel_bookings_city)

#average lead time for this set of hotels
mean(hotel_bookings_city$lead_time)

#wants to know a lot more information about city hotels, including the maximum and minimum lead time
hotel_summary <-
  hotel_bookings %>% 
  group_by(hotel) %>% 
  summarize(average_lead_time=mean(lead_time),
            min_lead_time=min(lead_time),
            max_lead_time=max(lead_time))

head(hotel_summary)

