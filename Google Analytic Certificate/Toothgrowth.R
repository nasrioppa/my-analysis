#load data
data("ToothGrowth")
view(ToothGrowth)

#use dplyr
library(dplyr)
filter_tg <- filter(ToothGrowth, dose == 0.5)
filter_tg

arrange(filter_tg, len)

#nested
arrange(filter(ToothGrowth, dose == 0.5), len)

#use pipes
filtered_toothgrowth <- ToothGrowth %>% 
  filter(dose == 0.5) %>% 
  arrange(len)

filtered_toothgrowth <- ToothGrowth %>% 
  filter(dose == 0.5) %>% 
  group_by(supp) %>% 
  summarize(mean_len = mean(len, na.rm = T), .group = "drop")