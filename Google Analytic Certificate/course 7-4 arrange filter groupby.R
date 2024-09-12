library(tidyverse)
#arrange
penguins %>% arrange(bill_length_mm) #ASC
penguins %>% arrange(-bill_length_mm) #DESC

penguins2 <- penguins %>% arrange(-bill_length_mm) 
view(penguins2)

#group summarize
penguins %>% 
  group_by(island) %>% 
  drop_na() %>% 
  summarize(mean_bill_length = mean(bill_length_mm))

penguins %>% 
  group_by(island) %>% 
  drop_na() %>% 
  summarize(max_bill_length = max(bill_length_mm))

penguins %>% 
  group_by(species, island) %>% 
  drop_na() %>% 
  summarize(max_bl = max(bill_length_mm), mean_bl= mean(bill_length_mm))

#filter
penguins %>% 
  filter(species == "Adelie")