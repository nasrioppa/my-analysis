library("here")
library("skimr")
library("janitor")
library("dplyr")

library(palmerpenguins)
skim_without_charts(penguins)
glimpse(penguins)
head(penguins)

#select
penguins %>% 
  select(-species)

penguins %>% 
  rename(island_new = island)

rename_with(penguins, toupper) #rename cols all to upper

rename_with(penguins, tolower) #rename cols all to lower

clean_names(penguins) #change name that easy to recall