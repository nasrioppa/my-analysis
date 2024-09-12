library("palmerpenguins")
library(ggplot2)

ggplot(data = penguins, aes(x=flipper_length_mm, y=body_mass_g)) + geom_point()

ggplot(data = penguins, aes(x=flipper_length_mm, y=body_mass_g)) + 
      geom_point(aes(color = species))

ggplot(data = penguins, aes(x=flipper_length_mm, y=body_mass_g)) + 
  geom_point(aes(shape = species))

ggplot(data = penguins, aes(x=flipper_length_mm, y=body_mass_g)) + 
  geom_point(aes(shape = species, color = species))

ggplot(data = penguins, aes(x=flipper_length_mm, y=body_mass_g)) + 
  geom_point(aes(shape = species, color = species)) + facet_wrap(~species)

ggplot(data = penguins, aes(x=flipper_length_mm, y=body_mass_g)) + 
  geom_point(aes(shape = species, color = species)) + facet_wrap(~species) + labs(title = "Body mass vs Flipper")
