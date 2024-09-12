library(ggplot2)
library(palmerpenguins)

data(penguins)
View(penguins)

#color for define species
ggplot(data = penguins) + 
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species))

#shape for define species
ggplot(data = penguins) + 
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, shape = species))

#shape + color for define species
ggplot(data = penguins) + 
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g,shape = species, color = species))

#shape + color + size for define species
ggplot(data = penguins) + 
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g,
                           shape = species, color = species, size = species))

#alpha for define species
ggplot(data = penguins) + 
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, alpha = species))

ggplot(data = penguins) + 
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g), color = 'purple')

