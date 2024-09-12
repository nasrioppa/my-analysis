library(ggplot2)
library(palmerpenguins)

#smooth + point
ggplot(data = penguins) + 
  geom_smooth(mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g))
#same
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth()

#geom smooth + linetype
ggplot(data = penguins) + 
  geom_smooth(mapping = aes(x = flipper_length_mm, y = body_mass_g, 
                            linetype = species))
#geom_jitter
ggplot(data = penguins) + 
  geom_jitter(mapping = aes(x = flipper_length_mm, y = body_mass_g))

#bar for diamonds cut
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut)) #count the each type of cut

#color bar from color
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, color = cut)) #color the boarder of bar

#color in the bar
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut)) #fill the color of bar

#color in the bar to separate clarity
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))
