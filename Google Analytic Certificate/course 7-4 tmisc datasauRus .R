#Same data, different outcome
library("Tmisc")
data("quartet")
view(quartet)

quartet %>% 
  group_by(set) %>% 
  summarize(mean(x), sd(x), mean(y), sd(y), cor(x,y))

ggplot(quartet, aes(x,y)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) + 
  facet_wrap(~set)

library("datasauRus")
ggplot(datasaurus_dozen,aes(x=x,y=y,colour=dataset)) +
  geom_point() +
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap(~dataset,ncol=3)