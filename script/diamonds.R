library(dplyr)
library(ggplot2)
library(lattice)
diamonds <- ggplot2::diamonds %>% mutate_if(is.factor, ~ factor(., ordered = FALSE))


#k fold cross validation
train.control <- caret::trainControl(method = "cv", number = 10)
# On entraîne le modèle
model_cv <- caret::train(price ~ carat + as.factor(cut) + as.factor(color) + as.factor(clarity) + depth + table + x + y +z, 
                         data = diamonds, 
                         na.action  = na.pass,
                         method = "lm",
                         trControl = train.control)
print(model_cv)

colnames(diamonds)

flights_weather_nonaFactor %>%
  group_by(origin, wind_dir) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = wind_dir, y = dep_delay)) +
  geom_point(aes(color=origin)) +
  geom_smooth(method = 'lm')

diamonds %>% group_by(carat) %>% 
  summarise(prix_moy  = mean(price)) %>% 
  ggplot(aes(x=carat, y =prix_moy))+
  geom_point(aes(color = carat ))+
  geom_smooth(method = 'lm')

diamonds %>% group_by(carat, clarity) %>% 
  summarise(prix_moy  = mean(price)) %>% 
  ggplot(aes(x=carat, y =prix_moy))+
  geom_point(aes(color = clarity ))+
  geom_smooth(method = 'lm')

diamonds %>% group_by(carat, cut) %>% 
  summarise(prix_moy  = mean(price)) %>% 
  ggplot(aes(x=carat, y =prix_moy))+
  geom_point(aes(color = cut ))+
  geom_smooth(method = 'lm')

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

