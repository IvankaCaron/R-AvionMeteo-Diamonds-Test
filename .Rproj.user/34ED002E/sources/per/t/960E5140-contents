library(nycflights13)
library(dplyr)
library(ggplot2)
library(lattice)



flights_weather <- flights %>% inner_join(weather, by = c("time_hour", "origin"))
sum(is.na(flights_weather$dep_delay))

flights_weather_nona <- flights_weather %>% filter(!is.na(dep_delay))
flights_weather_nonaFactor <- flights_weather_nona %>% mutate(origin_fac = as.factor(origin))

n = nrow(flights_weather_nonaFactor)
split = sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.75, 0.25))

#separation de donne test & training
training = flights_weather_nonaFactor[split, ]
testing = flights_weather_nonaFactor[!split, ]

#ypred - y moyenne MAE ( compare sa o moyer de abs(res de training test))
#racine care de la moyene ypred - y au care - RMSE
#k fold cross validation - combian de fois je vais partitione mon base de done, caret contien ca 

# model
 model <-    lm(dep_delay ~ temp + humid + precip + pressure + visib + wind_speed  + origin_fac, data=training)
summary(model)
summary(model)$coefficients
summary(model)$r.squared
#prediction
predict(model, testing)
pred_y <- c(predict(model, testing))
pred_y <- c(pred_y)
testing$testingPredict <- pred_y 
testing%>% select(dep_delay, testingPredict)
#evaluation du model - regresion lin
summary(lm(dep_delay ~0+ testingPredict,data=testing))
summary(lm(dep_delay ~testingPredict,data=testing))
#evaluation du model - Pearson's product-moment correlation
cor.test(testing$testingPredict,testing$dep_delay)


testing %>% mutate(error_squared = abs(testingPredict - dep_delay)) %>% 
  summarise_at(vars(error_squared), 
               list(moy = mean, min = min, med = median, max = max, sd = sd), na.rm = T)


#k fold cross validation
train.control <- caret::trainControl(method = "cv", number = 10)
# On entraîne le modèle
model_cv <- caret::train(dep_delay ~ temp + humid + precip + pressure + visib + 
                           wind_speed + distance + as.factor(origin), 
                         data = flights_weather_nona, 
                         na.action  = na.pass,
                         method = "lm",
                         trControl = train.control)
print(model_cv)


flights_weather_nonaFactor%>%ggplot(aes(temp, dep_delay)) +
  geom_point() +
  geom_smooth(method = 'lm')

flights_weather_nonaFactor%>%ggplot(aes(wind_dir, dep_delay)) +
  geom_point() +
  geom_smooth(method = 'lm')

flights_weather_nonaFactor %>%
  group_by(origin, wind_dir) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = wind_dir, y = dep_delay)) +
  geom_point(aes(color=origin)) +
  geom_smooth(method = 'lm')

#les valeurs manququant
flights_weather %>% mutate(delay_manquante = if_else(is.na(dep_delay), 1, 0)) %>% 
  group_by(delay_manquante) %>% 
  summarise_at(vars(temp, humid, precip, pressure, visib, wind_speed, wind_gust), list(moy = mean) , na.rm = T)

#regresion multiple - ajout variable un par an
variables <- list('humid', 'precip', 'pressure', 'visib', 'wind_speed')
chaine <- c()
for (x in variables) {
  chaine <- c(chaine, x)
  # print(chaine_1)
  print(chaine)
  print(summary(lm(dep_delay~ ., 
                   data = select(flights_weather_nona, 
                                 c("dep_delay", chaine)))))
}


#regresion multiple - ajout variable un par an avec coef
variables <- list('humid', 'precip', 'pressure', 'visib', 'wind_speed')
chaine <- c()
for (x in variables) {
  chaine <- c(chaine, x)
  # print(chaine_1)
  print(chaine)
  print(summary(lm(dep_delay~ ., 
                   data = select(flights_weather_nona, 
                                 c("dep_delay", chaine))))$coefficients)
  print(summary(lm(dep_delay~ ., 
                   data = select(flights_weather_nona, 
                                 c("dep_delay", chaine))))$r.squared)
}
