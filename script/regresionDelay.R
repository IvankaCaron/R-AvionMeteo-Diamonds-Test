library(nycflights13)
library(dplyr)
#library(tidyverse)
flights_weather <- flights %>% inner_join(weather, by = c("time_hour", "origin"))
sum(is.na(flights_weather$dep_delay))

flights_weather_nona <- flights_weather %>% filter(!is.na(dep_delay))
flights_weather_nona

# temp
modeltemp <-  lm(temp ~dep_delay, data=flights_weather_nona)
modeltemp %>% 
  map(summary) %>%
  str()


flights_weather_nona%>%ggplot(aes(temp, dep_delay)) +
  geom_point() +
  geom_smooth(method = 'lm')

flights_weather_nona%>%ggplot(aes(wind_dir, dep_delay)) +
  geom_point() +
  geom_smooth(method = 'lm')

flights_weather_nona %>%
  group_by(origin, wind_dir) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = wind_dir, y = dep_delay)) +
  geom_point(aes(color=origin)) +
  geom_smooth(method = 'lm')

# temp
summary(lm(dep_delay ~ humid , data=flights_weather_nona))$coeficients
summary(lm(temp ~dep_delay, data=flights_weather_nona))
sum_temp <- summary(lm(temp ~dep_delay, data=flights_weather_nona))
# humid
sum_humid <- summary(lm(humid ~dep_delay, data=flights_weather_nona))
# precip
sum_precip <- summary(lm(precip ~dep_delay, data=flights_weather_nona))
#pressure
sum_pressure <- summary(lm(pressure ~dep_delay, data=flights_weather_nona))
#visib
sum_visib <- summary(lm(visib ~dep_delay, data=flights_weather_nona))
# wind_speed
sum_wind_speed <- summary(lm(wind_speed ~dep_delay, data=flights_weather_nona))
# wind_gust
sum_wind_gust <- summary(lm(wind_gust ~dep_delay, data=flights_weather_nona))

#  coefficients 
sum_temp$coefficients
sum_temp$coefficients[2]
coef_temp <- sum_temp$coefficients[2]
coef_humid <- sum_humid$coefficients[2]
coef_precip <- sum_precip$coefficients[2]
coef_pressure <- sum_pressure$coefficients[2]
coef_visib <- sum_visib$coefficients[2]
coef_wind_speed <- sum_wind_speed$coefficients[2]
coef_wind_gust <- sum_wind_gust$coefficients[2]
coef_temp <- sum_temp$coefficients[2]

# p-value
sum_temp$coefficients
pv_temp <- sum_temp$coefficients["dep_delay",4]
pv_humid <- sum_humid$coefficients["dep_delay",4]
pv_precip <- sum_precip$coefficients["dep_delay",4]
pv_pressure <- sum_pressure$coefficients["dep_delay",4]
pv_visib <- sum_visib$coefficients["dep_delay",4]
pv_wind_speed <- sum_wind_speed$coefficients["dep_delay",4]
pv_wind_gust <- sum_wind_gust$coefficients["dep_delay",4]

# R2
sum_humid$r.squared
r2_temp <- sum_temp$r.squared[1]
r2_humid <- sum_humid$r.squared
r2_precip <- sum_precip$r.squared
r2_pressure <- sum_pressure$r.squared
r2_visib <- sum_visib$r.squared
r2_wind_speed <- sum_wind_speed$r.squared
r2_wind_gust <- sum_wind_gust$r.squared


df <- data.frame(name = c("temp", "humid", "precip", "pressure", "visib", "wind_speed", "wind_gust"),
                 coef = c(coef_temp, coef_humid, coef_precip, coef_pressure, coef_visib, coef_wind_speed, coef_wind_gust),
                 pv = c(pv_temp, pv_humid, pv_precip, pv_pressure, pv_visib, pv_wind_speed, pv_wind_gust),
                 r2 = c(r2_temp, r2_humid, r2_precip, r2_pressure, r2_visib, r2_wind_speed, r2_wind_gust)
)

print(df)
summary(lm(temp ~dep_delay, data=flights_weather_nona))

#regresion multiple
variables <- list('humid', 'precip', 'pressure', 'visib', 'wind_speed', 'wind_gust')
chaine <- c()
for (x in variables) {
  chaine <- c(chaine, x)
  # print(chaine_1)
  print(chaine)
  print(summary(lm(dep_delay~ ., 
                   data = select(flights_weather_nona, 
                                 c("dep_delay", chaine)))))
}




