#Chargement de library
library(nycflights13)
library(dplyr)
library(ggplot2)
#dimension de flight
dim(flights)

#chargement des tables
flights <- nycflights13::flights
weather <- nycflights13::weather
planes <- nycflights13::planes
airports <- nycflights13::airports
airlines <- nycflights13::airlines

#dictionnaire des variables
?nycflights13 ::flights
?nycflights13::weather


sum(is.na(flights$distance))

testNA <- flights %>% filter( !is.na(dep_time) & (is.na(flights$air_time) | is.na(dep_delay) | is.na(arr_delay)))
dim(testNA)


summary(flights$distance)
unique(flights$origin)

flights %>% 
  group_by(AerOrigin =origin) %>% 
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    sd_distance = sd(distance, na.rm = TRUE),
    min_distance = min(distance, na.rm = TRUE),
    max_distance = max(distance, na.rm = TRUE)
  )

summarize(group_by(flights, origin), distance_mean = mean(distance), 
          distance_sd = sd(distance), distance_min = min(distance),
          distance_max = max(distance))

flights%>% 
  group_by(flights$origin)%>% 
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    mean_airtime = mean(air_time, na.rm = TRUE),
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    mean_arr_delay = mean(arr_delay, na.rm = TRUE)
   
  )

DistanceStat = flights %>% 
  group_by(airport = origin) %>% 
  summarise(
    mean_distance = mean(distance, na.rm = TRUE),
    sd_distance = sd(distance, na.rm = TRUE),
    min_distance = min(distance, na.rm = TRUE),
    max_distance = max(distance, na.rm = TRUE)
  )

AirTimeStat = flights %>% 
  group_by(airport = origin) %>% 
  summarise(
    mean_air_time = mean(air_time, na.rm = TRUE),
    sd_air_time = sd(air_time, na.rm = TRUE),
    min_air_time = min(air_time, na.rm = TRUE),
    max_air_time = max(air_time, na.rm = TRUE)
  )

DepDelayStat = flights %>% 
  group_by(airport = origin) %>% 
  summarise(
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    sd_dep_delay = sd(dep_delay, na.rm = TRUE),
    min_dep_delay = min(dep_delay, na.rm = TRUE),
    max_dep_delay = max(dep_delay, na.rm = TRUE)
  )

ArrDelayStat = flights %>% 
  group_by(airport = origin) %>% 
  summarise(
    mean_arr_delay = mean(arr_delay, na.rm = TRUE),
    sd_arr_delay = sd(arr_delay, na.rm = TRUE),
    min_arr_delay = min(arr_delay, na.rm = TRUE),
    max_arr_delay = max(arr_delay, na.rm = TRUE)
  )

DistanceStat%>%
  ggplot(aes(x = airport, y = mean_distance)) + 
  geom_bar(stat = "identity", aes(color = airport)) + facet_wrap(~airport)+ 
  geom_point(aes(x = airport, y = sd_distance), col = "red")+ 
  geom_point(aes(x = airport, y = min_distance), col = "blue")+ 
  geom_point(aes(x = airport, y = max_distance), col = "green")+
  ggtitle("airportStat")

ggplot(DistanceStat) + 
  geom_boxplot(aes(x = airport, y = mean_distance)) + 
  geom_point(aes(x = airport, y = sd_distance), col = "red")+ 
  geom_point(aes(x = airport, y = min_distance), col = "blue")+ 
  geom_point(aes(x = airport, y = max_distance), col = "green")

# Aperçu rapide en utilisant la fonction 'is.na'
sapply(flights, function(x) sum(is.na(x)))
#flights <- select(flights, -year, -month, -day, -hour)

#select(df7new, -year)
FlightWeather = merge(flights, weather, by =c("year", "month", "day", "hour", "origin"))
#, by = c("time_hour", "origin")
unique(FlightWeather["origin"])

dim(weather)
unique(weather["origin"])
#1        EWR
#8704     JFK
#17410    LGA
unique(weather["month"])
# > unique(weather["month"])
# month
# 1        1
# 743      2
# 1412     3
# 2155     4
# 2875     5
# 3619     6
# 4339     7
# 5080     8
# 5820     9
# 6539    10
# 7275    11
# 7990    12
unique(weather["time_hour"])

sapply(weather, function(x) sum(is.na(x)))
sapply(FlightWeather, function(x) sum(is.na(x)))
dim(weather)
dim(FlightWeather)


FlightWeather %>%
  group_by(humid) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = humid, y = dep_delay)) +
  geom_line(color ="red") + geom_point(color ="red") 

FlightWeather %>%
  group_by(precip) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = precip, y = arr_delay)) +
  geom_line(color ="blue") + geom_point(color ="blue") 

FlightWeather %>%
  group_by( precip) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE),
            arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = dep_delay, y = arr_delay) ) + geom_point(color = "blue") 

#ca l'air que visib a l'impact sur le delay:
FlightWeather %>%
  group_by(Aéroport = origin, visib) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = visib, y = dep_delay)) +
  geom_point(aes(color = Aéroport)) +
  labs(title="Visibilité  / Retards aux départs",
       x = "Visibilité", y ="Retards aux départs")



FlightWeather %>%
  group_by(temp) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = temp, y = dep_delay)) +
  geom_point(color ="red") +geom_line(color ="red") 

FlightWeather %>%
  group_by(Aéroport =origin, wind_gust) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = wind_gust, y = dep_delay)) +
  geom_point(aes(color=Aéroport)) +
  labs(title="Rafale / Retards aux départs",
       x = "Rafale", y ="Retards aux départs")

?nycflights13::weather

FlightWeather %>%
  group_by(origin, wind_dir) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = wind_dir, y = dep_delay)) +
  geom_point(aes(color=origin))

FlightWeather %>%
  group_by( wind_dir) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = wind_dir, y = dep_delay)) +
  geom_point()

#Graph1 <-  df1 %>% filter(year == 2007) %>% 
 # ggplot(aes(x = lifeExp, y = gdpPercap, size = pop)) +
#  geom_point(aes(color = continent)) + xlab(label= "Espérance de vie(années") +
 # ylab(label = "PIB/hab") + ggtitle("Espérance de vie et PIB/hab en 2007") 
#Graph1 


unique(weather$wind_dir)

FlightWeather %>%
  group_by(wind_speed) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = wind_speed, y = dep_delay)) +
  geom_point()
  
FlightWeather %>%
  group_by(pressure) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = pressure, y = dep_delay)) +
  geom_point()

worst_hours <- flights %>%
  mutate(hour = sched_dep_time %/% 100) %>%
  group_by(origin, year, month, day, hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(dep_delay)) %>%
  slice(1:48)
weather_most_delayed <- semi_join(weather, worst_hours, 
                                  by = c("origin", "year",
                                         "month", "day", "hour"))

select(weather_most_delayed, temp, wind_speed, precip) %>%
  print(n = 48)
ggplot(weather_most_delayed, aes(x = precip, y = wind_speed, color = temp)) +
  geom_point()

FlightWeather$dep_delay_bool <- FlightWeather$dep_delay <0


