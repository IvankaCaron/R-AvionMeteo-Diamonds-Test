library(nycflights13)
library(maps)
library(geosphere)
library(dplyr)
airports
#https://www.gis-blog.com/flight-connection-map-with-r/

FlightWeather <-  merge(flights, weather, by =c("year", "month", "day", "hour", "origin", "time_hour"))
FlightWeather$dep_delay_bool <- FlightWeather$dep_delay <0
FlightWeatkerAirports = merge(FlightWeather , airports, by.x = "origin", by.y = "faa")



unique(FlightWeatkerAirports["origin"])


usairports <- filter(airports, lat < 48.5)
usairports <- filter(usairports, lon > -130)
usairports <- filter(usairports, faa!="JFK") #filter out jfk
jfk <- filter(FlightWeatkerAirports, origin=="JFK") #separate df for jfk

#create basemap
map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))
#overlay airports
points(usairports$lon,usairports$lat, pch=3, cex=0.1, col="chocolate1")

for (i in (1:dim(usairports)[1])) { 
  inter <- gcIntermediate(c(jfk$lon[1], jfk$lat[1]), c(usairports$lon[i], usairports$lat[i]), n=200)
  lines(inter, lwd=0.1, col="turquoise2")    
}


