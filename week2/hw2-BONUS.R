library(ggplot2)
library(ggmap)
library(mapproj)
library(maps)
map <- get_map(location = c(lon = 121.555, lat = 25.0592), zoom = 12,language = "zh-TW",maptype = "toner-lite")


u <- read.csv("Address_Finish.csv")
ggmap(map) + geom_point(aes(x = Response_X, y = Response_Y ), data = u )