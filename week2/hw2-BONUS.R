library(ggplot2)
library(ggmap)
library(mapproj)
library(maps)
map <- get_map(location = c(lon = 121.555, lat = 25.0592), zoom = 12,language = "zh-TW",maptype = "toner-lite")


u <- read.csv("Address_Finish.csv")
ggmap(map,darken = c(0.2, "white")) + geom_point(aes(x = Response_X, y = Response_Y ), data = u, color="purple", size=0.9 )+labs(title = "台北市102年到107年自行車竊盜分布地圖")
