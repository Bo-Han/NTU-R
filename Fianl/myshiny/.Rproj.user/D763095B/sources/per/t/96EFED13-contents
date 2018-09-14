
library(sf)
library(ggplot2)
worldmap <- st_read("TM_WORLD_BORDERS-0.3.shp",stringsAsFactors=FALSE,quiet=TRUE)
names(worldmap)[3] <- paste('Country.Code')
worldmap <- st_transform(worldmap, "+init=esri:54030")
raw_gender <- read.csv('Gender_wewant.csv')

gender_map <- merge(worldmap, raw_gender, by="Country.Code")

library(stringr)
for (i in 15:ncol(gender_map)-1){
  names(gender_map)[i] <- str_sub(names(gender_map)[i],-5,-2)
  if((i >= 17) == (i < 41)){
    gender_map[i+1] <- (gender_map[[i+1]] + gender_map[[i]] + gender_map[[i-1]] + gender_map[[i-2]]) /4
  }
}

library(tidyr)
library(dplyr)

ath<- read.csv("athlete_wewant.csv")
ath['num']=1

ath$Team=NULL
ath1<-ath%>%filter(Year>=1990)%>% group_by(NOC,Year,Sex)%>%summarize(Medal_sum=sum(Medal_score),people=sum(num))
ath1A<-ath%>%filter(Year>=1990)%>%group_by(NOC,Year)%>%summarize(join_people=sum(num))
ath1B<-unite(ath1A,NOC,NOC,Year,sep='_')

ath2A<-ath1%>%filter(Sex=="F")
ath2B<-unite(ath2A,NOC,NOC,Year,sep='_')
ath2C<-data.frame(ath2B[,1],ath2B[,3])

ath3<-merge(ath2C,ath1B,by="NOC")
ath3["Fscore"]=(ath3[,2]/ath3[,3])

#plotting
gender_map%>%
  filter(Series.Name == 'Population, female (% of total)')%>%
  select('2010')%>%
  ggplot() +
  geom_sf(aes(fill=`2010`))+
  scale_fill_gradient(low = "#FFE4F3", high = "#F62018")
