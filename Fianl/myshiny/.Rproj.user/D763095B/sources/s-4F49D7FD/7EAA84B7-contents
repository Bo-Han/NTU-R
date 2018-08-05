library(stringr)
gender <-read.csv("Gender_wewant1.csv")
any(is.na(gender))
#summary(gender)
library(dplyr)
library(tidyr)
library(ggplot2)
colnames(gender)<-c("Country.Code","Country.Name","Series.Name",as.character(c(1990:2016)))
gen1<-cbind(gender[,1],gender[,4:30])

gen1[264:526,2:28]<-(gen1[264:526,2:28]*(-1)+1000)/10
gen1[527:789,2:28]<-(gen1[527:789,2:28]*(-1)+10)*10

colnames(gen1)<-c("Country.Code",as.character(c(1990:2016)))
gen2<-gather(gen1,year,gender_var,-Country.Code)

gen3<-unite(gen2,Country.Code_year,Country.Code,year,sep='_')
gen4<-na.omit(gen3)%>%
  group_by(Country.Code_year)%>%
  summarize(ave=mean(gender_var))


ath<- read.csv("athlete_wewant.csv")
ath['num']=1

ath1<-ath%>%group_by(NOC,Year,Sex)%>%summarize(Medal_sum=sum(Medal_score),people=sum(num))
ath1<-ath1%>%filter(Year>=1990)

ath2<-spread(ath1,Sex,Medal_sum)
ath2["F_peo*medal"]<-ath2['people']*ath2["F"]
ath3=data.frame(ath2[,1:3],ath2[,5])
ath4=na.omit(ath3)
ath5<-unite(ath4,Country.Code_year,NOC,Year,sep='_')
colnames(ath5)<-c('NOC',"Malepeople","Malemedalscore")

ath6=data.frame(ath2[,1:3],ath2[,6])
ath7=na.omit(ath6)
ath8<-unite(ath7,Country.Code_year,NOC,Year,sep='_')
colnames(ath8)<-c('NOC',"Femalepeople","Femalemedalscore*people")

ath9=merge(ath5,ath8,by="NOC")
ath9["score"]=ath9[,5]/ath9[,2]
ath10=data.frame(ath9["NOC"],ath9["score"])
colnames(gen4)=c('NOC',"gen_ave")


datamerge2=merge(ath10,gen4,by="NOC")
datamerge2[datamerge2==0]<-NA
datamerge3<-na.omit(datamerge2)
ggplot(datamerge3,aes(x=gen_ave,y=score))+geom_point()+geom_smooth()+scale_y_log10()


genA <-gen1[1:263,]
genB <-gen1[264:526,]
genC <-gen1[527:789,]
genD <-gen1[790:1052,]
genE <-gen1[1053:1315,]
genF <-gen1[1316:1578,]
genG <-gen1[1579:1841,]
genH <-gen1[1842:2104,]

genA1 <-gather(genA,year,'Adjusted net enrollment rate, primary, female (% of primary school age children)',-Country.Code)
genB1 <-gather(genB,year,'Adolescent fertility rate (Adjusted)',-Country.Code)
genC1 <-gather(genC,year,'Fertility rate, total (Adjusted)',-Country.Code)
genD1 <-gather(genD,year,'Labor force, female (% of total labor force)',-Country.Code)
genE1 <-gather(genE,year,'Literacy rate, adult female (% of females ages 15 and above)',-Country.Code)
genF1 <-gather(genF,year,'Life expectancy at birth, female (years)',-Country.Code)
genG1 <-gather(genG,year,'Expected years of schooling, female',-Country.Code)
genH1 <-gather(genH,year,'Primary education, pupils (% female)',-Country.Code)

genA2<-unite(genA1,NOC,Country.Code,year,sep='_')
genB2<-unite(genB1,NOC,Country.Code,year,sep='_')
genC2<-unite(genC1,NOC,Country.Code,year,sep='_')
genD2<-unite(genD1,NOC,Country.Code,year,sep='_')
genE2<-unite(genE1,NOC,Country.Code,year,sep='_')
genF2<-unite(genF1,NOC,Country.Code,year,sep='_')
genG2<-unite(genG1,NOC,Country.Code,year,sep='_')
genH2<-unite(genH1,NOC,Country.Code,year,sep='_')

dm<-merge(datamerge3,genA2,by="NOC")
dm1<-merge(dm,genB2,by="NOC")
dm2<-merge(dm1,genC2,by="NOC")
dm3<-merge(dm2,genD2,by="NOC")
dm4<-merge(dm3,genE2,by="NOC")
dm5<-merge(dm4,genF2,by="NOC")
dm6<-merge(dm5,genG2,by="NOC")
dm7<-merge(dm6,genH2,by="NOC")

ggplot(dm7,aes(x=`Adjusted net enrollment rate, primary, female (% of primary school age children)`,y=score))+geom_point()+geom_smooth()+scale_y_log10()
ggplot(dm7,aes(x=`Adolescent fertility rate (Adjusted)`,y=score))+geom_point()+geom_smooth()+scale_y_log10()
ggplot(dm7,aes(x=`Fertility rate, total (Adjusted)`,y=score))+geom_point()+geom_smooth()+scale_y_log10()
ggplot(dm7,aes(x=`Labor force, female (% of total labor force)`,y=score))+geom_point()+geom_smooth()+scale_y_log10()
ggplot(dm7,aes(x=`Literacy rate, adult female (% of females ages 15 and above)`,y=score))+geom_point()+geom_smooth()+scale_y_log10()
ggplot(dm7,aes(x=`Life expectancy at birth, female (years)`,y=score))+geom_point()+geom_smooth()+scale_y_log10()
ggplot(dm7,aes(x=`Expected years of schooling, female`,y=score))+geom_point()+geom_smooth()+scale_y_log10()
ggplot(dm7,aes(x=`Primary education, pupils (% female)`,y=score))+geom_point()+geom_smooth()+scale_y_log10()
