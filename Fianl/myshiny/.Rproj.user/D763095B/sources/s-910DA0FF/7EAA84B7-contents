library(stringr)
gender <-read.csv("Gender_wewant1.csv")
any(is.na(gender))
#summary(gender)
library(dplyr)
library(tidyr)
library(ggplot2)
colnames(gender)<-c("Country.Code","Country.Name","Series.Name",as.character(c(1992:2016)))
gen1<-cbind(gender[,1],gender[,4:28])

#gen1[264:526,2:26]<-(gen1[264:526,2:26]*(-1)+1000)/10
#gen1[1316:1578,2:26]<-gen1[1316:1578,2:26]*5

colnames(gen1)<-c("Country.Code",as.character(c(1992:2016)))
gen2<-gather(gen1,year,gender_var,-Country.Code)

gen3<-unite(gen2,Country.Code_year,Country.Code,year,sep='_')
gen4<-na.omit(gen3)%>%
  group_by(Country.Code_year)%>%
  summarize(ave=mean(gender_var))


ath<- read.csv("athlete_wewant.csv")
ath['num']=1

ath1<-ath%>%filter(Year>=1990)%>%group_by(NOC,Year,Sex)%>%summarize(Medal_sum=sum(Medal_score),people=sum(num))
ath1A<-ath%>%filter(Year>=1990)%>%group_by(NOC,Year)%>%summarize(join_people=sum(num))
ath1B<-unite(ath1A,NOC,NOC,Year,sep='_')

ath2A<-ath1%>%filter(Sex=="F")
ath2B<-unite(ath2A,NOC,NOC,Year,sep='_')
ath2C<-data.frame(ath2B[,1],ath2B[,3])

ath3<-merge(ath2C,ath1B,by="NOC")
ath3["Fscore"]=ath3[,2]/ath3[,3]

#ath2<-spread(ath1,Sex,Medal_sum)
#ath2["F_peo*medal"]<-ath2['people']*ath2["F"]
#ath3=data.frame(ath2[,1:3],ath2[,5])
#ath4=na.omit(ath3)
#ath5<-unite(ath4,Country.Code_year,NOC,Year,sep='_')
#colnames(ath5)<-c('NOC',"Malepeople","Malemedalscore")

#ath6=data.frame(ath2[,1:3],ath2[,6])
#ath7=na.omit(ath6)
#ath8<-unite(ath7,Country.Code_year,NOC,Year,sep='_')
#colnames(ath8)<-c('NOC',"Femalepeople","Femalemedalscore*people")

#ath9=merge(ath5,ath8,by="NOC")
#ath9["score"]=ath9[,5]/ath9[,2]
#ath10=data.frame(ath9["NOC"],ath9["score"])

colnames(gen4)=c('NOC',"gen_ave")


datamerge2=merge(ath3,gen4,by="NOC",all=T)
datamerge3<-na.omit(datamerge2)
datamerge4<-datamerge3%>%filter(gen_ave<=65)
ggplot(datamerge4,aes(x=gen_ave,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()


genA <-gen1[1:263,]
genB <-gen1[264:526,]
genC <-gen1[527:789,]
genD <-gen1[790:1052,]
genE <-gen1[1053:1315,]
genF <-gen1[1316:1578,]

genA1 <-gather(genA,year,'Adjusted net enrollment rate, primary, female (% of primary school age children)',-Country.Code)
genB1 <-gather(genB,year,'Adolescent fertility rate (Adjusted)',-Country.Code)
genC1 <-gather(genC,year,'Labor force, female (% of total labor force)',-Country.Code)
genD1 <-gather(genD,year,'Expected years of schooling, female',-Country.Code)
genE1 <-gather(genE,year,'Primary education, pupils (% female)',-Country.Code)
genF1 <-gather(genF,year,'Employers, female (% of female employment) (modeled ILO estimate)',-Country.Code)

genA2<-unite(genA1,NOC,Country.Code,year,sep='_')
genB2<-unite(genB1,NOC,Country.Code,year,sep='_')
genC2<-unite(genC1,NOC,Country.Code,year,sep='_')
genD2<-unite(genD1,NOC,Country.Code,year,sep='_')
genE2<-unite(genE1,NOC,Country.Code,year,sep='_')
genF2<-unite(genF1,NOC,Country.Code,year,sep='_')

dm<-merge(datamerge4,genA2,by="NOC")
dm1<-merge(dm,genB2,by="NOC")
dm2<-merge(dm1,genC2,by="NOC")
dm3<-merge(dm2,genD2,by="NOC")
dm4<-merge(dm3,genE2,by="NOC")
dm5<-merge(dm4,genF2,by="NOC")

ggplot(dm5,aes(x=`Adjusted net enrollment rate, primary, female (% of primary school age children)`,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()
ggplot(dm5,aes(x=`Adolescent fertility rate (Adjusted)`,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()
ggplot(dm5,aes(x=`Labor force, female (% of total labor force)`,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()
ggplot(dm5,aes(x=`Expected years of schooling, female`,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()
ggplot(dm5,aes(x=`Primary education, pupils (% female)`,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()
ggplot(dm5,aes(x=`Employers, female (% of female employment) (modeled ILO estimate)`,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()

summary(dm5)
