library(stringr)
dm5 <-read.csv("dm5.csv")
#any(is.na(gender))
#summary(gender)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(rpart)
library(DMwR)
library(grid)


###plot

ggplot(dm5,aes(x=gen_ave,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()

ggplot(dm5,aes(x=enrollment,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()
ggplot(dm5,aes(x=adolescent_fertility,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()
ggplot(dm5,aes(x=Labor,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()
ggplot(dm5,aes(x=Schooling_years,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()
ggplot(dm5,aes(x=Primary_edu,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()
ggplot(dm5,aes(x=Employer,y=Fscore))+geom_point()+geom_smooth(method="lm")+scale_y_log10()+scale_x_log10()

###analysis

m6 <- lm(Fscore~+enrollment+adolescent_fertility+Labor+Schooling_years+Primary_edu+Employer,data=dm5)

anova(m6)

regtree <- rpart(dm5$Fscore~., data=dm5[,9:11])
prettyTree(regtree)

