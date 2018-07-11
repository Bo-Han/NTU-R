
library(ggplot2)


library(dplyr)

ggplot(data=iris,aes(x=Species))+geom_bar()

ggplot(data=iris,aes(x=Sepal.Width))+geom_histogram()

ggplot(data=iris,aes(x=Sepal.Width,y=Sepal.Length,color=Species,size=Petal.Length))+geom_point()

ggplot(data=iris,aes(x=Sepal.Width,y=Sepal.Length))+geom_point()+facet_wrap(~Species)

ggplot(data=iris,aes(x=Species,y=Sepal.Length))+geom_boxplot()