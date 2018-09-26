# R 基本概念：變數型態

#1.Numbers

x<- 1+5
x
rm(x)    #刪除
?rm
x


x<- 1+5
class(x)
is.numeric(x)
is.character(x)




# 2. Characters
y<-"MYNTU"
nchar(y)     #nchar可以知道類別項目有幾個字元
is.numeric(y)
is.character(y)
is.matrix(y)
is.vector(y)

# 3. Date
z<-as.Date ("2015-02-01")
class(z)
is.numeric(z)
ti<-as.numeric(z)

# since 1970,Jan, 1
ti    #UNIX時間，或稱POSIX時間是UNIX或類UNIX系統使用的時間表示方式：從1970年1月1日


####   R 基本概念：資料結構

#  vector
X1<-c(1,2,3,4)
X1
#  Matrix
X2<-matrix(1:12, nrow=2)
X2
#  Array
X3<-array(1:12, dim=c(2,3,2))
X3
#  Data Frame
x1=c(1:10)
y1=c(11:20)
z1=c(21:30)

DF1<-data.frame(XX=x1,YY=y1,ZZ=z1)   #換欄位的名字
DF1

#  List     一串雜記，可以想像是一張筆記紙
LIST1 <- list(gender="man", age=18, hobby=c("tease", "be teased"))
LIST1

L1<-list(TheDataFrame=DF1,TheVECTOR=1:10,TheLIST=LIST1)  #把dataframe、vector、list通放進來
L1


#  Factor  factor的型態，主要用來表有等級或類別屬性的資料(category variable)。
#(例如：性別(男、女)，年級(小一、小二….碩一、碩二)，地區(北、中、南、東)…等等。)
#用R進行資料分析時，當遇到這類「類別變數」時，要轉換成factor的資料型態，再丟入模型(model)進行分析

grade <- c("Poor","Excellent","Improved","Excellent","Improved")  # 建立一個character vector
levels(grade)   #沒有先factor過，無法運算

grade <- factor(grade)   # 轉換成factor型態
levels(grade)                     # Levels的屬性代表: 在這個變數裡面，存在哪些類別
#如果有一組類別變數，要查看裡面存在著哪些類別，可以用levels()函式：


##############################
G = factor(grade, level=c("Poor","Improved","Excellent"), order=TRUE)
G
levels(G)






############讀取檔案
getwd() # get working directory
#setwd("D:/107-1 Statistics/lab2") 
Student <- read.csv("Student.csv")
##Student <-read.table("Student.csv", header=TRUE, sep=",")   # header=True 資料的第一筆是否為變數名稱

head(Student)
class(Student)

###########

length(Student) # number of fields
nrow(Student) # number of records



##########做類別資料的圖
# Pie Chart
counts <- Student$ReligImp
counts

counts <- table(Student$ReligImp)    #"table是負責把資料整理為一個列連表的形式
#如果沒有用table，出來的資料是沒有經過整理的一大段資料)"
counts

pct <- round(counts/sum(counts)*100)  #round功能：去掉小數的部分且進位
#a = counts/sum(counts)
#a
pct

#指令：round(x, digits=n)
#說明：其中n為設定留下的位數
#範例：round(3.855, digits=2) 得到 3.86
#pct <- round(counts/sum(counts)*100,digits=2)  
#pct
counts

lbls <- rownames(counts)
lbls
lbls <- paste(lbls, pct) # add percents to labels
lbls
lbls <- paste(lbls,"%",sep="_") # ad % to labels  sep"放入想要分隔的符號"
lbls

counts
pie(counts, labels = lbls, col=c("yellow","red","blue"), main="ReligImp") 

pie(counts)




# Bar Plot
barplot(counts, main="Students", col=rainbow(4),
        legend = rownames(counts),xlab="ReligImp")



# Crosstabs 列連表
Sex<-Student$Sex
Sex
ReligImp<-Student$ReligImp

mytable <- xtabs(~Sex+ReligImp)   #xtabs() 裡面放入想要做列連表的變數
mytable


# Stacked Bar Plot
barplot(mytable, col=c("red","blue"), main="Comparisons of ReligImp",
        xlab="ReligImp", ylab="No. of Students",legend = rownames(mytable))


barplot(mytable)     #一樣可以產生圖，但顏色為預設，也沒有辦法產生標題或X、Y軸名稱



# Grouped Bar Plot
barplot(mytable, col=c("red","blue"), main="Comparisons of ReligImp",
        xlab="ReligImp", ylab="No. of Students",legend = rownames(mytable),
        beside=TRUE)



# Histogram ---------------------------------------------------------------

hist(Student$GPA, breaks=10, col="blue", main="GPA Distribution", xlab="GPA")

d<-density(Student$GPA, na.rm=TRUE)
plot(d, col="red", main="GPA Distribution", xlab="GPA")


# Boxplot -----------------------------------------------------------------

boxplot(GPA~Sex, Student=Student, col=(c("red","blue")), 
        main="Comparisons of GPA", xlab="Gender", ylab="GPA" )



# Stem-and-leaf plot ------------------------------------------------------

GPA<- Student$GPA
summary(GPA)

stem(GPA, scale = 2)
stem(GPA, scale = 1)


# Dot plot ----------------------------------------------------------------

GPA<- Student$GPA
dotchart(GPA, cex=1, lcolor = NULL)

# or
stripchart(GPA, method = "stack", offset = .3, at = .15, pch = 19,
           main = "Dotplot of GPA", xlab = "GPA")

hist(GPA, breaks = 50, col = "blue")


# Desciptive statistics ---------------------------------------------------

length(GPA)
summary(GPA) # mean, median, Q25 and Q75, min, max

mean(GPA)
mean(GPA, na.rm = T)
var(GPA, na.rm = T) # sample variance
sd(GPA, na.rm = T) # sample SD
IQR(GPA, na.rm = T)


# 
is.na(GPA)
length(GPA[is.na(GPA)])
GPA[is.na(GPA)] = 0
length(GPA[is.na(GPA)])


# How about standard deviation of population?
s = sd(GPA, na.rm = T)
sqrt( (s^2)*(657-1)/657 )


























