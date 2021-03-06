library(bitops)
library(httr)
library(RCurl)
library(tm)
library(NLP)
library(tmcn)
library(factoextra)
library(jiebaRD)
library(jiebaR)
library(devtools)
install_github("ggbiplot", "vqv")
library(scales)
library(grid)
library(ggbiplot)
library(XML)
library(knitr)


# INPUT DATA
d.corpus <- Corpus( DirSource("./DATA" ))
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})


# TDM詞頻矩陣
mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

n = length(seg)
TDM = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}
TDM[is.na(TDM)] <- 0
kable(head(TDM))


# TF-IDF
tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum)

library(Matrix)
idfCal <- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal)

doc.tfidf <- TDM
tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX

stopLine = rowSums(doc.tfidf[,2:(n+1)])
delID = which(stopLine == 0)


t = as.data.frame(t(doc.tfidf))
t = t[-1,]
t = apply(t[,1:100], 2, as.numeric)
t = apply(t[,1:100], 1, as.numeric)
t=data.frame(t)

t_pca = prcomp(t)
p <- ggbiplot(t_pca, obs.scale = 1, var.scale = 1,ellipse = TRUE, circle = TRUE)
p <- p + scale_color_discrete(name = '')
p <- p + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
p
fviz_eig(t_pca)
fviz_pca_ind(t_pca, geom.ind = c("point"), col.ind = "cos2")
fviz_pca_var(t_pca, col.var = "contrib",repel = TRUE)
fviz_pca_biplot(t_pca, geom.ind = "point",repel = TRUE)
t.eig <- get_eig(t_pca)
t.var <- get_pca_var(t_pca)
t.ind <- get_pca_ind(t_pca)

ind.coord2 <- t.ind$coord[, 1:2]
wss <- c()
for (i in 1:10) { wss[i] <- kmeans(ind.coord2, i)$tot.withinss }
plot(wss, type = "b")

kmeansData = t_pca$rotation[,1:2]
cl <- kmeans(kmeansData, 2)
plot(kmeansData, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)
