library(bitops)
library(httr)
library(RCurl)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)
library(devtools)
install_github("ggbiplot", "vqv")
library(scales)
library(grid)
library(ggbiplot)
library(XML)
library(knitr)

d.corpus <- Corpus( DirSource("./DATA") )
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})



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


