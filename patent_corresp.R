library(RMeCab)
library(MASS)

remove.pat <- function(d,s){
  d <- apply(as.data.frame(d),1,function(a){gsub(s,"",a)})
}

pdat<-read.csv(filename)
pdats<-pdat[1:10,]
pdats$抄録又は要約 <- remove.pat(pdats$抄録又は要約,"【(.){1,4}】")
pdats$抄録又は要約 <- remove.pat(pdats$抄録又は要約,"図.")

## youyaku <- pdat$抄録又は要約
## youyaku <- as.data.frame(youyaku[1:10])
## youyaku_a <- apply(youyaku,1,function(a){gsub("【(.){1,4}】","",a)})
#youyaku_k <-  RMeCabDF(as.data.frame(youyaku_a))

出願人s<-table(pdat$出願人)
出願人tops<-tail(sort(出願人s))

popnum <- 10

negwords <-list()
poswords <-list()

for (i in rownames(出願人tops)){
#  words <- subset(pdata_tops,subset= 出願人==i)$抄録又は要約
}
poswords <-unique(poswords)


df <- data.frame()
for (i in rownames(出願人tops)){
  yous <- subset(pdat,subset= 出願人==i)$抄録又は要約
  res<-list()
  for (j in 1:length(yous)){
    res <- c(res,RMeCabC(yous[[j]]))
  }  
  res2<-list()
  for (j in 1:length(res)){
#    if(names(res[[j]])=="名詞" | names(res[[j]])=="形容詞"){
    if(names(res[[j]])=="名詞"){
    res2 <- c(res2,res[[j]])
    }
  }
  t1 <- table(unlist(res2))

  if(show){
    printf(t1)
  }else{
  t3 <- t1[t1>popnum]
#  t3 <- t2[names(t2) %in%  poswords]
  df <- rbind(df,
              data.frame(words=names(t3),id=rep(i,length(t3)),comp=t3)
              )
}
  
}

if(!show){
  cdocs <- xtabs(comp~words+id ,data=df)
  res <- corresp(cdocs,nf=2)
  png(paste(filename,"corr.png",sep=""),width=800,height=600)
  biplot(res,xlim=c(-4,4),ylim=c(-4,4),cex = c(0.5, 1))
  dev.off()
}

