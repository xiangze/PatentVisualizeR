library(kernlab)
source("Ngramhisto.R")

cvnum <- 4

#posvecs <- read.csv("data/2golist_psize30.txt",col.names=F)
#negvecs <- read.csv("novels/2golist_psize30.txt",col.names=F)
#posvecs <- read.csv("data/3golist.txt",col.names=F)
#negvecs <- read.csv("novels/3golist_psize30.txt",col.names=F)

posvecs <- read.csv(paste("data/",N,"golist_psize",psize,".txt",sep=""),col.names=F)
negvecs <- read.csv(paste("novels/",N,"golist_psize",psize,".txt",sep=""),col.names=F)

pos <- apply(posvecs,1, read.csv)
totpos <-mergedflist(pos)
totpos$Class <- rep("patent",nrow(totpos))
#write.csv(totpos,"totpos.csv")
print("merged pos")

neg <- apply(negvecs,1, read.csv)

for(i in 1:length(neg)){
  neg[[i]] <- neg[[i]][1:(nrow(totpos)/length(neg)),]
}

totneg <-mergedflist(neg)
totneg$Class <- rep("nonpatent",nrow(totneg))
#write.csv(totneg,paste("totneg_",N,"psize",".csv",sep=""))
print("merged neg")

#tott <-mergedflist(list(totpos,totneg))
tott<-merge(totpos,totneg,all=T)
tott[is.na(tott)] <- 0

print("merged")
write.csv(tott,paste("toty_",N,"psize",psize,".csv",sep=""))
idx <- sample(1:cvnum, length(tott$Class), replace=T)

pred <- rep(NA, length(tott$Class))

for (i in 1:cvnum) {
  print(paste("cv",i))
  is.test <- idx == i
  train <- tott[!is.test,]
  test<-subset(tott[is.test,],select=-Class)
  svmres <- ksvm(Class ~., data=train)
#  svmres <- ksvm(Class ~., data=train,kernel="polydot", kpar=list(degree=2))
#  svmres <- ksvm(Class ~., data=train,epsilon=0.01,kpar=list(sigma=16),cross=3)
  pred[is.test] <- predict(svmres, test)
}
table(tott$Class, pred)
