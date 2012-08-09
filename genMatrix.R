library(ggplot2)

#pdata<-read.csv("data/液晶_補償回路_補正回路.csv")
pdata<-read.csv(filename)
pdata_sm <- pdata[1:100,]
pdata_sm$IPCs<-gsub("\\|(.*)","",pdata_sm$IPC.コード)

pdata_sm$出願人 <- sapply(pdata_sm$出願人,function(s){gsub("株式会社","",s)})
出願人tops<-table(pdata_sm$出願人)
出願人tops<-tail(sort(出願人tops))
#dimnames(出願人tops)<-lapply(dimnames(出願人tops),function(s){gsub("株式会社","",s)})

pdata_tops <- data.frame()
for (i in rownames(出願人tops)){
  pdata_tops<-rbind(subset(pdata_sm,subset= 出願人==i),pdata_tops)
}

d<-ggplot(pdata_tops,aes(x=出願人 ,y=IPCs))
png(paste(filename,".png",sep=""),width=800,height=600)
d+stat_sum(aes(group=1))
dev.off()


