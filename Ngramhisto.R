WordNgram <- function(inp,N=2){
  ln <- list()
  for (k in 1:(length(inp)-N)){
    c <- character()
    for (p in k:(k+N)){
      c <- paste(names(inp[p]),c)
      ln[[k]] <- c
    }
  }
  ln
}

#  res <- sapply(youyaku_k,function(a){WordNgram(a,N)})
#  rl <- sapply(res,function(i){rle(as.character(i))})
#  rl <- sapply(res,function(i){rle(sort(unlist(i)))})
#  dims<- rle(sort(unlist(res[[1]])))

Ngramhisto <- function(ll){
  if(!is.list(ll)){
    print("arg is not list") 
  }else{
  res <- sapply(ll,function(a){WordNgram(a,N)})
  histo <- list()
  for (i in 1:length(res)){
    histo[[i]]<- rle(sort(unlist(res[[i]])))
  }
}
  histo 
}

getdfhistos <- function(histos){
  if(!is.list(histos)){
    print("arg is not list") 
  }else{
    nhistos <- sapply(histos,function(i){
      df<-data.frame(t(i$length))
      colnames(df)<-t(i$values)
      df
    })
  }
}

mergedflist <- function(nhistos){
  res <- data.frame()
  for(i in 1:length(nhistos)){
    res<-merge(res,nhistos[[i]],all=T)
  }
  res[is.na(res)] <- 0

  res
}

uniquedim <- function(histos){
  if(!is.list(histos)){
    print("arg is not list") 
  }else{
    totlist <- list()
#    for (h in 1:length(histos)){
#     totlist <- c(totlist,histos[[h]]$values)
#    }
    totlist <- sapply(histos,function(a){a$values})
    totlist <- sort(unlist(totlist))
    totlist <- unique(totlist)
    as.list(totlist)
  }
}

## redimhist <- function(histos,newdims){
##   if(!is.list(histos)||(!is.list(newdims))){
##     print("args are not list") 
##   }else{
##     newhistos <- sapply(histos,
##                         function(h){
##                           l <- list
                         
##                           for (hi in 1:length(h)){

##                         }
##                         )
##     newhistos
##   }
#}
