#' @export
#' @param 
getFeeds=function(input=input){
  
  library(rCharts)
  library(XML)
  library(RColorBrewer)
  library(geosphere)
  library(rworldmap)
  #####################################################
  mineFeeds=function(auxnames,lists,maxTime){
    Sys.setlocale("LC_TIME", "C")
    now=format(Sys.time(),tz="GMT",format="%Y-%m-%d %H:%M:%S")
    sPDF=getMap()
    names=sPDF$NAME_SORT
    aux=matrix(0,length(names),length(lists))
    DOCS=NULL
    DOCSall=NULL
    CORR=matrix(0,nrow=length(names),ncol=length(names))
    colnames(CORR)=names
    rownames(CORR)=names
    for (i in 1:length(lists)){
      doc=xmlToList(lists[i])
      if (dim(summary(doc))[1]<10){
        doc=doc$channel
      }
      a=summary(doc)
      Doc=NULL
      for (j in which(rownames(a)=="item")){
        datum=doc[j]$item$pubDate
        if (is.null(datum)){
          datum=doc[j]$item$date
          if (sum(grep("T",datum))>0){
            datum=strsplit(datum,"T")[[1]]
          }
          datum=paste(as.Date(datum[1],tz="GMT",format="%Y"),
                      gsub("Z","",datum[2]))
          datum=format(as.POSIXct(datum),tz="GMT",format="%Y-%m-%d %H:%M:%S")
        } else {
          if (sum(grep(",",datum))>0){
            datum=strsplit(datum,", ")[[1]][2]
          }
          datum=paste(as.Date(datum,tz="GMT",format="%d %b %Y"),
                      strsplit(datum,format(Sys.Date(),format="%Y"))[[1]][2])
          datum=format(as.POSIXct(datum,tz="GMT",format="%Y-%m-%d %H:%M:%S"))
        }
        datumaux=difftime(now,datum,units="hours")<maxTime
        doc_=doc[j]$item$description[[1]]
        if (datumaux&!is.null(doc_)){
          doc_=gsub(">" ,"> ",doc_)
          doc_=strsplit(doc_,"<a")[[1]][1]
          doc_=strsplit(doc_,"<img")[[1]][1]
          doc_=gsub("-" ," " ,doc_)
          doc_=gsub("\n"," " ,doc_)
          doc_=gsub("\""," " ,doc_)
          doc_=gsub("," ,""  ,doc_)
          matches=unique(auxnames$index[
            which(sapply(paste(auxnames$name," ",sep=""),regexpr,doc_,ignore.case=F)>-1)])
          #         print(matches)
          CORR[matches,matches]=CORR[matches,matches]+1
          aux[matches,i]=aux[matches,i]+1
          Doc=c(Doc,doc_)
          DOCS=c(DOCS,doc_)
        }
      }
      print(paste(sum(aux),lists[i]))
      DOCSall=c(DOCSall,doc)
    }
    CORRvec=NULL
    orgnames=sPDF$NAME_SORT
    CORR[lower.tri(CORR)]=0
    a=which(CORR>0,arr.ind=T)
    a=a[!a[,1]==a[,2],]
    a=matrix(a,ncol=2)
    if (nrow(a)>0){
      a=matrix(a[setdiff(1:dim(a)[1],data.frame(which(a==207,arr.ind=T))$row),],ncol=2)
      for (i in 1:dim(a)[1]){
        CORRvec$country[i]=paste(rownames(CORR)[a[i,1]],colnames(CORR)[a[i,2]])
        CORRvec$country2[i]=paste(rownames(CORR)[a[i,2]],colnames(CORR)[a[i,1]])
        CORRvec$lon1[i]=sPDF$LON[which(orgnames==rownames(CORR)[a[i,1]])]
        CORRvec$lat1[i]=sPDF$LAT[which(orgnames==rownames(CORR)[a[i,1]])]
        CORRvec$lon2[i]=sPDF$LON[which(orgnames==rownames(CORR)[a[i,2]])]
        CORRvec$lat2[i]=sPDF$LAT[which(orgnames==rownames(CORR)[a[i,2]])]
        CORRvec$score[i]=CORR[a[i,1],a[i,2]]
      }
      CORRvec=data.frame(CORRvec)
      CORRvec=CORRvec[order(CORRvec$score,decreasing=T),]
    }
    elapsedtime=difftime(format(Sys.time(),tz="GMT",format="%Y-%m-%d %H:%M:%S"),now)
    print(elapsedtime)
    out=NULL
    out$Hit=aux
    out$CorrVec=CORRvec
    out$Docs=DOCS
    out$DocsAll=DOCSall
    out$time=elapsedtime
    return(out)
  }
  #####################################################
  
  
  data(Data)
  maxTime=input[length(input)]
  lists=input[1:(length(input)-1)]
  lists=LIST[is.element(names(LIST),lists)]
  lists=gsub("feed://","http://",lists)
  
  docs=mineFeeds(auxnames,lists,maxTime)
  hits=which(rowSums(docs$Hit)>0)
  list(channel=paste(hits,collapse=""))
}