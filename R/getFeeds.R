#' @export
#' @param 
getFeeds=function(input=input){
  data(Data)
  library(XML)
  library(RJSONIO)
  library(rCharts)
  library(geosphere)
  library(rworldmap)
  library(RColorBrewer)
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
          CORR[matches,matches]=CORR[matches,matches]+1
          aux[matches,i]=aux[matches,i]+1
          print(matches)
          doc_=data.frame(title=doc_,adrs=doc[j]$item$guid$text,match=ifelse(length(matches)>0,matches,0))
          # Doc=c(Doc,doc_)
          DOCS=rbind(DOCS,doc_)
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
  bezier.curve <- function(p1, p2, p3) {
    n <- seq(0,1,length.out=50)
    bx <- (1-n)^2 * p1[[1]] +
      (1-n) * n * 2 * p3[[1]] +
      n^2 * p2[[1]]
    by <- (1-n)^2 * p1[[2]] +
      (1-n) * n * 2 * p3[[2]] +
      n^2 * p2[[2]]
    data.frame(lon=bx, lat=by)
  }
  bezier.uv.arc <- function(p1, p2) {
    # Get unit vector from P1 to P2
    u <- p2 - p1
    u <- u / sqrt(sum(u*u))
    d <- sqrt(sum((p1-p2)^2))
    # Calculate third point for spline
    m <- d / 2
    h <- floor(d * .2)
    # Create new points in rotated space 
    pp1 <- c(0,0)
    pp2 <- c(d,0)
    pp3 <- c(m, h)
    mx <- as.matrix(bezier.curve(pp1, pp2, pp3))
    # Now translate back to original coordinate space
    theta <- acos(sum(u * c(1,0))) * sign(u[2])
    ct <- cos(theta)
    st <- sin(theta)
    tr <- matrix(c(ct,  -1 * st, st, ct),ncol=2)
    tt <- matrix(rep(p1,nrow(mx)),ncol=2,byrow=TRUE)
    points <- tt + (mx %*% tr)
    tmp.df <- data.frame(points)
    colnames(tmp.df) <- c("lon","lat")
    tmp.df
  }
  bezier.uv.merc.arc <- function(p1, p2) {
    # Do a mercator projection of the latitude
    # coordinates
    pp1 <- p1
    pp2 <- p2
    pp1[2] <- asinh(tan(p1[2]/180 * pi))/pi * 180
    pp2[2] <- asinh(tan(p2[2]/180 * pi))/pi * 180
    arc <- bezier.uv.arc(pp1,pp2)
    arc$lat <-  atan(sinh(arc$lat/180 * pi))/pi * 180
    arc
  }
  #####################################################
  maxTime=input[length(input)]
  lists=input[1:(length(input)-1)]
  lists=LIST[is.element(names(LIST),lists)]
  lists=gsub("feed://","http://",lists)
  docs=mineFeeds(auxnames,lists,maxTime)
  hits=which(rowSums(docs$Hit)>0)
  sPDF=getMap()  
  palette1=colorRampPalette(brewer.pal(n=9,name='YlGnBu')[3:9])(max(rowSums(docs$Hit)))
  if (!is.null(docs$CorrVec$score)){
    palette2=colorRampPalette(brewer.pal(n=9,name='Reds')[6:9])(max(docs$CorrVec$score))
  }
  json='[{"type":"FeatureCollection","features":['
  for (i in hits){
    crit=docs$Docs$match==i
    pops=paste(paste("<a href='",docs$Docs$adrs[crit],"' target='_blank'>'",
                     as.character(docs$Docs$title[crit]),"'</a>",sep=""),collapse="<br><br>")
    type=length(sPDF@polygons[[i]]@Polygons)
    if (type==1){
      xy=sPDF@polygons[[i]]@Polygons[[1]]@coords
      aux=paste('{"type":"Feature",
                "properties":{
                "region_id":',i,',
                "region_hits":',rowSums(docs$Hit)[i],',
                "region_name": "',auxnames[i,1],'",
                "style":{
                "popup": "<p> ', pops ,' </p>",
                "strokeWidth": "1px",
                "strokeOpacity": 0.1,
                "fillOpacity": 0.4,
                "color":"',palette1[rowSums(docs$Hit)[i]],'"}},
                "geometry":{
                "type":"Polygon","coordinates":  [',RJSONIO::toJSON(xy),']  }},')  
    } else {
      xy=NULL
      for (j in 1:type){
        xy=paste(xy,RJSONIO::toJSON(sPDF@polygons[[i]]@Polygons[[j]]@coords),sep='],[')
      }
      xy=paste(substr(xy,3,nchar(xy)),']')
      aux=paste('{"type":"Feature",
              "properties":{
              "region_id":',i,',
              "region_hits":',rowSums(docs$Hit)[i],',
              "region_name": "',auxnames[i,1],'",
              "style":{
              "popup": "<p> ', pops ,' </p>",
              "strokeWidth": "1px",
              "strokeOpacity": 0.1,
              "fillOpacity": 0.4,
              "color":"',palette1[rowSums(docs$Hit)[i]],'"}},
              "geometry":{
              "type":"MultiPolygon","coordinates":  [',xy,']  }},')  
    }
    json=paste(json,aux)
    cen=c(sPDF$LON[i],sPDF$LAT[i])
    cen=rbind(cen,cen+rnorm(2,sd=1e-9))
    aux=paste('{"type":"Feature",
            "properties":{
            "region_name": "',auxnames[i,1],'",
            "region_hits":',rowSums(docs$Hit)[i],',
            "style":{
            "opacity": 1,
            "weight": ',(rowSums(docs$Hit)[i]*5.5)^(0.8),',
            "color":"red"}},
            "geometry":{
            "type":"LineString","coordinates":  ',RJSONIO::toJSON(cen),'  }},')
    json=paste(json,aux)
    aux=paste('{"type":"Feature",
            "properties":{
            "region_name": "',auxnames[i,1],'",
            "region_hits":',rowSums(docs$Hit)[i],',
            "style":{
            "opacity": 0.8,
            "weight": ',(rowSums(docs$Hit)[i]*4)^(0.8),',
            "color":"black"}},
            "geometry":{
            "type":"LineString","coordinates":  ',RJSONIO::toJSON(cen),'  }},')
    json=paste(json,aux)
    kors=c(na.omit(c(pmatch(auxnames[i,1],docs$CorrVec$country),
                     pmatch(auxnames[i,1],docs$CorrVec$country2))))
    kors=c(which(!is.na(lapply(docs$CorrVec$country,function(x)pmatch(auxnames[i,1],x)))),
           which(!is.na(lapply(docs$CorrVec$country2,function(x)pmatch(auxnames[i,1],x)))))
    print(paste(auxnames[i,1],docs$CorrVec$country[kors]))
    if (length(kors)>0){
      for (j in 1:length(kors)){
        xy=bezier.uv.arc(as.numeric(docs$CorrVec[kors[j],5:6]),as.numeric(docs$CorrVec[kors[j],3:4]))
        colnames(xy)=NULL
        aux=paste('{"type":"Feature",
                "properties":{
                "style":{
                "weight":',docs$CorrVec$score[kors[j]]*2.2,',
                "opacity": 0.05,
                "color":"',palette2[docs$CorrVec$score[kors[j]]],'"},
                "region_id":',i,',
                "region_hits":',docs$CorrVec$score[kors[j]],'},
                "geometry":{
                "type":"LineString","coordinates":  ',RJSONIO::toJSON(data.matrix(xy)),'  }},')
        json=paste(json,aux)
      }
    }
  }
  json=paste(substr(json,1,nchar(json)-1),']}]')
  json=gsub("\\n","",json)
  list(channel=paste(json,collapse=""))
}