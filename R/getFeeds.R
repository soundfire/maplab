#' @export
#' @param 
getFeeds=function(input=input){
  
  map=Leaflet$new()
  
  data(Data)
  maxTime=input[length(input)]
  lists=input[1:(length(input)-1)]
  lists=LIST[is.element(names(LIST),lists)]
  lists=gsub("feed://","http://",lists)
  list(channel=paste(lists,map,collapse=""))
}