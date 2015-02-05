#' @export
#' @param 
getFeeds=function(args=input){
  load(file="R/Data.RData")
  maxTime=input[length(input)]
  lists=input[1:(length(input)-1)]
  lists=LIST[is.element(names(LIST),lists)]
  lists=gsub("feed://","http://",lists)
  list(channel=paste(lists,collapse=""))
}