#' @export
#' @param 
getFeeds=function(input=input){
  #   load(file="R/Data.rda")
  #   maxTime=input[length(input)]
  #   lists=input[1:(length(input)-1)]
  #   lists=LIST[is.element(names(LIST),lists)]
  #   lists=gsub("feed://","http://",lists)
  #   list(channel=paste(lists,collapse=""))
  #   list(channel=input)
  list(channel=getwd())
}