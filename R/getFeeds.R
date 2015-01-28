# #' @export
# #' @param 
# getFeeds <- function(...){
# #   lists=c("feed://rss.nytimes.com/services/xml/rss/nyt/World.xml",
# #           "feed://feeds.bbci.co.uk/news/world/rss.xml",
# #           "feed://rss.dw.de/rdf/rss-en-all",
# #           "http://feeds.washingtonpost.com/rss/homepage",
# #           "feed://rss.cnn.com/rss/edition.rss",
# #           "feed://www.aljazeera.com/Services/Rss/?PostingId=2007731105943979989",
# #           "feed://rss.feedsportal.com/c/266/f/3503/index.rss",
# #           "http://www.voanews.com/api/epiqq",
# #           "http://www.npr.org/rss/rss.php?id=1001",
# #           "http://online.wsj.com/xml/rss/3_7085.xml",
# #           "http://feeds.theguardian.com/theguardian/world/rss")
# #   lists=gsub("feed://","http://",lists)
#   lists="fuck"
#   list(lists=lists)
# }


#' Hello World
#' 
#' Basic hello world function to be called from the demo app
#' 
#' @export
#' @param myname your name. Required.
hello <- function(myname = ""){
  if(myname == ""){
    stop("Tell me your name!")
  }
  list(
    message = paste("hello", myname, "! This is", R.Version()$version.string)
  )
}