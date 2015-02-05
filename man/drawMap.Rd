\name{drawMap}
\alias{drawMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
drawMap
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
drawMap(input, width = "100%", height = "100%")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (input, width = "100\%", height = "100\%") 
{
    map = Leaflet$new()
    map$set(width = width, height = height)
    map$fullScreen(TRUE)
    map$setView(c(20, 0), zoom = 3, maxZoom = 10)
    map$tileLayer(provider = "Stamen.Toner")
    maxTime = input$maxTime
    lists = input$selectFeeds
    lists = gsub("feed://", "http://", lists)
    auxnames = getNames()
    docs = mineFeeds(auxnames, lists, maxTime)
    hits = which(rowSums(docs$Hit) > 0)
    sPDF = getMap()
    palette1 = colorRampPalette(brewer.pal(n = 9, name = "YlGnBu")[3:9])(max(rowSums(docs$Hit)))
    if (!is.null(docs$CorrVec$score)) {
        palette2 = colorRampPalette(brewer.pal(n = 9, name = "Reds")[6:9])(max(docs$CorrVec$score))
    }
    json = "{\"type\":\"FeatureCollection\",\"features\":["
    for (i in hits) {
        type = length(sPDF@polygons[[i]]@Polygons)
        if (type == 1) {
            xy = sPDF@polygons[[i]]@Polygons[[1]]@coords
            aux = paste("{\"type\":\"Feature\",\n                \"properties\":{\n                \"region_id\":", 
                i, ",\n                \"region_hits\":", rowSums(docs$Hit)[i], 
                ",\n                \"region_name\": \"", auxnames[i, 
                  1], "\",\n                \"style\":{\n                \"strokeWidth\": \"1px\",\n                \"strokeOpacity\": 0.1,\n                \"fillOpacity\": 0.4,\n                \"color\":\"", 
                palette1[rowSums(docs$Hit)[i]], "\"}},\n                \"geometry\":{\n                \"type\":\"Polygon\",\"coordinates\":  [", 
                RJSONIO::toJSON(xy), "]  }},")
        }
        else {
            xy = NULL
            for (j in 1:type) {
                xy = paste(xy, RJSONIO::toJSON(sPDF@polygons[[i]]@Polygons[[j]]@coords), 
                  sep = "],[")
            }
            xy = paste(substr(xy, 3, nchar(xy)), "]")
            aux = paste("{\"type\":\"Feature\",\n              \"properties\":{\n              \"region_id\":", 
                i, ",\n              \"region_hits\":", rowSums(docs$Hit)[i], 
                ",\n              \"region_name\": \"", auxnames[i, 
                  1], "\",\n              \"style\":{\n              \"strokeWidth\": \"1px\",\n              \"strokeOpacity\": 0.1,\n              \"fillOpacity\": 0.4,\n              \"color\":\"", 
                palette1[rowSums(docs$Hit)[i]], "\"}},\n              \"geometry\":{\n              \"type\":\"MultiPolygon\",\"coordinates\":  [", 
                xy, "]  }},")
        }
        json = paste(json, aux)
        cen = c(sPDF$LON[i], sPDF$LAT[i])
        cen = rbind(cen, cen + rnorm(2, sd = 1e-09))
        aux = paste("{\"type\":\"Feature\",\n            \"properties\":{\n            \"region_name\": \"", 
            auxnames[i, 1], "\",\n            \"region_hits\":", 
            rowSums(docs$Hit)[i], ",\n            \"style\":{\n            \"opacity\": 1,\n            \"weight\": ", 
            (rowSums(docs$Hit)[i] * 5.5)^(0.8), ",\n            \"color\":\"red\"}},\n            \"geometry\":{\n            \"type\":\"LineString\",\"coordinates\":  ", 
            RJSONIO::toJSON(cen), "  }},")
        json = paste(json, aux)
        aux = paste("{\"type\":\"Feature\",\n            \"properties\":{\n            \"region_name\": \"", 
            auxnames[i, 1], "\",\n            \"region_hits\":", 
            rowSums(docs$Hit)[i], ",\n            \"style\":{\n            \"opacity\": 0.8,\n            \"weight\": ", 
            (rowSums(docs$Hit)[i] * 4)^(0.8), ",\n            \"color\":\"black\"}},\n            \"geometry\":{\n            \"type\":\"LineString\",\"coordinates\":  ", 
            RJSONIO::toJSON(cen), "  }},")
        json = paste(json, aux)
        kors = c(na.omit(c(pmatch(auxnames[i, 1], docs$CorrVec$country), 
            pmatch(auxnames[i, 1], docs$CorrVec$country2))))
        kors = c(which(!is.na(lapply(docs$CorrVec$country, function(x) pmatch(auxnames[i, 
            1], x)))), which(!is.na(lapply(docs$CorrVec$country2, 
            function(x) pmatch(auxnames[i, 1], x)))))
        print(paste(auxnames[i, 1], docs$CorrVec$country[kors]))
        if (length(kors) > 0) {
            for (j in 1:length(kors)) {
                xy = bezier.uv.arc(as.numeric(docs$CorrVec[kors[j], 
                  5:6]), as.numeric(docs$CorrVec[kors[j], 3:4]))
                colnames(xy) = NULL
                aux = paste("{\"type\":\"Feature\",\n                \"properties\":{\n                \"style\":{\n                \"weight\":", 
                  docs$CorrVec$score[kors[j]] * 2.2, ",\n                \"opacity\": 0.05,\n                \"color\":\"", 
                  palette2[docs$CorrVec$score[kors[j]]], "\"},\n                \"region_id\":", 
                  i, ",\n                \"region_hits\":", docs$CorrVec$score[kors[j]], 
                  "},\n                \"geometry\":{\n                \"type\":\"LineString\",\"coordinates\":  ", 
                  RJSONIO::toJSON(data.matrix(xy)), "  }},")
                json = paste(json, aux)
            }
        }
    }
    polys = RJSONIO::fromJSON(paste(substr(json, 1, nchar(json) - 
        1), "]}"))
    map$geoJson(polys, style = paste("#! function(feature) {return feature.properties.style;} !#", 
        sep = ""), onEachFeature = paste("#!", paste(readLines("oef.js"), 
        collapse = "\n"), "!#"))
    pindex = round(seq(from = 1, to = length(palette1), length.out = min(10, 
        length(palette1))))
    palette1 = palette1[pindex]
    legend_vec = as.character(pindex)
    names(legend_vec) = palette1.legend
    map$legend(position = "bottomleft", colors = names(legend_vec), 
        labels = as.vector(legend_vec))
    return(map)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
