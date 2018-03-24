getUnit <- function(r, dataForPlot) {
  unit <- gsub(" ","",substr(crs(r)@projargs,gregexpr("+units=", crs(r)@projargs)[[1]][1]+attributes(gregexpr("+units=", crs(r)@projargs)[[1]])$match.length,gregexpr("+units=", crs(r)@projargs)[[1]][1]+attributes(gregexpr("+units=", crs(r)@projargs)[[1]])$match.length+1))
  if (unit =='m'){
    dataForPlot$Area <<- dataForPlot$Area/10000
  } else if (unit =='km') {
    dataForPlot$Area <<- dataForPlot$Area/0.1
  } else if (unit == 'ft'){
    dataForPlot$Area <<- dataForPlot$Area/107640
  }  else if (unit == 'ac'){
    dataForPlot$Area <<- dataForPlot$Area/2.4711
  }  else if (unit == 'mi'){
    dataForPlot$Area <<- dataForPlot$Area/0.003861
  } else {
    dataForPlot$Area <<- dataForPlot$Area
  }
  return(unit)
}