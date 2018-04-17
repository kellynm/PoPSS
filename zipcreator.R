zipcreator <- function(dataList, path) {
  write.csv(dataList[[1]], file = "output/data.csv")
  for (ix in 2:length(dataList)) {
    p <- ix-1
    writeRaster(dataList[[ix]], file = paste("output/host",p,".tif",sep = ""), overwrite=TRUE)
  }
  files2zip <- dir("output", full.names = TRUE)
  zip(zipfile = "C:/Users/Chris/Desktop/testZip2.zip", files = files2zip)
  do.call(file.remove, list(list.files("output", full.names = TRUE)))
}