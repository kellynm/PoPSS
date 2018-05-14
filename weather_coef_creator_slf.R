library(rgdal)
library(raster)
library(ncdf4)

#years of interest:
start <- 2012
end <- 2017
time_range <- seq(start, end, 1)
directory <- "G:/DaymetUS"
tmin_files <- list.files(directory,pattern='tmin', full.names = TRUE)
dates <- substr(tmin_files,28,31)
tmin_files <- tmin_files[dates %in% time_range]

setwd("G:/DaymetUS/SLF_area")
#list of ALL weather layers
lst <- dir("G:/DaymetUS/SLF_area", pattern='.tif$', full.names=T)
prcp <- lst[1:6]
tavg <- lst[7:12]
tmax <- lst[13:18]
tmin <- lst[19:24]

#setwd("D:\\SCENIC\\PRISM")
fldCoef <- "WeatherCoeff"

if (!file.exists(file.path(fldCoef))) dir.create(file.path(getwd(),fldCoef))
#reference raster used to clip, project, and resample 

setwd("G:/DaymetUS/SLF_area/WeatherCoeff")
m <- c(-Inf, -12, 0, 
       -12, -11,0.05,
       -11, -10,0.1,
       -10,-9,0.15,
       -9,-8,0.2,
       -8,-7,0.25,
       -7,-6,0.3,
       -6,-5,0.35,
       -5,-4,0.4,
       -4,-3,0.45,
       -3,-2,0.5,
       -2,-1,0.55,
       -1,0,0.6,
       0,1,0.65,
       1,2,0.7,
       2,3,0.75,
       3,4,0.8,
       4,5,0.85,
       5,6,0.9,
       6,7,0.95,
       7,8,1,
       8, 30, 1, 
       30, 31, 1,
       31, 32, .8,
       32, 33, .6,
       33, 34, .4,
       34, 35, .2,
       35, Inf, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

for (i in 1:length(tavg)){
  #tavg_rast <- stack(tavg[i])
  tmin_rast <- stack(tavg[i])
  tmin_rast <- tmin_avg[[1:31]]
  #r <- stack(tmin_files[i])
  #names(tavg_rast) <- names(r)
  #indices <- format(as.Date(names(r), format = "X%Y.%m.%d"), format = "%m")
  #indices <- as.numeric(indices)
  #tavg_monthly <- stackApply(tavg_rast, indices, fun=mean)
  #c_coef <- reclassify(tavg_monthly,rclmat)
  crit_temp <- stackApply(tmin_rast, indices = rep(1,31), fun=min)
  writeRaster(x=crit_temp, filename = paste("crit_temp_",time_range[i],"_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
  #writeRaster(x=c_coef, filename = paste("c_coef_",time_range[i],"_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
  print(i)
}




slf_temp <- function(x){
  data <- x
  if (x <= (-12)){ 
    data = 0 
  } else if (x < 8 & x > (-12)) {
    data = (1/20)*x + (3/5)
  } else if ( x >= 8 && x <= 30) {
      data = 1
  } else if (x>30 && x <= 35) {
    data= (-1/5)*x+7
  } else if (x>35) {
    data = 0
  }
  return(data)
}

slf_temp2 <- function(x){
  ifelse(x <= -12, 0, ifelse(x > (-12) && x < 8, (1/20)*x+(3/5), ifelse(x >= 8 && x <= 30, 1, ifelse(x > 30 && x< 35, (-1/5)*x+7, ifelse(x >= 35, 0)))))
}

slf_temp2(tavg_monthly[[1]])
x1 = seq(-12,8,1)
y1 = x1
x2 = seq(30, 35, 1)
y2 = x2

for (i in 1:length(x2)) {
  y2[i] = slf_temp(x2[i])
}
y2



directory <- "G:/DaymetUS/SLF_area/WeatherCoeff"
c_coef <- list.files(directory,pattern='c_coef', full.names = TRUE)
crit_temp <- list.files(directory,pattern='crit_temp', full.names = TRUE)
c_coef_s <- stack(c_coef[4:6])
crit_temp_s <- stack(crit_temp[4:6])
writeRaster(x=crit_temp_s, filename = paste("crit_temp_2015_2017_slfarea2.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
writeRaster(x=c_coef_s, filename = paste("c_coef_2015_2017_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')


## Create final scenario
lst <- dir("G:/DaymetUS/SLF_area/WeatherCoeff", pattern='.tif$', full.names=T)
c_coef <- lst[c(1:3,5:7)]
c_coef_r <- lapply(c_coef, stack)
coef_a <- lapply(c_coef_r, as.array)
for (i in 1:length(coef_a)){
  coef_a[[i]][is.na(coef_a[[i]])] <-0
}
score <- lapply(coef_a, mean)
b2017_2027 <- stack(c_coef_r[[6]], c_coef_r[[3]], c_coef_r[[4]],c_coef_r[[3]], c_coef_r[[4]],c_coef_r[[3]], c_coef_r[[4]],c_coef_r[[3]], c_coef_r[[4]],c_coef_r[[3]], c_coef_r[[4]])
g2017_2027 <- stack(c_coef_r[[6]], c_coef_r[[1]], c_coef_r[[6]],c_coef_r[[1]], c_coef_r[[6]],c_coef_r[[1]], c_coef_r[[6]],c_coef_r[[1]], c_coef_r[[6]],c_coef_r[[1]], c_coef_r[[6]])
writeRaster(x=b2017_2027, filename = paste("b_coef_2017_2027_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
writeRaster(x=g2017_2027, filename = paste("g_coef_2017_2027_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')

b2018_2027 <- stack(c_coef_r[[3]], c_coef_r[[4]],c_coef_r[[3]], c_coef_r[[4]],c_coef_r[[3]], c_coef_r[[4]],c_coef_r[[3]], c_coef_r[[4]],c_coef_r[[3]], c_coef_r[[4]])
g2018_2027 <- stack(c_coef_r[[1]], c_coef_r[[6]],c_coef_r[[1]], c_coef_r[[6]],c_coef_r[[1]], c_coef_r[[6]],c_coef_r[[1]], c_coef_r[[6]],c_coef_r[[1]], c_coef_r[[6]])
writeRaster(x=b2018_2027, filename = paste("b_coef_2018_2027_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
writeRaster(x=g2018_2027, filename = paste("g_coef_2018_2027_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')


c2016_2017 <- stack(c_coef_r[[5]], c_coef_r[[6]])
writeRaster(x=c2016_2017, filename = paste("c_coef_2016_2017_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
