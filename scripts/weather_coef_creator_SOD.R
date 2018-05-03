library(rgdal)
library(raster)
library(ncdf4)

#years of interest:
start <- 1990
end <- 2017
time_range <- seq(start, end, 1)
directory <- "G:/DaymetUS"
tmin_files <- list.files(directory,pattern='tmin', full.names = TRUE)
dates <- substr(tmin_files,28,31)
tmin_files <- tmin_files[dates %in% time_range]
setwd("G:/DaymetUS/California")
#list of ALL weather layers
lst <- dir("G:/DaymetUS/California", pattern='.tif$', full.names=T)
prcp <- lst[1:28]
tavg <- lst[29:56]
tmax <- lst[57:84]
tmin <- lst[85:112]
#setwd("D:\\SCENIC\\PRISM")
fldCoef <- "WeatherCoeff"

if (!file.exists(file.path(fldCoef))) dir.create(file.path(getwd(),fldCoef))
#reference raster used to clip, project, and resample 
r <- stack(tmin_files[i])
m <- c(0, 2.5, 0,  2.5, Inf, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
setwd("G:/DaymetUS/California/WeatherCoeff")

for (i in 2:length(prcp)){
  tavg_rast <- stack(tavg[i])
  prcp_rast <- stack(prcp[i])
  prcp_rast2 <- reclassify(prcp_rast,rclmat)
  names(tavg_rast) <- names(r)
  names(prcp_rast) <- names(r)
  # month <- as.numeric(substr(names(r), 7,8))
  # r2 <- r[[which(month %in% 1)]]
  weekly <- rep(seq(1,52,1),7)
  weekly <- weekly[order(weekly)]
  weekly[365] <- 52
  precip_weekly <- stackApply(prcp_rast2, indices = weekly, fun=sum)
  m_coef <- precip_weekly/7
  tavg_weekly <- stackApply(tavg_rast, indices = weekly, fun=mean)
  c_coef <- -0.066 + 0.056 * tavg_weekly - 0.0036 * (tavg_weekly - 15)**2 - 0.0003 * (tavg_weekly - 15)**3
  c_coef[c_coef < 0] <- 0 # restrain lower limit to 0
  c_coef[c_coef > 1] <- 1 # restrain upper limit to 1
  writeRaster(x=m_coef, filename = paste("m_coef_",time_range[i],"_california.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
  writeRaster(x=c_coef, filename = paste("c_coef_",time_range[i],"_california.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
  print(i)
}





