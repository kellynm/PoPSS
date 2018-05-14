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


## Create initial model data from 1990 to 2000
directory <- "G:/DaymetUS/California/WeatherCoeff"
c_coef <- list.files(directory,pattern='c_coef', full.names = TRUE)
m_coef <- list.files(directory,pattern='m_coef', full.names = TRUE)
c_coef_s <- stack(c_coef[5:32])
m_coef_s <- stack(m_coef[5:32])
c_coef1990_2000 <- c_coef_s[[1:572]]
m_coef1990_2000 <- m_coef_s[[1:572]]

## Create scores for the years from 1990 to 2017
## Temperature score
c_coef_r <- lapply(c_coef_s[3:30], stack)
coef_a <- lapply(c_coef_r, as.array)
for (i in 1:length(coef_a)){
  coef_a[[i]][is.na(coef_a[[i]])] <-0
}
Cscore <- lapply(coef_a, mean)
## Moisture Score
m_coef_r <- lapply(m_coef_s[3:30], stack)
moef_a <- lapply(m_coef_r, as.array)
for (i in 1:length(moef_a)){
  moef_a[[i]][is.na(moef_a[[i]])] <-0
}
Mscore <- lapply(moef_a, mean)

Tscore <- Mscore
for (i in 1:length(Mscore)){
  Tscore[[i]] <- Mscore[[i]]*Cscore[[i]]
}
tscore <- as.data.frame(Tscore)
names(tscore) <- seq(1990,2017,1)
tscore <- t(tscore)
tscore [order(tscore)]

writeRaster(x=m_coef1990_2000, filename = paste("m_coef_1990_2000_california.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
writeRaster(x=c_coef1990_2000, filename = paste("c_coef_1990_2000_california.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
## Create bad weather scenario
c_coef2000_2010 <- c_coef_s[[573:1092]]
m_coef2000_2010 <- c_coef_s[[573:1092]]
writeRaster(x=m_coef2000_2010, filename = paste("m_coef_2000_2010_california.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
writeRaster(x=c_coef2000_2010, filename = paste("c_coef_2000_2010_california.tif", sep = ""), overwrite=TRUE, format = 'GTiff')


bc_coef2010_2030 <- c_coef_s[[c(1093:1456,1197:1248,625:676,885:936,1:52,937:988,1301:1352,1197:1248,625:676,885:936,1:52,937:988,1301:1352,1197:1248)]]
bm_coef2010_2030 <- c_coef_s[[c(1093:1456,1197:1248,625:676,885:936,1:52,937:988,1301:1352,1197:1248,625:676,885:936,1:52,937:988,1301:1352,1197:1248)]]
writeRaster(x=bm_coef2010_2030, filename = paste("bm_coef_2010_2030_california.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
writeRaster(x=bc_coef2010_2030, filename = paste("bc_coef_2010_2030_california.tif", sep = ""), overwrite=TRUE, format = 'GTiff')

## Create good weather scenario
gc_coef2010_2030 <- c_coef_s[[c(1093:1456,417:468,1041:1092,261:312,781:832,313:364,157:208,417:468,1041:1092,261:312,781:832,313:364,157:208,417:468)]]
gm_coef2010_2030 <- c_coef_s[[c(1093:1456,417:468,1041:1092,261:312,781:832,313:364,157:208,417:468,1041:1092,261:312,781:832,313:364,157:208,417:468)]]
writeRaster(x=gm_coef2010_2030, filename = paste("gm_coef_2010_2030_california.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
writeRaster(x=gc_coef2010_2030, filename = paste("gc_coef_2010_2030_california.tif", sep = ""), overwrite=TRUE, format = 'GTiff')

