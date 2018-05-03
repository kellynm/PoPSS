## Read in libraries
library(rgdal)
library(raster)
library(ncdf4)
library(sp)

daymet_setup <- function(directory, output_directory, start, end, timestep, states_of_interest= c('California'), variables = c("prcp")){
  
}

## Setup start and stop years
start <-  2012
end <- 2017
time_range <- seq(start, end, 1)
## set up directory with files and create a new directory based on location of interest
setwd("G:/DaymetUS2")
directory <- "G:/DaymetUS"
output_directory <- dir.create("G:/DaymetUS/California")

## reference shapefile used to clip, project, and resample 
states <- readOGR("C:/Users/Chris/Desktop/California/us_states_lccproj.shp") # link to your local copy
reference_area <- states[states@data$STATE_NAME %in% c('Delaware','District of Columbia','New Jersey','Virginia', 'Pennsylvania', 'Maryland'),]
rm(states)

## list of files
precip_files <- list.files(directory,pattern='prcp', full.names = TRUE)
precip_files <- precip_files[2:length(precip_files)]
tmax_files <- list.files(directory,pattern='tmax', full.names = TRUE)
tmin_files <- list.files(directory,pattern='tmin', full.names = TRUE)
dates <- substr(precip_files,28,31)
precip_files <- precip_files[dates %in% time_range]
tmin_files <- tmin_files[dates %in% time_range]
tmax_files <- tmax_files[dates %in% time_range]


setwd("G:/DaymetUS/SLF_area")

for (i in 1:length(precip_files)) {
  precip <- stack(precip_files[[i]], varname = "prcp")
  precip <- crop(precip, reference_area)
  precip <- mask(precip, reference_area)
  writeRaster(x=precip, filename = paste("daymet_v3_prcp_",time_range[i],"_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
  print(i)
}

## Create Tavg
for (i in 2:length(tmax_files)){
  tmin <- stack(tmin_files[[i]], varname = "tmin")
  tmin <- crop(tmin, reference_area)
  tmin <- mask(tmin, reference_area)
  tmax <- stack(tmax_files[[i]], varname = "tmax")
  tmax <- crop(tmax, reference_area)
  tmax <- mask(tmax, reference_area)
  tavg <- tmax
  for (j in 1:nlayers(tmax)){
    tavg[[j]] <- overlay(tmax[[j]], tmin[[j]], fun = function(r1, r2){return((r1+r2)/2)})
    print(j)
  }
  writeRaster(x=tmax, filename = paste("daymet_v3_tmax_",time_range[i],"_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
  writeRaster(x=tmin, filename = paste("daymet_v3_tmin_",time_range[i],"_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
  writeRaster(x=tavg, filename = paste("daymet_v3_tavg_",time_range[i],"_slfarea.tif", sep = ""), overwrite=TRUE, format = 'GTiff')
  print(i)
}


## Using ncdf4
p_list <- lapply(precip_files, nc_open)
tmax_list <- lapply(tmax_files, nc_open)
tmin_list <- lapply(tmin_files, nc_open)

## pull out lat, lon, and time
lat <- ncvar_get(p_list[[1]], varid = "lat")
lon <- ncvar_get(p_list[[1]], varid = "lon")
time <- ncvar_get(p_list[[1]], varid = "time")

pvar <- ncvar_get(p_list[[1]], varid = "prcp")
tmax_var <- ncvar_get(tmax_list[[1]], varid = "tmax")
tmin_var <- ncvar_get(tmin_list[[1]], varid = "tmin")