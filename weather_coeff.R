## Read in libraries
library(rgdal)
library(raster)
library(ncdf4)
library(sp)
library(googledrive)

weather_coeff <- function(directory, output_directory, start, end, timestep, states_of_interest= c('California'), variables = c("prcp"), pest){
  ## create time range
  time_range <- seq(start, end, 1)
  
  ## read in list of daymet files to choose from later 
  precip_files <- list.files(directory,pattern='prcp', full.names = TRUE)
  tmax_files <- list.files(directory,pattern='tmax', full.names = TRUE)
  tmin_files <- list.files(directory,pattern='tmin', full.names = TRUE)
  dates <- substr(precip_files,28,31) # Assumes daymet data is saved in the exact naming format that it is downloaded as
  precip_files <- precip_files[dates %in% time_range]
  tmin_files <- tmin_files[dates %in% time_range]
  tmax_files <- tmax_files[dates %in% time_range]
  
  ## reference shapefile used to clip, project, and resample 
  states <- readOGR("C:/Users/Chris/Desktop/California/us_states_lccproj.shp") # link to your local copy
  reference_area <- states[states@data$STATE_NAME %in% states_of_interest,]
  rm(states)
  
  ## Create raster stacks for the area of interest and years of interest from Daymet data
  prec <- stack()
  tmin_s <- stack()
  tmax_s <- stack()
  tavg_s <- stack()
  
  for (i in 1:length(precip_files)) {
    ## Precipitation 
    precip <- stack(precip_files[[i]], varname = "prcp")
    precip <- crop(precip, reference_area)
    precip <- mask(precip, reference_area)
    if (i>1 && compareCRS(precip,prec) == FALSE) { precip@crs <- crs(prec) }
    prec <- stack(prec, precip)
    rm(precip)
    
    ## Temperature
    tmin <- stack(tmin_files[[i]], varname = "tmin")
    tmin <- crop(tmin, reference_area)
    tmin <- mask(tmin, reference_area)
    if (i>1 && compareCRS(tmin,tmin_s) == FALSE) { tmin@crs <- crs(tmin_s) }
    tmax <- stack(tmax_files[[i]], varname = "tmax")
    tmax <- crop(tmax, reference_area)
    tmax <- mask(tmax, reference_area)
    if (i>1 && compareCRS(tmax,tmax_s) == FALSE) { tmax@crs <- crs(tmax_s) }
    tavg <- tmax
    for (j in 1:nlayers(tmax)){
      tavg[[j]] <- overlay(tmax[[j]], tmin[[j]], fun = function(r1, r2){return((r1+r2)/2)})
      print(j)
    }
    tmin_s <- stack(tmin, tmin_s)
    tmax_s <- stack(tmax, tmax_s)
    tavg_s <- stack(tavg, tavg_s)
    
    print(i)
  }
  
  ## create directory for writing files
  dir.create(output_directory)
  
  ## Write outputs as raster format to output directory
  writeRaster(x=prec, filename = paste(output_directory, "/prcp_coeff_", start, "_", end, "_", pest, ".tif", sep = ""), overwrite=TRUE, format = 'GTiff')
  writeRaster(x=temp, filename = paste(output_directory, "/temp_coeff_", start, "_", end, "_", pest, ".tif", sep = ""), overwrite=TRUE, format = 'GTiff')
  
}