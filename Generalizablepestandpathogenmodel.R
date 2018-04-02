## install packages (only needed on first run of the model)
#install.packages(c("rgdal","raster","lubridate","CircStats","Rcpp", "rgrass7", "optparse", "plotrix", "ncdf4", "dismo", "sp"))

## load packages:
suppressPackageStartupMessages(library(raster))    # Raster operation and I/O. Depends R (≥ 2.15.0)
suppressPackageStartupMessages(library(rgdal))     # Geospatial data abstraction library. Depends R (≥ 2.14.0)
suppressPackageStartupMessages(library(lubridate)) # Make dealing with dates a little easier. Depends R (≥ 3.0.0)
suppressPackageStartupMessages(library(CircStats)) # Circular Statistics - Von Mises distribution
suppressPackageStartupMessages(library(Rcpp))      # Seamless R and C++ Integration. Depends R (≥ 3.0.0)
suppressPackageStartupMessages(library(plotrix))   # Add text annotations to plot
suppressPackageStartupMessages(library(ncdf4))     # work with NetCDF datasets
suppressPackageStartupMessages(library(dismo))     # Regression for ecological datasets
suppressPackageStartupMessages(library(sp))        # Classes and methods for spatial data

pest <- function(host1_rast,host1_score = NULL, host2_rast=NULL,host2_score=NULL,host3_rast=NULL,host3_score=NULL, host4_rast=NULL,host4_score=NULL,host5_rast=NULL,host5_score=NULL,
                 host6_rast=NULL,host6_score=NULL,host7_rast=NULL,host7_score=NULL,host8_rast=NULL,host8_score=NULL,host9_rast=NULL,host9_score=NULL,host10_rast=NULL,host10_score=NULL,
                 allTrees,initialPopulation, start, end, SS = 'NO', s1 = 1 , s2 = 12, sporeRate, windQ, windDir, tempData, precipData, kernelType ='Cauchy', kappa = 2){
  
## Define the main working directory based on the current script path (un commment next line if used outside of shiny framework)
#setwd("C:\\Users\\chris\\Dropbox\\Projects\\Code\\Aphis Modeling Project")

## Use an external source file w/ all modules (functions) used within this script. 
## Use FULL PATH if source file is not in the same folder w/ this script
source('./scripts/myfunctions_SOD.r') # loads custom functions for dispersal using R
sourceCpp("./scripts/myCppFunctions.cpp") # load custom functions dispersal that use C++ (Faster)

## Input rasters: abundance (tree density per hectare)
# Host 1
host1_rast <- host1_rast
host1_score <- host1_score
# Host 2
host2_rast <- host2_rast
host2_score <- host2_score
# Host 3
host3_rast <- host3_rast
host3_score <- host3_score
# Host 4
host4_rast <- host4_rast
host4_score <- host4_score
# Host 5
host5_rast <- host5_rast
host5_score <- host5_score
# Host 6
host6_rast <- host6_rast
host6_score <- host6_score
# Host 7
host7_rast <- host7_rast
host7_score <- host7_score
# Host 8
host8_rast <- host8_rast
host8_score <- host8_score
# Host 9
host9_rast <- host9_rast
host9_score <- host9_score
# Host 10
host10_rast <- host10_rast
host10_score <- host10_score
 
#----> All live trees (for calculating the proportion of infected)
all_trees_rast <- allTrees

## raster resolution
res_win <- res(host1_rast)[1]
n_cols <- as.numeric(ncol(host1_rast))
n_rows <- as.numeric(nrow(host1_rast))

### INFECTED AND SUSCEPTIBLES ####

## Initial infection (OAKS):
I_host2_rast <- initialPopulation
I_host2_stack <- I_host2_rast

## define matrices for infected and susceptible species of interest
I_oaks <- as.matrix(I_host2_rast)
S_oaks <- as.matrix(host2_rast - I_host2_rast)
I_host1 <- matrix(0, nrow=n_rows, ncol=n_cols)
S_host1 <- as.matrix(host1_rast)

## Initialize infected trees for each species (!!NEEDED UNLESS EMPIRICAL INFO IS AVAILABLE!!)
if(any(S_host1[I_oaks > 0] > 0)) I_host1[I_oaks > 0] <- mapply(function(x,y) ifelse(x > y, min(c(x,y*2)), x), 
                                                             S_host1[I_oaks > 0], I_oaks[I_oaks > 0]) 
## update susceptible matrices by subtracting the initialized infections 
S_host1 <- S_host1 - I_host1 

## Update Infected host rasters for output
I_host1_rast <- I_host2_rast
I_host1_rast[] <- I_host1
I_host1_stack <- I_host1_rast

## define matrix for all live trees (for calculating the percentage of infected)
all_trees <- as.matrix(all_trees_rast)

## Start-End date:
start = start
end = end
if (start > end) stop('start date must precede end date!!')

## build time series for simulation steps:
dd_start <- as.POSIXlt(as.Date(paste(start,'-01-01',sep='')))
dd_end <- as.POSIXlt(as.Date(paste(end,'-12-31',sep='')))
tstep <- as.character(seq(dd_start, dd_end, 'weeks'))

## create list for yearly output
split_date2 = unlist(strsplit(tstep, '-'))
split_date2 = as.data.frame(as.numeric(split_date2[seq(2,length(split_date2),3)]))
listvar = 1
yearlyoutputlist = 0
for (i in 2:nrow(split_date2)) {
  if (split_date2[i,1] > split_date2[i-1,1] && split_date2[i,1] == 10) {
    yearlyoutputlist[listvar] <- i-1
    listvar = listvar +1
  } 
}

## Create data frame for infected host data 
years = seq(start, end, 1)
dataForOutput <- data.frame(years = years, infectedHost1Individuals = 0, infectedHost1Area = 0, infectedHost2Individuals = 0, infectedHost2Area = 0) # replace infected host with actual host names
yearTracker = 0

## create formatting expression for padding zeros depending on total number of steps
formatting_str = paste("%0", floor( log10( length(tstep) ) ) + 1, "d", sep='')

## WEATHER SUITABILITY: read and stack weather suitability raster BEFORE running the simulation

## weather coefficients
#mcf.array <- ncvar_get(nc_open('./layers/weather/weatherCoeff_2000_2014.nc'),  varid = "Mcoef") #M = moisture;
#ccf.array <- ncvar_get(nc_open('./layers/weather/weatherCoeff_2000_2014.nc'),  varid = "Ccoef") #C = temperature;
mcf.array <- ncvar_get(nc_open(precipData),  varid = "Mcoef") #M = moisture;
ccf.array <- ncvar_get(nc_open(tempData),  varid = "Ccoef") #C = temperature;

## Seasonality: Do you want the spread to be limited to certain months?
ss <- SS   #'YES' or 'NO'
if (ss == 'YES') months_msk <- paste('0', s1:s2, sep='') #1=January 9=September (Default to 1-12)

##Wind: Do you want the spread to be affected by wind?
wind <- windQ #'YES' or 'NO'
pwdir <- windDir
spore_rate <- sporeRate

#time counter to access pos index in weather raster stacks
cnt <- 1 

## ----> MAIN SIMULATION LOOP (weekly time steps) <------
for (tt in tstep){
  
  #split date string for raster time stamp
  split_date = unlist(strsplit(tt, '-'))
  
  if (tt == tstep[1]) {
    
    if(!any(S_oaks > 0)) stop('Simulation ended. All oaks are infected!')
    
    ##CALCULATE OUTPUT TO PLOT: 
    # 1) values as % infected
    #I_host2_rast[] <- ifelse(I_host2_rast[] == 0, NA, I_host2_rast[]/host2_rast[])
    
    # 2) values as number of infected per cell
    I_host2_rast[] <- ifelse(I_host2_rast[] == 0, NA, I_host2_rast[])
    
    # 3) values as 0 (non infected) and 1 (infected) cell
    #I_host2_rast[] <- ifelse(I_host2_rast[] > 0, 1, 0) 
    #I_host2_rast[] <- ifelse(I_host2_rast[] > 0, 1, NA) 
    
    #WRITE TO FILE:
    #writeRaster(I_host2_rast, filename=paste('./', fOutput, '/', opt$output, '_', sprintf(formatting_str, cnt), sep=''), format='HFA', datatype='FLT4S', overwrite=TRUE) # % infected as output
    #writeRaster(I_host2_rast, filename=paste('./', fOutput, '/', opt$output, '_', sprintf(formatting_str, cnt), sep=''), format='HFA', datatype='INT1U', overwrite=TRUE) # nbr. infected hosts as output
    #writeRaster(I_host2_rast, filename=paste('./', fOutput, '/', opt$output, '_', sprintf(formatting_str, cnt), sep=''), format='HFA', datatype='LOG1S', overwrite=TRUE)  # 0=non infected 1=infected output
    
  }else{
    
    #check if there are any susceptible oaks left on the landscape (IF NOT continue LOOP till the end)
    if(!any(S_oaks > 0)) break
    
    #update week counter
    cnt <- cnt + 1
    
    ## is current week time step within a spread month (as defined by input parameters)?
    if (ss == 'YES' & !any(substr(tt,6,7) %in% months_msk)) next
    
    ## Total weather suitability:
    W <- mcf.array[,,cnt] * ccf.array[,,cnt]
    
    #GENERATE SPORES:  
    #integer matrix
    set.seed(42)
    spores_mat <- SporeGenCpp(I_host1, W, rate = spore_rate) #rate: spores/week for each infected host (4.4 default)
    
    ##SPORE DISPERSAL:  
    #'List'
    if (wind == 'YES') {
      
      #Check if predominant wind direction has been specified correctly:
      if (!(pwdir %in% c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'))) stop('A predominant wind direction must be specified: N, NE, E, SE, S, SW, W, NW')
      out <- SporeDispCppWind_mh(spores_mat, S_UM=S_host1, S_OK=S_oaks, I_UM=I_host1, I_OK=I_oaks, N_LVE=all_trees, 
                                 W, rs=res_win, rtype=kernelType, scale1=20.57, wdir=pwdir, kappa=kappa)
    
    }else{
      out <- SporeDispCpp_mh(spores_mat, S_UM=S_host1, S_OK=S_oaks, I_UM=I_host1, I_OK=I_oaks, N_LVE=all_trees,
                             W, rs=res_win, rtype=kernelType, scale1=20.57) ##TO DO
    }  
    
    ## update R matrices:
    # Host 1
    S_host1 <- out$S_UM 
    I_host1 <- out$I_UM 
    # Host 2
    S_oaks <- out$S_OK 
    I_oaks <- out$I_OK
    
    ##CALCULATE OUTPUT TO PLOT:
    I_host2_rast[] <- I_oaks
    I_host1_rast[] <- I_host1
    
    # 1) values as % infected
    #I_host2_rast[] <- ifelse(I_host2_rast[] == 0, NA, I_host2_rast[]/host2_rast[])
    
    # 2) values as number of infected per cell
    I_host2_rast[] <- ifelse(I_host2_rast[] == 0, NA, I_host2_rast[])
    I_host1_rast[] <- ifelse(I_host1_rast[] == 0, NA, I_host1_rast[])
    
    # 3) values as 0 (non infected) and 1 (infected) cell
    #I_host2_rast[] <- ifelse(I_host2_rast[] > 0, 1, 0) 
    #I_host2_rast[] <- ifelse(I_host2_rast[] > 0, 1, NA) 
        
    if (cnt %in% yearlyoutputlist){
      yearTracker = yearTracker+1
      I_host2_stack <- stack(I_host2_rast, I_host2_stack)
      I_host1_stack <- stack(I_host1_rast, I_host1_stack)
      dataForOutput$infectedHost1Individuals[yearTracker] <- sum(na.omit(I_host1_rast@data@values))/1000
      dataForOutput$infectedHost1Area[yearTracker] <- ncell(na.omit(I_host1_rast@data@values))*res(I_host1_rast)[2]*res(I_host1_rast)[1]
      dataForOutput$infectedHost2Individuals[yearTracker] <- sum(na.omit(I_host2_rast@data@values))/1000
      dataForOutput$infectedHost2Area[yearTracker] <- ncell(na.omit(I_host2_rast@data@values))*res(I_host2_rast)[2]*res(I_host2_rast)[1]
      
      ## WRITE TO FILE:
      #writeRaster(I_host2_rast, filename=paste('./', fOutput, '/', opt$output, '_', sprintf(formatting_str, cnt), sep=''), format='HFA', datatype='FLT4S', overwrite=TRUE) # % infected as output
      #writeRaster(I_host2_rast, filename=paste('./', fOutput, '/', opt$output, '_', sprintf(formatting_str, cnt), sep=''), format='HFA', datatype='INT1U', overwrite=TRUE) # nbr. infected hosts as output
      #writeRaster(I_host2_rast, filename=paste('./', fOutput, '/', opt$output, '_', sprintf(formatting_str, cnt), sep=''), format='HFA', datatype='LOG1S', overwrite=TRUE)  # 0=non infected 1=infected output
      #return(I_host2_rast)
      
    }
    
  }
  
}

I_host2_stack <- subset(I_host2_stack, order(seq(nlayers(I_host2_stack)-1, 1, -1)))
I_host1_stack <- subset(I_host1_stack, order(seq(nlayers(I_host1_stack)-1, 1, -1)))
names(I_host2_stack) <- years
names(I_host1_stack) <- years
data <- list(dataForOutput, I_host2_stack)

return(data)
}



