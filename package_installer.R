## Install require packages if they aren't loaded
package_list <- c("leaflet",  "RColorBrewer", "scales", "lattice", "shiny", "dplyr", "rgdal", "shinyjs", 
                  "shinyBS", "data.table", "devtools", "ggplot2", "tidyr", "raster", "rgdal", "lubridate", "CircStats", 
                  "Rcpp", "ncdf4", "dismo", "sp", "shinydashboard", "shinydashboardPlus")
packages_installed <- installed.packages()[,1]
for (i in 1:length(package_list)) {
  if(package_list[i] %in% packages_installed){
  } else {
    install.packages(package_list[i])
}
}
