usCounties <<- readOGR("./layers/usLower48Counties.shp")
usStates <<- readOGR("./layers/usLower48States.shp")

ca <- extract(I_oaks_rast2[[1]],usStates, fun = sum, na.rm=TRUE)
ca2 <- extract(I_oaks_rast2[[1]],usStates, fun = length(), na.rm=TRUE)



county <- extract(I_oaks_rast2[[1]],usCounties, fun = sum, na.rm=TRUE)

#usStates@data[usStates@data$STATE_NAME=="California"]
#usStates@data
