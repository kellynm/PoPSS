host1 = "./layers/UMCA_den_100m.img"
host1 = raster(host1)
host2 = raster("./layers/OAKS_den_100m.img")
allTrees = raster("./layers/TPH_den_100m.img")
initialPopulation = raster ("./layers/init_2000_cnt.img")
start = 2000
end = 2010
SS = 'YES'
s1 = 1
s2 = 9
sporeRate =4.4
windQ ='YES'
windDir = 'NE'
tempData = './layers/weather/weatherCoeff_2000_2014.nc'

#setwd("C:\\Users\\chris\\Dropbox\\Projects\\Code\\Aphis Modeling Project")
I_oaks_rast2 <- pest(host1,host2,allTrees,initialPopulation, start, end, SS, s1, s2, sporeRate, windQ, windDir, tempData)
data <- pest(host1,host2,allTrees,initialPopulation, start, end, SS, s1, s2, sporeRate, windQ, windDir, tempData)

data2 <- data[[1]]
I_oaks_rast2 <- data[[2]]



title = "Precipitation 1990-2090"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"))
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot3 = ggplot(data2, aes(x=years, y=infectedHost1Area))+geom_line(aes(years,infectedHost1Area))
plot3






ggplot()
#cumulativeinfection <- sum(na.omit(I_oaks_rast2[[1]]@data@values))
#areainfected <- ncell(na.omit(I_oaks_rast2[[1]]@data@values))*res(I_oaks_rast2[[1]])[2]*res(I_oaks_rast2[[1]])[1]
