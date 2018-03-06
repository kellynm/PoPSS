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

pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(initialPopulation), na.color = "transparent")
olg <- c('init')
m = leaflet() %>% addTiles() %>% clearImages %>%
  addRasterImage(initialPopulation, colors = pal, opacity= 0.8, group = "init") %>%
  addLayersControl(
    overlayGroups = olg,
    options = layersControlOptions(collapsed = FALSE, opacity =0.6))
  #addLegend(pal = pal, values = values(initialPopulation), title = "Bay Layer Density (100 m)")

for (i in 1:nlayers(I_oaks_rast2)){
  pal <- colorNumeric(c("#0C2C84","#41B6C4","#FFFFCC"), values(I_oaks_rast2[[i]]), na.color = "transparent")
  olg = c(olg, paste("year",i))
  m = m %>% 
    addRasterImage(I_oaks_rast2[[i]], colors = pal, opacity= 0.8, group = paste("year",i)) %>%
    addLayersControl(
      overlayGroups = olg,
      options = layersControlOptions(collapsed = FALSE, opacity =0.6))
    #addLegend(pal = pal, values = values(I_oaks_rast), title = "Bay Layer Density (100 m)")
  print(i)
  m
}



