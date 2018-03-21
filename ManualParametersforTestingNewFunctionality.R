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
dataList <- pest(host1,host2,allTrees,initialPopulation, start, end, SS, s1, s2, sporeRate, windQ, windDir, tempData)

dataReturn <- dataList[[1]]
I_oaks_rast2 <- dataList[[2]]

make1 <- dataReturn[,1:3]
make2 <- dataReturn[,c(1,4:5)]
names(make1) <- c('Year','Area','Count')
names(make2) <- c('Year','Area','Count')
make1$Host <- 'Tanoak'
make2$Host <- 'Oaks'
make <- rbind(make1,make2)
dataForPlot <- make

leaflet("stateData", height = "600px") %>% addMiniMap(position = "topright")

title = "Model Output"
theme = theme_set(theme_classic())
theme = theme_update(text = element_text(family = "Arial"), legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"), plot.background = element_rect(fill = "#3F3E3E", colour = "#3F3E3E"), panel.background = element_rect(fill = "#3F3E3E", colour = "#3F3E3E"), legend.background = element_rect(fill = "#3F3E3E"))
theme = theme_update(axis.text = element_text(size = 12, color="white"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5,colour="white", size =18), axis.line = element_line(colour="white"),axis.title=element_text(size=16, vjust=0,35,color="white"),legend.text=element_text(size=12,color="white"))
ggplot(dataForPlot, aes(x=Year, y=Area, color=factor(Host)))+geom_line(aes(Year, Area), size = 1.5)+scale_color_manual(values=c("#54ACC1", "#ADBD60"))+scale_fill_manual(values=c("#54ACC1", "#ADBD60"))+
  ggtitle(title)+theme(text = element_text(family = "sans"))+
  scale_x_continuous(name="Year", breaks=seq(start, end, 2))+
  scale_y_continuous(name="yName")+guides(col=guide_legend(ncol=3),shape=guide_legend(ncol = 1))

zipped.csv <- function(df, zippedfile) {
  # init temp csv
  temp <- tempfile(fileext=".csv")
  # write temp csv
  write.csv(df, file=temp)
  # zip temp csv
  zip(zippedfile,temp)
  # delete temp csv
  unlink(temp)
}



leaflet(usStates) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>% 
  addMiniMap(usCounties, position = "topright")


# 
# %>%   
#   addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#                                                                  opacity = 1.0, fillOpacity = 0.5,
#                                                                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))