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

make1 <- data2[,1:3]
make2 <- data2[,c(1,4:5)]
names(make1) <- c('Year','Area','Count')
names(make2) <- c('Year','Area','Count')
make1$Host <- 'Tanoak'
make2$Host <- 'Oaks'
make <- rbind(make1,make2)


title = "Model Output"
theme = theme_set(theme_classic())
theme = theme_update(legend.position="top", legend.title=element_blank(),legend.spacing=unit(-0.5,"lines"), plot.background = element_rect(fill = "#3F3E3E"), panel.background = element_rect(fill = "#3F3E3E"), legend.box.background = element_rect(fill = "#3F3E3E"))
theme = theme_update(axis.text = element_text(colour="#227032"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5,colour="#227032"), axis.line = element_line(colour="#227032"))
plot3 = ggplot(make, aes(x=Year, y=Area, color=factor(Host)))+geom_line(aes(Year,Area))
plot3 = plot3+scale_color_manual(values=c("blue", "red"))+scale_fill_manual(values=c("blue", "red"))
plot3 = plot3+ggtitle(title)
plot3 = plot3 + theme(axis.text=element_text(size=10,colour="#227032"),axis.title=element_text(size=16, vjust=0,35,colour="#227032"),legend.text=element_text(size=10,colour="#227032"),plot.title=element_text(size=20))
plot3 = plot3 + scale_x_continuous(name="Year", breaks=seq(start, end, 2))
plot3 = plot3 + scale_y_continuous(name=expression("Area "*~m^2))+guides(col=guide_legend(ncol=3),shape=guide_legend(ncol = 1))
plot3






ggplot()
#cumulativeinfection <- sum(na.omit(I_oaks_rast2[[1]]@data@values))
#areainfected <- ncell(na.omit(I_oaks_rast2[[1]]@data@values))*res(I_oaks_rast2[[1]])[2]*res(I_oaks_rast2[[1]])[1]
