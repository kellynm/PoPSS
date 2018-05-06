host1_rast = raster("./layers/UMCA_den_100m.img")
host2_rast = raster("./layers/OAKS_den_100m.img")
host3_rast = raster("./layers/LIDE3_den_100m.img")
allTrees = raster("./layers/TPH_den_100m.img")
initialPopulation = raster ("./layers/init_2000_cnt.img")
start = 2000
end = 2010
SS = 'YES'
s1 = 1
s2 = 9
sporeRate =4.4 ## spore rate default of original attempt
windQ ='YES'
windDir = 'NE'
tempData = './layers/weather/weatherCoeff_2000_2014.nc'
precipData = tempData
number_of_hosts = 2
host1_score = 10
host2_score = 0
host3_score = 5
host4_score = NULL
host5_score = NULL
host6_score = NULL
host7_score = NULL
host8_score = NULL
host9_score = NULL
host10_score = NULL

#setwd("C:\\Users\\chris\\Dropbox\\Projects\\Code\\Aphis Modeling Project")
I_oaks_rast2 <- pest(host1,host2,allTrees,initialPopulation, start, end, SS, s1, s2, sporeRate, windQ, windDir, tempData)
dataList <- pest(host1_rast =host1_rast,host2_rast=host2_rast,host3_rast = host3_rast, allTrees= allTrees,initialPopulation= initialPopulation, start =start, end=end, seasonality=SS, s1=s1, s2=s2, sporeRate=sporeRate, windQ=windQ, windDir=windDir, tempData=tempData, precipData=precipData, number_of_hosts = number_of_hosts, host1_score = host1_score, host2_score = host2_score, host3_score = host3_score)


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


## test sample to make sure it works as intended
prop_S_host1 =.1
prop_S_host2 =.1
prop_S_host3 =.3
prop_S_host4 =.05
prop_S_host5 =.05
prop_S_host6 =.05
prop_S_host7 =.05
prop_S_host8 =.2
prop_S_host9 =0
prop_S_host10 =.1
sample(seq_len(10), 1, replace = FALSE, prob = c(prop_S_host1, prop_S_host2,prop_S_host3, prop_S_host4, prop_S_host5,prop_S_host6,prop_S_host7,prop_S_host8,prop_S_host9,prop_S_host10))


## test implementation of zip file
write.csv(dataReturn, file = "output/data.csv")
writeRaster(dataList[[2]], file = "output/host1.tif")
files2zip <- dir("output", full.names = TRUE)
zip(zipfile = "C:/Users/Chris/Desktop/testZip2.zip", files = files2zip)
do.call(file.remove, list(list.files("output", full.names = TRUE)))

## move from .nc to .tif
file.nc <- "./layers/weather/weatherCoeff_2000_2014.nc"
temp <- raster(file.nc, varname = "Ccoef")
file.tif <- 'layers/weatherCoeff_2000_2014.tif'
precip <- raster(file.nc, varname = "Mcoef")
ccf.array2 <- as.array(precip)


pest_vars <<- list(host1_rast = NULL,host1_score = NULL, host2_rast=NULL,host2_score=NULL,host3_rast=NULL,host3_score=NULL, host4_rast=NULL,host4_score=NULL,host5_rast=NULL,host5_score=NULL,
                   host6_rast=NULL,host6_score=NULL,host7_rast=NULL,host7_score=NULL,host8_rast=NULL,host8_score=NULL,host9_rast=NULL,host9_score=NULL,host10_rast=NULL,host10_score=NULL,
                   allTrees=NULL,initialPopulation=NULL, start=2000, end=2010, seasonality = 'NO', s1 = 1 , s2 = 12, sporeRate = 4.4, windQ =NULL, windDir=NULL, tempQ="NO", tempData=NULL, precipQ="NO", precipData=NULL, kernelType ='Cauchy', kappa = 2, number_of_hosts = 1)
pest_vars$host1_rast = raster("C:/Users/Chris/Dropbox/Projects/APHIS/Ailanthus/ToF.tif")
pest_vars$allTrees = raster("C:/Users/Chris/Dropbox/Projects/APHIS/Ailanthus/totalhost.tif")
pest_vars$initialPopulation = raster ("C:/Users/Chris/Dropbox/Projects/APHIS/Ailanthus/2014Initial.tif")
pest_vars$start = 2015
pest_vars$end = 2017
pest_vars$seasonality = 'YES'
pest_vars$s1 = 6
pest_vars$s2 = 11
pest_vars$sporeRate =4.4 ## spore rate default of original attempt
pest_vars$tempData = 'G:/DaymetUS/SLF_area/WeatherCoeff/c_coef_2015_2017_slfarea.tif'
pest_vars$host1_score = 10
pest_vars$number_of_hosts = 1
pest_vars$tempQ = "YES"
pest_vars$precipQ = "NO"
pest_vars$windQ = "NO"
pest_vars$kernelType = "Cauchy"
pest_vars$scale1 = 40
data <- do.call(pest, pest_vars)
scale = 2
sporeRate = 2
seed_n = 22
i=0
params <- data.frame(scale, sporeRate, seed_n, i)
scales <- seq(20,80,2)
spores <- seq(2.0, 4.4, 0.2)
seeds <- c(42, 45, 78, 29, 57)
for (scale in scales) {
  for (sporeRate in spores) {
    for (seed in seeds) {
  i = i + 1
  pest_vars <<- list(host1_rast = NULL,host1_score = NULL, host2_rast=NULL,host2_score=NULL,host3_rast=NULL,host3_score=NULL, host4_rast=NULL,host4_score=NULL,host5_rast=NULL,host5_score=NULL,
                     host6_rast=NULL,host6_score=NULL,host7_rast=NULL,host7_score=NULL,host8_rast=NULL,host8_score=NULL,host9_rast=NULL,host9_score=NULL,host10_rast=NULL,host10_score=NULL,
                     allTrees=NULL,initialPopulation=NULL, start=2000, end=2010, seasonality = 'NO', s1 = 1 , s2 = 12, sporeRate = 4.4, windQ =NULL, windDir=NULL, tempQ="NO", tempData=NULL, 
                     precipQ="NO", precipData=NULL, kernelType ='Cauchy', kappa = 2, number_of_hosts = 1, seed_n =42)
  pest_vars$host1_rast = raster("C:/Users/Chris/Dropbox/Projects/APHIS/Ailanthus/ToF.tif")
  pest_vars$allTrees = raster("C:/Users/Chris/Dropbox/Projects/APHIS/Ailanthus/totalhost.tif")
  pest_vars$initialPopulation = raster ("C:/Users/Chris/Dropbox/Projects/APHIS/Ailanthus/2014Initial.tif")
  pest_vars$start = 2015
  pest_vars$end = 2017
  pest_vars$seasonality = 'YES'
  pest_vars$s1 = 6
  pest_vars$s2 = 11
  pest_vars$sporeRate =sporeRate ## spore rate default of original attempt
  pest_vars$tempData = 'G:/DaymetUS/SLF_area/WeatherCoeff/c_coef_2015_2017_slfarea.tif'
  pest_vars$host1_score = 10
  pest_vars$number_of_hosts = 1
  pest_vars$tempQ = "YES"
  pest_vars$precipQ = "NO"
  pest_vars$windQ = "NO"
  pest_vars$kernelType = "Cauchy"
  pest_vars$scale1 = scale
  pest_vars$seed_n = seed
  data[[i]] <- do.call(pest, pest_vars)
  params[i,1] <- scale
  params[i,2] <- sporeRate
  params[i,3] <- seed
  params[i,4] <- i
  print(i)
}}}



host1_rast = raster("C:/Users/Chris/Dropbox/Projects/APHIS/Ailanthus/ToF.tif")
allTrees = raster("C:/Users/Chris/Dropbox/Projects/APHIS/Ailanthus/totalhost.tif")
initialPopulation = raster ("C:/Users/Chris/Dropbox/Projects/APHIS/Ailanthus/2014Initial.tif")
start = 2015
end = 2017
seasonality = 'YES'
s1 = 6
s2 = 11
sporeRate =3 ## spore rate default of original attempt
tempData = 'G:/DaymetUS/SLF_area/WeatherCoeff/c_coef_2015_2017_slfarea.tif'
host1_score = 10
number_of_hosts = 1
tempQ = "YES"
precipQ = "NO"
windQ = "NO"
kernelType = "Cauchy"
scale1 = 50

host_score <- rep(host1_score,10)

writeRaster(I_host1_stack, "C:/Users/Chris/Desktop/slftest2.tif", overwrite = TRUE, format = 'GTiff')

for (tt in tstep){
  cnt <- cnt + 1
  if (cnt %in% yearlyoutputlist){
    print(cnt)
  }
}
class(cnt)
