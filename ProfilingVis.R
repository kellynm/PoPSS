## Profiling pest function (should be used to improve performance)

devtools::install_github("hadley/lineprof")
library(lineprof)
profileData <- lineprof(pest(host1,host2,allTrees,initialPopulation, start, end, SS, s1, s2, sporeRate, windQ, windDir, tempData, precipData))


library(profvis)
profileData2 <- profvis({pest(host1_rast =host1_rast,host2_rast=host2_rast,allTrees= allTrees,initialPopulation= initialPopulation, start =start, end=end, seasonality=SS, s1=s1, s2=s2, sporeRate=sporeRate, windQ=windQ, windDir=windDir, tempData=tempData, precipData=precipData, number_of_hosts = number_of_hosts)})
profileData2
