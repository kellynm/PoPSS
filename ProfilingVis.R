## Profiling pest function (should be used to improve performance)

devtools::install_github("hadley/lineprof")
library(lineprof)
profileData <- lineprof(pest(host1,host2,allTrees,initialPopulation, start, end, SS, s1, s2, sporeRate, windQ, windDir, tempData, precipData))


library(profvis)
profileData2 <- profvis({pest(host1,host2,allTrees,initialPopulation, start, end, SS, s1, s2, sporeRate, windQ, windDir, tempData, precipData)})
profileData2
