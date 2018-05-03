library(data.table)

dirs <- list.dirs("C:/Users/cmjone25/Dropbox/Projects/APHIS")
dir <- dirs[2]
files <- list.files(dir, pattern = ".csv", full.names = T)

slfdist <- lapply(files, read.csv)
#data <- unique(setDT(slf2015dist)[order(DISTANCE), by ="INPUT_FID"])

slfdist2014 <- data.table(slfdist[[1]])
slfdist2015 <- data.table(slfdist[[2]])
slfdist2016 <- data.table(slfdist[[3]])
slfdist2017 <- data.table(slfdist[[4]])
avg2014 <- mean(slfdist2014$DISTANCE)
min2015 <- slfdist2015[ , .SD[which.min(DISTANCE)], by = NEAR_FID]
avg2015 <- mean(min2015$DISTANCE)
min2016 <- slfdist2016[ , .SD[which.min(DISTANCE)], by = NEAR_FID]
avg2016 <- mean(min2016$DISTANCE)
min2017 <- slfdist2017[ , .SD[which.min(DISTANCE)], by = NEAR_FID]
avg2017 <- mean(min2017$DISTANCE)


data2 <- rbind(slfdist2014,min2015,min2016, min2017)
avgAll <- mean(data2$DISTANCE)
