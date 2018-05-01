library(prism)
library(daymetr)
options(prism.path = )
C:\Users\Chris\Desktop
dir.create("C:/Users/Chris/Desktop/Daymet")
download_daymet_ncss(location = c(42.07,-124.52,32.5,-114.11), start = 1990, end =2017, param = "prcp",path = "C:/Users/Chris/Desktop/Daymet")
download_daymet_ncss(location = c(42.07,-124.52,32.5,-114.11), start = 1990, end =2017, param = "tmin",path = "C:/Users/Chris/Desktop/Daymet")
download_daymet_ncss(location = c(42.07,-124.52,32.5,-114.11), start = 1990, end =2017, param = "tmax",path = "C:/Users/Chris/Desktop/Daymet")