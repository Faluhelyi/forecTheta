library(Mcomp)
library(parallel)
library(doParallel)
library(iterators)
library(forecast)
devtools::load_all(".")
#data("lynx")

y <- M3[[1]]$x

#plot(y)

m1 <- dotm(y, h=5)
m2 <- seasonal_dotm(y, h=5)

####################################################################

m1 <- dotm(y = AirPassengers, h = 30)
m2 <- seasonal_dotm(y = AirPassengers, h = 30)

#m <- dotm(y = lynx, h = 5)
#m <- seasonal_dotm(y = lynx, h = 5)
plot(m1)
plot(m2)

