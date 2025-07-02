library(Mcomp)
library(parallel)
library(doParallel)
library(iterators)
library(forecast)
devtools::load_all(".")

m1 <- dotm(y = AirPassengers, h = 30)
m2 <- seasonal_dotm(y = AirPassengers, h = 30)
m3 <- seasonal_dotm(y = AirPassengers, h = 30, s_type = "additive")
m4 <- bagged_seasonal_dotm(y = AirPassengers, h = 30, s_type = "additive")
m5 <- bagged_seasonal_dotm(y = AirPassengers, h = 30)

png("benchmarks/plot_dotm.png", width = 800, height = 600)
plot(m1)
dev.off()

png("benchmarks/plot_dynamic_sotm_multiplicativo.png", width = 800, height = 600)
plot(m2)
dev.off()

png("benchmarks/plot_dynamic_sotm_aditivo.png", width = 800, height = 600)
plot(m3)
dev.off()

png("benchmarks/plot_bagged_dynamic_sotm_aditivo.png", width = 800, height = 600)
plot(m4)
dev.off()

png("benchmarks/plot_bagged_dynamic_sotm_multiplicativo.png", width = 800, height = 600)
plot(m5)
dev.off()

