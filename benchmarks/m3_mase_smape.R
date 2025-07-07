#devtools::install()

library(forecTheta)
library(forecast)
library(Mcomp)
library(foreach)
library(parallel)
library(doParallel)

data(M3)

inicio = Sys.time()

cl <- makeCluster(detectCores(logical = FALSE))
registerDoParallel(cl)

out = foreach(i = 1:3003, .packages = c("forecast", "forecTheta")) %dopar% {
  x = M3[[i]]$x
  h = M3[[i]]$h
  f = seasonal_ThetaModel(y = x, h = h)
  f
}

stopCluster(cl)

fim = Sys.time()
fim - inicio


forec = matrix(NA, nrow=3003, ncol=18)
obs = matrix(NA, nrow=3003, ncol=18) #matrix of the out-sample values
meanDiff <- rep(1, 3003)

for(i in 1:3003){
  x=M3[[i]]$x
  h=M3[[i]]$h
  obs[i,1:h] = M3[[i]]$xx
  forec[i,1:h] = out[[i]]$mean
  meanDiff[i] = mean(abs(diff(x, lag = frequency(x))))
}

############## sMAPE ###################
sAPE_matrix = errorMetric(obs=obs, forec=forec, type="sAPE", statistic="N")
#### Yearly ###
mean( sAPE_matrix[1:645, 1:6] ) %>% round(2)
#### QUARTERLY ###
mean( sAPE_matrix[646:1401, 1:8] ) %>% round(2)
#### MONTHLY ###
mean( sAPE_matrix[1402:2829, 1:18] ) %>% round(2)
#### Other ###
mean( sAPE_matrix[2830:3003, 1:8] ) %>% round(2)
#### ALL ###
mean( sAPE_matrix, na.rm=TRUE ) %>% round(2)
#### ALL 2 ###
sAPE_matrix %>% rowMeans(na.rm=T) %>% mean() %>% round(2)

#
############# MASE ######################
AE_matrix = errorMetric(obs=obs, forec=forec, type="AE", statistic="N")
ASE_matrix=AE_matrix/meanDiff
#### Yearly ###
mean( ASE_matrix[1:645, 1:6] ) %>% round(2)
#### QUARTERLY ###
mean( ASE_matrix[646:1401, 1:8] ) %>% round(2)
#### MONTHLY ###
mean( ASE_matrix[1402:2829, 1:18] ) %>% round(2)
#### Other ###
mean( ASE_matrix[2830:3003, 1:8] ) %>% round(2)
#### ALL ###
mean( ASE_matrix, na.rm=TRUE ) %>% round(2)
#### ALL 2 ###
ASE_matrix %>% rowMeans(na.rm=T) %>% mean() %>% round(2)

