library(Mcomp)
library(parallel)
library(doParallel)
library(iterators)

# cluster config
cl <- makeCluster(detectCores(logical = FALSE))
registerDoParallel(cl)

# load local forecTheta for each node and export those functions
clusterEvalQ(cl, devtools::load_all("."))
clusterExport(cl, c("dotm", "dstm", "otm", "stm"))

out_dotm <- foreach(
  i = 1:3003, .packages = c("foreach", "forecast", "Mcomp")
) %dopar% {
  x <- M3[[i]]$x
  h <- M3[[i]]$h
  f <- dotm(y = x, h = h)
}

out_dstm <- foreach(
  i = 1:3003, .packages = c("foreach", "forecast", "Mcomp")
) %dopar% {
  x <- M3[[i]]$x
  h <- M3[[i]]$h
  f <- dstm(y = x, h = h)
}

out_otm <- foreach(
  i = 1:3003, .packages = c("foreach", "forecast", "Mcomp")
) %dopar% {
  x <- M3[[i]]$x
  h <- M3[[i]]$h
  f <- otm(y = x, h = h)
}

out_stm <- foreach(
  i = 1:3003, .packages = c("foreach", "forecast", "Mcomp")
) %dopar% {
  x <- M3[[i]]$x
  h <- M3[[i]]$h
  f <- stm(y = x, h = h)
}

stopCluster(cl)

# models out list
out <- list(out_stm, out_otm, out_dstm, out_dotm)

results_smape <- c()
results_mase <- c()
results_model <- c()
rn <- c()
l <- length(out)
# apply for each model out
for (mo in 1:l) {
  outt <- out[[mo]]
  forec <- matrix(NA, nrow=3003, ncol=18)
  obs <- matrix(NA, nrow=3003, ncol=18) #matrix of the out-sample values
  meanDiff <- rep(1, 3003)
  for(i in 1:3003){
    x <- M3[[i]]$x
    h <- M3[[i]]$h
    obs[i, 1:h] <- M3[[i]]$xx
    forec[i, 1:h] <- outt[[i]]$mean
    meanDiff[i] <- mean(abs(diff(x, lag = frequency(x))))
  }

  ############## sMAPE ###################
  sAPE_matrix = errorMetric(
    obs = obs, forec = forec, type = "sAPE", statistic = "N"
  )

  #### Yearly ###
  ye <- round(mean(sAPE_matrix[1:645, 1:6]), 2)

  #### QUARTERLY ###
  qe <- round(mean(sAPE_matrix[646:1401, 1:8]), 2)

  #### MONTHLY ###
  me <- round(mean(sAPE_matrix[1402:2829, 1:18]), 2)

  #### Other ###
  ot <- round(mean(sAPE_matrix[2830:3003, 1:8]), 2)

  #### ALL ###
  all <- round(mean(sAPE_matrix, na.rm = TRUE), 2)
  results_smape <- c(results_smape, ye, qe, me, ot, all)

  ############# MASE ######################
  AE_matrix = errorMetric(
    obs = obs, forec = forec, type = "AE", statistic = "N"
  )
  ASE_matrix <- AE_matrix / meanDiff

  #### Yearly ###
  ye <- round(mean(ASE_matrix[1:645, 1:6]), 2)

  #### QUARTERLY ###
  qe <- round(mean(ASE_matrix[646:1401, 1:8]), 2)

  #### MONTHLY ###
  me <- round(mean(ASE_matrix[1402:2829, 1:18]), 2)

  #### Other ###
  ot <- round(mean(ASE_matrix[2830:3003, 1:8]), 2)

  #### ALL ###
  all <- round(mean(ASE_matrix, na.rm = TRUE ), 2)  
  results_mase <- c(results_mase, ye, qe, me, ot, all)
  ########################################################
  results_model <- c(results_model, rep(tail(outt, 1)[[1]]$method, 5))
  rn <- c(rn, c("Yearly", "Quarterly", "Monthly", "Other", "All"))
}


results <- data.frame(
  smape <- results_smape,
  mase <- results_mase,
  model <- results_model,
  idx <- rn
)
colnames(results) <- c("sMAPE", "MASE", "Model")


test_that("STM Operating Normally", {
  smape <- results[results$Model == "Standard Theta Model", ]$sMAPE
  mase <- results[results$Model == "Standard Theta Model", ]$MASE
  expect_equal(smape, c(16.73, 9.24, 13.85, 4.93, 13.06), tolerance = 0.05)
  expect_equal(mase, c(2.77, 1.12, 0.86, 2.27, 1.16), tolerance = 0.05)
})

test_that("OTM Operating Normally", {
  smape <- results[results$Model == "Optimised Theta Model", ]$sMAPE
  mase <- results[results$Model == "Optimised Theta Model", ]$MASE
  expect_equal(smape, c(16.60, 9.14, 14.11, 4.85, 13.21), tolerance = 0.05)
  expect_equal(mase, c(2.71, 1.10, 0.86, 2.23, 1.14), tolerance = 0.05)
})

test_that("DSTM Operating Normally", {
  smape <- results[results$Model == "Dynamic Standard Theta Model", ]$sMAPE
  mase <- results[results$Model == "Dynamic Standard Theta Model", ]$MASE
  expect_equal(smape, c(16.69, 9.24, 13.82, 4.92, 13.04), tolerance = 0.05)
  expect_equal(mase, c(2.76, 1.12, 0.86, 2.27, 1.16), tolerance = 0.05)
})

test_that("DOTM Operating Normally", {
  smape <- results[results$Model == "Dynamic Optimised Theta Model", ]$sMAPE
  mase <- results[results$Model == "Dynamic Optimised Theta Model", ]$MASE
  expect_equal(smape, c(15.94, 9.28, 13.74, 4.58, 12.90), tolerance = 0.05)
  expect_equal(mase, c(2.59, 1.12, 0.85, 1.94, 1.12), tolerance = 0.05)
})
