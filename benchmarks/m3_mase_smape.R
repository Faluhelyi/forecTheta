# benchmark/teste_modelos_theta.R

# =====================================
# Benchmark: Avaliação dos modelos Theta
# =====================================

# Pacotes necessários
library(Mcomp)
library(parallel)
library(doParallel)
library(iterators)
library(forecast)

# Carrega o código do pacote localmente
devtools::load_all(".")

# =====================
# Configuração do cluster
# =====================
cl <- makeCluster(detectCores(logical = FALSE))
registerDoParallel(cl)

# Carrega funções do pacote em cada nó
clusterEvalQ(cl, devtools::load_all("."))
#clusterExport(cl, c("dotm", "dstm", "otm", "stm"))
clusterExport(cl, c("seasonal_dotm", "seasonal_dstm", "seasonal_otm",
                    "seasonal_stm"))

#clusterExport(cl, c("bagged_seasonal_dotm", "bagged_seasonal_dstm",
#                    "bagged_seasonal_otm", "bagged_seasonal_stm"))

# =====================
# Execução dos modelos em paralelo
# =====================
model_fits <- list()

model_fits$dotm <- foreach(
  i = 1:3003, .packages = c("forecast", "Mcomp")
) %dopar% {
  x <- M3[[i]]$x
  h <- M3[[i]]$h
  #bagged_seasonal_dotm(y = x, h = h)
  seasonal_dotm(y = x, h = h)
  #dotm(y = x, h = h)
}

#model_fits$dstm <- foreach(
#  i = 1:3003, .packages = c("forecast", "Mcomp")
#) %dopar% {
#  x <- M3[[i]]$x
#  h <- M3[[i]]$h
#  seasonal_dstm(y = x, h = h, s_type = "additive", s_test = "unit_root")
#  #dstm(y = x, h = h, s_type = "additive", s_test = "unit_root")
#}

model_fits$otm <- foreach(
  i = 1:3003, .packages = c("forecast", "Mcomp")
) %dopar% {
  x <- M3[[i]]$x
  h <- M3[[i]]$h
  #bagged_seasonal_otm(y = x, h = h, s_type = "multiplicative", s_test = "unit_root")
  seasonal_otm(y = x, h = h)
  #otm(y = x, h = h)
}

#model_fits$stm <- foreach(
#  i = 1:3003, .packages = c("forecast", "Mcomp")
#) %dopar% {
#  x <- M3[[i]]$x
#  h <- M3[[i]]$h
#  seasonal_stm(y = x, h = h, s_type = "additive", s_test = "unit_root")
#  #stm(y = x, h = h, s_type = "additive", s_test = "unit_root")
#}

stopCluster(cl)

# =====================
# Avaliação dos resultados
# =====================

calculate_errors <- function(outt, model_name) {
  forec <- matrix(NA, nrow = 3003, ncol = 18)
  obs <- matrix(NA, nrow = 3003, ncol = 18)
  meanDiff <- rep(1, 3003)

  for (i in 1:3003) {
    x <- M3[[i]]$x
    h <- M3[[i]]$h
    obs[i, 1:h] <- M3[[i]]$xx
    forec[i, 1:h] <- outt[[i]]$mean
    meanDiff[i] <- mean(abs(diff(x, lag = frequency(x))))
  }

  # sMAPE
  sAPE_matrix <- errorMetric(obs = obs, forec = forec, type = "sAPE", statistic = "N")
  smape_vals <- c(
    Yearly = mean(sAPE_matrix[1:645, 1:6]),
    Quarterly = mean(sAPE_matrix[646:1401, 1:8]),
    Monthly = mean(sAPE_matrix[1402:2829, 1:18]),
    Other = mean(sAPE_matrix[2830:3003, 1:8]),
    All = mean(sAPE_matrix, na.rm = TRUE)
  )

  # MASE
  AE_matrix <- errorMetric(obs = obs, forec = forec, type = "AE", statistic = "N")
  ASE_matrix <- AE_matrix / meanDiff
  mase_vals <- c(
    Yearly = mean(ASE_matrix[1:645, 1:6]),
    Quarterly = mean(ASE_matrix[646:1401, 1:8]),
    Monthly = mean(ASE_matrix[1402:2829, 1:18]),
    Other = mean(ASE_matrix[2830:3003, 1:8]),
    All = mean(ASE_matrix, na.rm = TRUE)
  )

  # Retorna os dois vetores em formato data.frame
  data.frame(
    Period = names(smape_vals),
    sMAPE = round(smape_vals, 2),
    MASE = round(mase_vals, 2),
    Model = model_name
  )
}

# Aplicar para todos os modelos
results_list <- list(
  #calculate_errors(model_fits$stm, "Seasonal Standard Theta Model"),
  calculate_errors(model_fits$otm, "SOTM"),
  #calculate_errors(model_fits$dstm, "Dynamic Seasonal Standard Theta Model"),
  calculate_errors(model_fits$dotm, "Dynamic SOTM")
)

# Combinar resultados em um único data.frame
results <- do.call(rbind, results_list)

# =====================
# Exibir resultado final
# =====================
print(results)

# Opcional: salvar em CSV
write.csv(results, "benchmarks/resultados_theta_sazonal_multiplicative.csv", row.names = FALSE)

