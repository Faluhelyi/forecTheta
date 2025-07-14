# Pacotes necessários
library(forecTheta)
library(forecast)
library(Mcomp)
library(foreach)
library(parallel)
library(doParallel)
library(dplyr)
library(tseries)
library(Kendall)

data(M3)

# Função para detectar tendência (linear ou não) com Mann-Kendall
has_trend <- function(y, alpha = 0.05) {
  test <- Kendall::MannKendall(y)
  test$sl < alpha
}

# Função para detectar sazonalidade
seasonal_test = function(y, s_test = c("default", "unit_root")) {
  fq = frequency(y)
  run_s_decomp = FALSE

  if (fq >= 3) {
    if ("logical" %in% class(s_test)) {
      run_s_decomp = s_test
    } else {
      s_test = match.arg(arg = s_test, choices = c("default", "unit_root"))
      yy = y
      if (s_test == "unit_root") {
        if (kpss.test(y)$p.value < 0.05) {
          yy = diff(y)
        }
      }
      xacf = acf(yy, lag.max = fq + 1, plot = FALSE)$acf[-1, 1, 1]
      clim = 1.64 / sqrt(length(yy)) * sqrt(cumsum(c(1, 2 * xacf^2)))
      run_s_decomp = abs(xacf[fq]) > clim[fq]
      rm(yy)
    }
  }

  return(run_s_decomp)
}

# Inicializar cluster
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Loop principal (foreach)
resultados <- foreach(i = 1:3003, .packages = c("forecast", "forecTheta", "tseries")) %dopar% {
  serie <- M3[[i]]
  x <- serie$x
  xx <- serie$xx
  h <- serie$h

  # Frequência a partir do campo "period"
  periodo <- serie$period
  frequencia_label <- switch(tolower(periodo),
                             "yearly" = "Yearly",
                             "quarterly" = "Quarterly",
                             "monthly" = "Monthly",
                             "other" = "Other")

  sazonal <- seasonal_test(x)
  tendencia <- has_trend(x)

  # Modelos
  f_seasonal_otm <- tryCatch(seasonal_dotm(y = x, h = h), error = function(e) return(NULL))
  f_otm <- tryCatch(dotm(y = x, h = h), error = function(e) return(NULL))

  if (is.null(f_seasonal_otm) || is.null(f_otm)) return(NULL)

  # sMAPE
  sape_seasonal <- errorMetric(obs = matrix(xx, nrow = 1),
                               forec = matrix(f_seasonal_otm$mean, nrow = 1),
                               type = "sAPE", statistic = "N")
  sape_noseasonal <- errorMetric(obs = matrix(xx, nrow = 1),
                                 forec = matrix(f_otm$mean, nrow = 1),
                                 type = "sAPE", statistic = "N")

  smape_seasonal <- mean(sape_seasonal, na.rm = TRUE)
  smape_noseasonal <- mean(sape_noseasonal, na.rm = TRUE)

  list(
    frequencia = frequencia_label,
    smape_seasonal_otm = smape_seasonal,
    smape_otm = smape_noseasonal,
    sazonal = sazonal,
    tendencia = tendencia
  )
}

stopCluster(cl)

# Limpeza
resultados <- resultados[!sapply(resultados, is.null)]
df <- do.call(rbind, lapply(resultados, as.data.frame))

# Garantir que todas as frequências apareçam
df$frequencia <- factor(df$frequencia,
                        levels = c("Yearly", "Quarterly", "Monthly", "Other"))

# Cálculo da melhoria percentual
df$improv <- 100 * (df$smape_seasonal_otm - df$smape_otm) / df$smape_otm

# Agregação
tabela6 <- df |>
  group_by(frequencia) |>
  summarise(
    All = round(mean(improv, na.rm = TRUE), 2),
    n_All = n(),
    Seasonal = round(mean(improv[sazonal], na.rm = TRUE), 2),
    n_Seasonal = sum(sazonal),
    NonSeasonal = round(mean(improv[!sazonal], na.rm = TRUE), 2),
    n_NonSeasonal = sum(!sazonal),
    Trended = round(mean(improv[tendencia], na.rm = TRUE), 2),
    n_Trended = sum(tendencia),
    NonTrended = round(mean(improv[!tendencia], na.rm = TRUE), 2),
    n_NonTrended = sum(!tendencia)
  )

# Função para formatar valores
fmt_val <- function(val, n) {
  if (is.na(val)) return(paste0("– (", n, ")"))
  else return(paste0(val, "% (", n, ")"))
}

# Tabela final formatada
tabela6_formatada <- data.frame(
  Frequency = tabela6$frequencia,
  `All (n)` = mapply(fmt_val, tabela6$All, tabela6$n_All),
  `Seasonal (n)` = mapply(fmt_val, tabela6$Seasonal, tabela6$n_Seasonal),
  `Non-seasonal (n)` = mapply(fmt_val, tabela6$NonSeasonal, tabela6$n_NonSeasonal),
  `Trended (n)` = mapply(fmt_val, tabela6$Trended, tabela6$n_Trended),
  `Non-trended (n)` = mapply(fmt_val, tabela6$NonTrended, tabela6$n_NonTrended)
)

# Exibir tabela
print(tabela6_formatada, row.names = FALSE)
