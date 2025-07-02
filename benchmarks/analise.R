theta_additive <- read.csv("benchmarks/resultados_theta_additive.csv", stringsAsFactors = FALSE)
theta_sazonal_additive <- read.csv("benchmarks/resultados_theta_sazonal_additive.csv", stringsAsFactors = FALSE)

variacao_percentual <- data.frame(
  Period = theta_sazonal_additive$Period,
  pct_change_sMAPE = round((theta_sazonal_additive$sMAPE - theta_additive$sMAPE) / theta_additive$sMAPE * 100, 2),
  pct_change_MASE  = round((theta_sazonal_additive$MASE - theta_additive$MASE) / theta_additive$MASE * 100, 2),
  Model = theta_sazonal_additive$Model
)

print(variacao_percentual)

write.csv(variacao_percentual, "benchmarks/pct_change_additive.csv", row.names = FALSE)

##########################################################################################################################################

theta_multiplicative <- read.csv("benchmarks/resultados_theta_multiplicative.csv", stringsAsFactors = FALSE)
theta_sazonal_multiplicative <- read.csv("benchmarks/resultados_theta_sazonal_multiplicative.csv", stringsAsFactors = FALSE)

variacao_percentual <- data.frame(
  Period = theta_sazonal_multiplicative$Period,
  pct_change_sMAPE = round((theta_sazonal_multiplicative$sMAPE - theta_multiplicative$sMAPE) / theta_multiplicative$sMAPE * 100, 2),
  pct_change_MASE  = round((theta_sazonal_multiplicative$MASE - theta_multiplicative$MASE) / theta_multiplicative$MASE * 100, 2),
  Model = theta_sazonal_multiplicative$Model
)

print(variacao_percentual)

write.csv(variacao_percentual, "benchmarks/pct_change_multiplicative.csv", row.names = FALSE)
