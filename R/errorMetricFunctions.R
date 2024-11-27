errorMetric <- function(obs, forec, type="sAPE", statistic="M"){
	
	if( !any(type==c("AE","SE","APE","sAPE")) ) stop("Error in errorMetric function: this error type has not been implemented.")
	if( !any(statistic==c("M","Md","N")) ) stop("Error in errorMetric function: this statistic has not been implemented.")
	if( is.ts(obs) || is.ts(forec) ){
		obs = as.numeric(obs)	
		forec = as.numeric(forec)
	}
	obs = as.matrix(obs)
	forec = as.matrix(forec)
	if(any(dim(obs)!=dim(forec)))  stop("Error in errorMetric function: the dimensions of the vectors are different.")
	
	if(type == "AE")
		errors = abs(obs - forec) 	
	
	if(type == "SE")
		errors = (obs - forec)^2 

	if(type == "APE")
		errors = abs( 100*(obs - forec)/obs )
	
	if(type == "sAPE")
		errors =  abs( 200*(obs - forec)/(abs(obs) + abs(forec)) )	
	
	if(statistic == "M")
		return(  mean(errors, na.rm=TRUE) )
	
	if(statistic == "Md")
		return(  median(errors, na.rm=TRUE) )
	
	return( errors )	

}

#' Calculate Coverage Rate of Prediction Intervals for Time Series
#'
#' This function computes the coverage rate of prediction intervals for a given time series.
#'
#' @param actual_values A numeric vector or a time series object (`ts`) containing the actual observed values.
#' @param lower_bounds A numeric vector or a time series object (`ts`) containing the lower bounds of the prediction intervals.
#' @param upper_bounds A numeric vector or a time series object (`ts`) containing the upper bounds of the prediction intervals.
#' @return A numeric value between 0 and 1 representing the proportion of observations within the prediction intervals.
#' @examples
#' # Example usage with numeric vectors
#' actual_values <- ts(c(100, 105, 110, 95, 120), start = c(2023, 1), frequency = 12)
#' lower_bounds <- ts(c(90, 100, 105, 85, 110), start = c(2023, 1), frequency = 12)
#' upper_bounds <- ts(c(110, 115, 120, 105, 130), start = c(2023, 1), frequency = 12)
#' calculate_coverage_rate(actual_values, lower_bounds, upper_bounds)
#' @export

calculate_coverage_rate <- function(actual_values, lower_bounds, upper_bounds) {
  if (!is.numeric(actual_values) && !is.ts(actual_values)) {
    stop("actual_values must be a numeric vector or a time series object (`ts`).")
  }
  if (!is.numeric(lower_bounds) && !is.ts(lower_bounds)) {
    stop("lower_bounds must be a numeric vector or a time series object (`ts`).")
  }
  if (!is.numeric(upper_bounds) && !is.ts(upper_bounds)) {
    stop("upper_bounds must be a numeric vector or a time series object (`ts`).")
  }
  
  if (is.ts(actual_values)) actual_values <- as.numeric(actual_values)
  if (is.ts(lower_bounds)) lower_bounds <- as.numeric(lower_bounds)
  if (is.ts(upper_bounds)) upper_bounds <- as.numeric(upper_bounds)
  
  if (length(actual_values) != length(lower_bounds) || length(actual_values) != length(upper_bounds)) {
    stop("The lengths of actual_values, lower_bounds, and upper_bounds must be equal.")
  }
  
  coverage <- (actual_values >= lower_bounds) & (actual_values <= upper_bounds)
  coverage_rate <- mean(coverage)
  return(coverage_rate)
}

#' Calculate Mean Scaled Interval Score (MSIS)
#'
#' Computes the MSIS for prediction intervals, measuring the accuracy and precision 
#' of forecast intervals while considering scaling based on past errors.
#'
#' @param actual A numeric vector of actual values.
#' @param lower A numeric vector of lower bounds of the prediction intervals.
#' @param upper A numeric vector of upper bounds of the prediction intervals.
#' @param scale A numeric value for scaling, typically based on the mean absolute 
#'              error of a naive forecast over a training period.
#' @param alpha A numeric value for the significance level of the interval (default is 0.05).
#'
#' @return A single numeric value representing the MSIS.
#' @examples
#' actual <- c(100, 110, 90)
#' lower <- c(95, 105, 85)
#' upper <- c(105, 115, 95)
#' scale <- 10
#' msis(actual, lower, upper, scale)
#' @export
msis <- function(actual, lower, upper, scale, alpha = 0.05) {
  if (length(actual) != length(lower) || length(actual) != length(upper)) {
    stop("All input vectors must have the same length.")
  }
  
  n <- length(actual)
  interval_score <- mean((upper - lower) + 
                           (2 / alpha) * (lower - actual) * (actual < lower) + 
                           (2 / alpha) * (actual - upper) * (actual > upper))
  
  msis_value <- interval_score / scale
  return(msis_value)
}

#' Calculate Absolute Coverage Difference (ACD)
#'
#' Computes the ACD for prediction intervals, measuring how close the actual 
#' coverage is to the expected coverage level.
#'
#' @param actual A numeric vector of actual values.
#' @param lower A numeric vector of lower bounds of the prediction intervals.
#' @param upper A numeric vector of upper bounds of the prediction intervals.
#' @param alpha A numeric value for the significance level of the interval (default is 0.05).
#'
#' @return A single numeric value representing the ACD.
#' @examples
#' actual <- c(100, 110, 90)
#' lower <- c(95, 105, 85)
#' upper <- c(105, 115, 95)
#' acd(actual, lower, upper)
#' @export
acd <- function(actual, lower, upper, alpha = 0.05) {
  if (length(actual) != length(lower) || length(actual) != length(upper)) {
    stop("All input vectors must have the same length.")
  }
  
  n <- length(actual)
  in_interval <- (actual >= lower & actual <= upper)
  coverage_rate <- mean(in_interval)
  acd_value <- abs(coverage_rate - (1 - alpha))
  
  return(acd_value)
}

#' Evaluate Coverage Rates for Multiple Time Series
#'
#' This function evaluates the coverage rates of prediction intervals across multiple time series.
#' It computes summary statistics and generates a creative visualization of the distribution of coverage rates.
#'
#' @param coverage_rates A numeric vector containing the coverage rates for all time series.
#' @return A list containing summary statistics (mean, median, standard deviation) and a visualization.
#' @examples
#' # Example usage
#' set.seed(123)
#' coverage_rates <- runif(3003, 0.8, 1) # Simulated coverage rates
#' result <- evaluate_coverage_rates(coverage_rates)
#' print(result$summary)
#' @export
evaluate_coverage_rates <- function(coverage_rates) {
  if (!is.numeric(coverage_rates)) {
    stop("The input coverage_rates must be a numeric vector.")
  }
  
  summary_stats <- list(
    mean = mean(coverage_rates),
    median = median(coverage_rates),
    sd = sd(coverage_rates),
    min = min(coverage_rates),
    max = max(coverage_rates)
  )
  
  library(ggplot2)
  
  coverage_plot <- ggplot(data.frame(coverage_rates), aes(x = coverage_rates)) +
    geom_histogram(
      aes(y = ..density..),
      bins = 30,
      fill = "skyblue",
      color = "black",
      alpha = 0.7
    ) +
    geom_density(color = "darkblue", size = 1) +
    geom_vline(aes(xintercept = summary_stats$mean),
               color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = summary_stats$median),
               color = "green", linetype = "dashed", size = 1) +
    labs(
      title = "Distribution of Coverage Rates Across Time Series",
      subtitle = paste0("Mean: ", round(summary_stats$mean, 2), 
                        ", Median: ", round(summary_stats$median, 2)),
      x = "Coverage Rate",
      y = "Density"
    ) +
    theme_minimal()
  
  print(coverage_plot)
  
  return(list(
    summary = summary_stats,
    plot = coverage_plot
  ))
}