
#' @title Simulate correlated variables
#'
#' @description This function simulates a correlated variable from a given variable.
#'
#' @param values A numeric vector of values from which to simulate the correlated variable.
#' @param correlation A numeric value between -1 and 1 indicating the correlation between the simulated variable and the given variable.
#' @param mean A numeric value indicating the mean of the simulated variable.
#' @param sd A numeric value indicating the standard deviation of the simulated variable.
#' @param lb A numeric value indicating the lower bound of the simulated variable.
#' @param ub A numeric value indicating the upper bound of the simulated variable.
#'
#' @return A numeric vector of simulated correlated variables.
#'
#' @examples
#' values <- rnorm(100, 0, 1)
#' correlated_values <- simulate_correlated_vars(values, correlation = 0.5, mean = 10, sd = 2, lb = 4, ub = 16)
#' cor(values, correlated_values)
#'
#' @export
simulate_correlated_vars <- function(values, correlation, mean = 0, sd = 1, lb = -Inf, ub = Inf) {
  # Simulate z-standardized correlated variable from values
  z_correlated_var <- correlation * scale(values) + sqrt(1 - correlation^2) * rnorm(length(values), 0, 1)

  # Transform z-standardized correlated variable to original scale
  correlated_var <- z_correlated_var * sd + mean

  # Apply lower and upper bounds
  bounded_correlated_var <- pmin(pmax(correlated_var, lb), ub)

  # Return bounded correlated variable
  return(bounded_correlated_var)
}
