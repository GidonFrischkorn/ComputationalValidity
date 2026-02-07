#' @title Calculate Area Under the Curve for DMC Automatic Activation
#'
#' @description
#' Computes the area under the curve (AUC) of the Gamma-distributed automatic 
#' activation function in the DMC model. The automatic activation follows:
#' f(t) = A * (t/tau)^2 * exp(-t/tau)
#' 
#' The integral from 0 to infinity equals: AUC = A * tau * Gamma(3) = 2 * A * tau
#' 
#' This composite parameter captures the total "dose" of automatic activation,
#' combining both the peak amplitude (A) and temporal dynamics (tau) into a 
#' single theoretically meaningful measure of interference strength.
#'
#' @param A Numeric vector of amplitude parameters (peak activation strength)
#' @param tau Numeric vector of temporal dynamic parameters (activation time course)
#'
#' @return Numeric vector of AUC values representing total automatic activation
#'
#' @details
#' The AUC provides a more interpretable measure than A or tau alone because:
#' - It represents the cumulative impact of automatic activation
#' - It parallels SSP's sd_0 parameter (total interference)
#' - It may show stronger parameter recovery than individual components
#' - It reduces the number of parameters from 5 to 4 for DMC analysis
#'
#' @examples
#' # Single values
#' calculate_auc(A = 0.2, tau = 0.08)
#' 
#' # Vectors
#' A_vals <- c(0.1, 0.2, 0.3)
#' tau_vals <- c(0.05, 0.08, 0.10)
#' calculate_auc(A = A_vals, tau = tau_vals)
#'
#' @export
calculate_auc <- function(A, tau) {
  # Validate inputs
  if (length(A) != length(tau)) {
    stop("A and tau must have the same length")
  }
  
  if (any(A < 0, na.rm = TRUE) || any(tau < 0, na.rm = TRUE)) {
    warning("Negative values detected in A or tau; AUC may be invalid")
  }
  
  # Calculate AUC using closed-form solution
  # For Gamma(3) shape, the integral is: A * tau * Gamma(3) = 2 * A * tau
  auc <- 2 * A * tau
  
  return(auc)
}
