#' @title Analysis Pipeline for M3 Cross-Task Correlations
#'
#' @param dat A list as returned by \code{simulate_correlation_m3()}, containing
#'   \code{sub_parms}, \code{sim_data}, and \code{empirical_correlation}.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{correlations}{Cross-task correlations for all indicators, with reliability correction.}
#'     \item{behavior}{Behavioral indicator data from \code{get_descriptives_m3()}.}
#'     \item{reliability}{Split-half reliability from \code{get_reliability_m3()}.}
#'     \item{genPars}{The generating parameters.}
#'     \item{genCorr}{The empirical parameter correlations.}
#'   }
#'
#' @export
analyze_correlation_m3 <- function(dat) {

  # Compute behavioral indicators
  desc_performance <- get_descriptives_m3(data = dat$sim_data)

  # Compute split-half reliability
  reliability <- get_reliability_m3(data = dat$sim_data)

  # Cross-task correlations (reuse existing function)
  correlations <- get_correlations(desc_performance, reliabilites = reliability)
  correlations <- cbind(correlations, data.frame(as.list(dat$empirical_correlation)))

  return(list(
    correlations = correlations,
    behavior     = desc_performance,
    reliability  = reliability,
    genPars      = dat$sub_parms,
    genCorr      = dat$empirical_correlation
  ))
}
