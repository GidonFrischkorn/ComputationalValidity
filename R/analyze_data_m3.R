#' @title Analysis Pipeline for M3 Parameter Recovery
#'
#' @param dat A list as returned by \code{simulate_data_m3()}, containing
#'   \code{sub_parms} (generating parameters) and \code{sim_data} (trial-level responses).
#'
#' @return A list with elements:
#'   \describe{
#'     \item{recovery}{Parameter recovery correlations from \code{get_parameter_recovery()}.}
#'     \item{behavior}{Behavioral indicator data from \code{get_descriptives_m3()}.}
#'     \item{reliability}{Split-half reliability from \code{get_reliability_m3()}.}
#'     \item{genPars}{The generating parameters.}
#'   }
#'
#' @export
analyze_data_m3 <- function(dat) {

  # Compute behavioral indicators from categorical responses
  desc_performance <- get_descriptives_m3(data = dat$sim_data)

  # Compute split-half reliability
  reliability <- get_reliability_m3(data = dat$sim_data)

  # Parameter recovery: correlate indicators with generating parameters
  recovery <- get_parameter_recovery(desc_performance, dat$sub_parms, df_reliability = reliability)

  return(list(
    recovery    = recovery,
    behavior    = desc_performance,
    reliability = reliability,
    genPars     = dat$sub_parms
  ))
}
