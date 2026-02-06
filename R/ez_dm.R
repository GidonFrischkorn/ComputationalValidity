#' @title Calculate ezDM parameters
#'
#' @description
#'   This functions calculates the ezDM parameters as proposed by Wagenmakers et al. (2007).
#'   For this it takes a vector of reaction times and accuracies, calculates the mean and sd
#'   of the reaction times and the proportion of correct responses and returns the ezDM
#'   parameters.
#'
#' @param RT numeric. A vector of reaction times.
#' @param ACC numeric. A vector of accuracies to determine the proportion correct.
#' @param s numeric. The diffusion constant. Typically set to 1 or 0.1. The default value is 1.
#' @param robust logical. Should robust statistics, that is median and Interquartile range, be used to calculate the ezDM parameters. Default is true.
#' @param use_RTs character. Should only correct RTs ("correct") or all RTs (any other character) be used to calculate mean and sd of RTs.
#'
#' @export
ez_dm <- function(RT, ACC, s = 1, robust = TRUE, use_RTs = "correct"){
  # The default value for the scaling parameter s equals 0.1
  s2 <- s^2

  # compute summary statistics for accuracies
  N = length(ACC)
  Pc = mean(ACC)

  # select RTs to use for summary statistics
  if (use_RTs == "correct") {
    useRTs <- RT[which(ACC == 1)]
  } else {
    useRTs <- RT
  }

  # Check for insufficient data
  if (length(useRTs) < 2) {
    warning("Insufficient correct trials for EZ-DM estimation")
    return(c(v = NA_real_, a = NA_real_, t0 = NA_real_))
  }

  # compute summary statistics for RTs
  if (robust == TRUE) {
    MRT = median(useRTs)
    VRT = (IQR(useRTs)/(qnorm(p = .75)*2))^2
  } else {
    MRT = mean(useRTs)
    VRT = var(useRTs)
  }

  # Check for zero or negative variance
  if (is.na(VRT) || VRT <= 0) {
    warning("Zero or negative RT variance; cannot estimate EZ-DM parameters")
    return(c(v = NA_real_, a = NA_real_, t0 = NA_real_))
  }

  # edge correction if required.
  if (Pc == 0 | Pc == 0.5) {
    Pc = Pc + (1/(2*N))
  } else if (Pc == 1) {
    Pc = Pc - (1/(2*N))
  }

  # Additional check after edge correction
  if (Pc < 0 || Pc >= 1) {
    warning(sprintf("Accuracy out of valid range after edge correction: Pc = %.3f", Pc))
    return(c(v = NA_real_, a = NA_real_, t0 = NA_real_))
  }

  # The function "qlogis" calculates the logit.
  L <- qlogis(Pc)

  # This gives drift rate.
  x <- L*(L*Pc^2 - L*Pc + Pc - .5)/VRT

  # Check if x is valid for taking the 4th root
  if (is.na(x) || x < 0) {
    warning(sprintf("Invalid value for drift rate calculation: x = %.3f", x))
    return(c(v = NA_real_, a = NA_real_, t0 = NA_real_))
  }

  v <- sign(Pc - .5)*s*x^(1/4)

  # This gives boundary separation.
  a <- s2*L/v

  # Check for invalid boundary
  if (is.na(a) || a <= 0 || is.infinite(a)) {
    warning(sprintf("Invalid boundary separation: a = %.3f", a))
    return(c(v = NA_real_, a = NA_real_, t0 = NA_real_))
  }

  # This gives nondecision time.
  y <- -v*a/s2
  MDT <- (a/(2*v)) * (1 - exp(y))/(1 + exp(y))
  t0 <- MRT - MDT

  # Check for negative or excessive non-decision time
  # if (is.na(t0) || t0 < 0 || t0 > MRT) {
  #   warning(sprintf("Invalid non-decision time: t0 = %.3f, MRT = %.3f", t0, MRT))
  #   return(c(v = NA_real_, a = NA_real_, t0 = NA_real_))
  # }

  # Return the calculated parameters
  parms <- c(v,a,t0)
  names(parms) <- c("v","a","t0")
  return(parms)
}
