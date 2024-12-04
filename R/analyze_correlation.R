#' @title Analyse Data for
#'
#' @param n_sub A single numeric value specifying for how many subjects data should be simulated.
#' @param n_trials A single numeric value specifying how many trials per condition should be simulated.
#' @param par_limits A data.frame containing two variables: min & max with 7 values
#'   for each parameter of the DMC. You have to either include a third variable `par_name` labeling
#'   which row contains information for which parameters or name the rows of the data frame.
#'   Parameter names of the dmc parameters can be obtained via: `dRiftDM::dmc_dm()$free_prms`
#'
#' @export
analyze_correlation <- function(dat) {
  # calculate descriptive performance indicators
  desc_performance <- get_descriptives(data = dat$sim_data)

  # ezDM parameters
  ezDM_performance <- get_ezDM(data = dat$sim_data)

  # combine descriptive and ezDM performance indicators
  all_performance <- rbind(desc_performance, ezDM_performance)

  # reliability estimates
  reliability <- get_reliability(data = dat$sim_data)

  # recovery of DMC parameters
  correlations <- get_correlations(all_performance, reliabilites = reliability)
  correlations <- cbind(correlations, data.frame(as.list(dat$empirical_correlation)))

  return(list(correlations = correlations,
              behavior = desc_performance,
              ezDM = ezDM_performance,
              reliability = reliability,
              genPars = dat$sub_parms,
              genCorr = dat$empirical_correlation))

}
