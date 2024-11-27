#' @title Simulate Data for the DMC
#'
#' @param n_sub A single numeric value specifying for how many subjects data should be simulated.
#' @param n_trials A single numeric value specifying how many trials per condition should be simulated.
#' @param par_limits A data.frame containing two variables: min & max with 7 values
#'   for each parameter of the DMC. You have to either include a third variable `par_name` labeling
#'   which row contains information for which parameters or name the rows of the data frame.
#'   Parameter names of the dmc parameters can be obtained via: `dRiftDM::dmc_dm()$free_prms`
#'
#' @export
simulate_data_dmc <- function(n_sub, n_trials, par_limits = NULL, verbose = 0) {
  # prepare lower and upper bounds for parameters
  if (is.null(par_limits)) {
    # default settings
    lower_limits <- c(1.5, .4, 0.15,0.001,0.02,0.015,10)
    upper_limits <- c(5, .8, 0.5,0.01,0.12,0.40,10.01)
  } else {
    if (all(dRiftDM::dmc_dm()$free_prms %in% rownames(par_limits))) {
      par_limits <- par_limits[dRiftDM::dmc_dm()$free_prms,]
      lower_limits <- par_limits$min
      upper_limits <- par_limits$max
    } else {
      stop("par_limits does not contain limits for some paramters of the DMC.")
    }
  }

  # simulate parameters for each subject
  sub_parms = dRiftDM::simulate_values(
    lower = lower_limits,
    upper = upper_limits,
    k = n_sub
  )

  # rename columns with parameter names
  colnames(sub_parms)[1:7] = dRiftDM::dmc_dm()$free_prms

  #simulate data
  sim_data = dRiftDM::simulate_data(
    drift_dm_obj = dRiftDM::dmc_dm(),
    n = n_trials,
    df_prms = sub_parms,
    verbose = verbose
  )

  # collect information in list
  data_list <- list(
    sub_parms = data.table::data.table(sub_parms),
    sim_data = data.table::data.table(sim_data)
  )

  # return the data_list
  return(data_list)
}
