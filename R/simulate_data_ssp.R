#' @title Simulate Data for the SSP
#'
#' @param n_sub A single numeric value specifying for how many subjects data should be simulated.
#' @param n_trials A single numeric value specifying how many trials per condition should be simulated.
#' @param par_limits A data.frame containing two variables: min & max with 7 values
#'   for each parameter of the SSP. You have to either include a third variable `par_name` labeling
#'   which row contains information for which parameters or name the rows of the data frame.
#'   Parameter names of the SSP parameters can be obtained via: `dRiftDM::ssp_dm()$free_prms`
#'
#' @export
simulate_data_ssp <- function(n_sub, n_trials, par_limits = NULL, verbose = 0) {
  # set up model object
  ssp_model <- dRiftDM::ssp_dm()
  ssp_model <- dRiftDM::set_free_prms(ssp_model, c("b", "non_dec", "p", "sd_0","r"))

  # prepare lower and upper bounds for parameters
  if (is.null(par_limits)) {
    # default settings
    lower_limits <- c(.4, 0.15, 1, 0.5, 8)
    upper_limits <- c(.8, 0.50, 4, 2.0, 12)
  } else {
    if (all(ssp_model$free_prms %in% rownames(par_limits))) {
      par_limits <- par_limits[ssp_model$free_prms,]
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
  colnames(sub_parms)[1:5] = ssp_model$free_prms

  #simulate data
  sim_data = dRiftDM::simulate_data(
    drift_dm_obj = ssp_model,
    n = n_trials,
    df_prms = sub_parms,
    verbose = verbose
  )

  sim_data$task = 1

  # collect information in list
  data_list <- list(
    sub_parms = data.table::data.table(sub_parms),
    sim_data = data.table::data.table(sim_data)
  )

  # return the data_list
  return(data_list)
}
