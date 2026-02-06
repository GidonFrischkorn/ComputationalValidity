#' @title Simulate Data for the SSP
#'
#' @param n_sub A single numeric value specifying for how many subjects data should be simulated.
#' @param n_trials A single numeric value specifying how many trials per condition should be simulated.
#' @param par_limits A data.frame containing two variables: min & max with 7 values
#'   for each parameter of the SSP. You have to either include a third variable `par_name` labeling
#'   which row contains information for which parameters or name the rows of the data frame.
#'   Parameter names of the SSP parameters can be obtained via: `dRiftDM::ssp_dm()$free_prms`
#' @param seed Optional random seed for reproducibility. If NULL, no seed is set.
#'
#' @export
simulate_data_ssp <- function(n_sub, n_trials, par_limits = NULL, verbose = 0, seed = NULL) {

  # set up model object
  ssp_model <- dRiftDM::ssp_dm(var_non_dec = FALSE, var_start = FALSE)

  # prepare lower and upper bounds for ALL parameters
  if (is.null(par_limits)) {
    # default settings - only 4 parameters (no r)
    lower <- c(
      b = 0.4, non_dec = 0.15, p = 1,
      sd_0 = 0.5
    )
    upper <- c(
      b = 0.8, non_dec = 0.50, p = 4,
      sd_0 = 2.0
    )
  } else {
    # Use custom limits
    varying_pars <- c("b", "non_dec", "p", "sd_0")
    if (!all(varying_pars %in% rownames(par_limits))) {
      stop("par_limits does not contain limits for some parameters of the SSP.")
    }
    lower <- c(
      b = par_limits["b", "min"],
      non_dec = par_limits["non_dec", "min"],
      p = par_limits["p", "min"],
      sd_0 = par_limits["sd_0", "min"]
    )
    upper <- c(
      b = par_limits["b", "max"],
      non_dec = par_limits["non_dec", "max"],
      p = par_limits["p", "max"],
      sd_0 = par_limits["sd_0", "max"]
    )
  }

  # simulate data directly using dRiftDM's simulate_data
  data_prms <- dRiftDM::simulate_data(
    object = ssp_model,
    n = n_trials,
    k = n_sub,
    lower = lower,
    upper = upper,
    seed = seed,
    progress = verbose
  )


  # Extract simulated parameters and data
  sub_parms <- data_prms$prms
  sim_data <- data_prms$synth_data

  sim_data$task <- 1

  # collect information in list
  data_list <- list(
    sub_parms = data.table::data.table(sub_parms),
    sim_data = data.table::data.table(sim_data)
  )

  # return the data_list
  return(data_list)
}
