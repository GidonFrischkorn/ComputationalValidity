#' @title Simulate Data for the DMC
#'
#' @param n_sub A single numeric value specifying for how many subjects data should be simulated.
#' @param n_trials A single numeric value specifying how many trials per condition should be simulated.
#' @param par_limits A data.frame containing two variables: min & max with 5 values
#'   for each varying parameter of the DMC (muc, b, non_dec, tau, A).
#'   You have to either include a third variable `par_name` labeling
#'   which row contains information for which parameters or name the rows of the data frame.
#' @param seed Optional random seed for reproducibility. If NULL, no seed is set.
#'
#' @export
simulate_data_dmc <- function(n_sub, n_trials, par_limits = NULL, verbose = 0, seed = NULL) {

  # set up model object
  dmc_model <- dRiftDM::dmc_dm(var_non_dec = FALSE, var_start = FALSE)

  # prepare lower and upper bounds for ALL parameters
  # For parameters that should vary: set different lower/upper
  # For constant parameters: set same values (no variation)
  if (is.null(par_limits)) {
    # default settings
    lower <- c(
      muc = 1.5, b = 0.4, non_dec = 0.15,
      tau = 0.02, A = 0.015
    )
    upper <- c(
      muc = 5, b = 0.8, non_dec = 0.5,
      tau = 0.12, A = 0.40
    )
  } else {
    # Use custom limits for varying parameters
    varying_pars <- c("muc", "b", "non_dec", "tau", "A")
    if (!all(varying_pars %in% rownames(par_limits))) {
      stop("par_limits does not contain limits for some parameters of the DMC.")
    }
    lower <- c(
      muc = par_limits["muc", "min"],
      b = par_limits["b", "min"],
      non_dec = par_limits["non_dec", "min"],
      tau = par_limits["tau", "min"],
      A = par_limits["A", "min"]
    )
    upper <- c(
      muc = par_limits["muc", "max"],
      b = par_limits["b", "max"],
      non_dec = par_limits["non_dec", "max"],
      tau = par_limits["tau", "max"],
      A = par_limits["A", "max"]
    )
  }

  # simulate data directly using dRiftDM's simulate_data
  # This function handles parameter simulation and data generation internally
  data_prms <- dRiftDM::simulate_data(
    object = dmc_model,
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
