#' @title Simulate Data for the DMC
#'
#' @param n_sub A single numeric value specifying for how many subjects data should be simulated.
#' @param n_trials A single numeric value specifying how many trials per condition should be simulated.
#' @param par_limits A data.frame containing two variables: min & max with 7 values
#'   for each parameter of the DMC. You have to either include a third variable `par_name` labeling
#'   which row contains information for which parameters or name the rows of the data frame.
#'   Parameter names of the dmc parameters can be obtained via: `dRiftDM::dmc_dm()$free_prms`
#' @param seed Optional random seed for reproducibility. If NULL, no seed is set.
#'
#' @export
simulate_correlation_dmc <- function(n_sub, n_trials, correlation, correlated_par, par_limits = NULL, verbose = 0, seed = NULL) {
  
  # set up model object - match simulate_data_dmc configuration
  dmc_model <- dRiftDM::dmc_dm(var_non_dec = FALSE, var_start = FALSE)

  # prepare lower and upper bounds for ALL parameters
  if (is.null(par_limits)) {
    # default settings - match simulate_data_dmc exactly
    lower_limits <- c(
      muc = 1.5, b = 0.4, non_dec = 0.15, 
      tau = 0.02, A = 0.015
    )
    upper_limits <- c(
      muc = 5, b = 0.8, non_dec = 0.5, 
      tau = 0.12, A = 0.40
    )
  } else {
    # Use custom limits for varying parameters
    varying_pars <- c("muc", "b", "non_dec", "tau", "A")
    if (!all(varying_pars %in% rownames(par_limits))) {
      stop("par_limits does not contain limits for some parameters of the DMC.")
    }
    lower_limits <- c(
      muc = par_limits["muc", "min"],
      b = par_limits["b", "min"],
      non_dec = par_limits["non_dec", "min"],
      tau = par_limits["tau", "min"],
      A = par_limits["A", "min"]
    )
    upper_limits <- c(
      muc = par_limits["muc", "max"],
      b = par_limits["b", "max"],
      non_dec = par_limits["non_dec", "max"],
      tau = par_limits["tau", "max"],
      A = par_limits["A", "max"]
    )
  }

  # Use dRiftDM's simulate_data to generate parameters for both tasks
  data_prms_task1 <- dRiftDM::simulate_data(
    object = dmc_model,
    n = n_trials,
    k = n_sub,
    lower = lower_limits,
    upper = upper_limits,
    seed = seed,
    progress = 0
  )
  
  data_prms_task2 <- dRiftDM::simulate_data(
    object = dmc_model,
    n = n_trials,
    k = n_sub,
    lower = lower_limits,
    upper = upper_limits,
    seed = if (!is.null(seed)) seed + 1 else NULL,
    progress = 0
  )
  
  # Extract parameter matrices
  sub_parms_task1 <- data_prms_task1$prms
  sub_parms_task2 <- data_prms_task2$prms

  empirical_correlation = c(correlation)

  if (grepl("muc",correlated_par)) {
    sub_parms_task2$muc = simulate_correlated_vars(sub_parms_task1$muc, correlation = correlation["muc"],
                                                   mean = mean(sub_parms_task1$muc), sd = sd(sub_parms_task1$muc),
                                                   lb = lower_limits["muc"], ub = upper_limits["muc"])
    observed_correlation = cor(sub_parms_task1$muc, sub_parms_task2$muc)
    empirical_correlation <- append(empirical_correlation,observed_correlation)
  }

  if(grepl("b", correlated_par)) {
    sub_parms_task2$b = simulate_correlated_vars(sub_parms_task1$b, correlation = correlation["b"],
                                                 mean = mean(sub_parms_task1$b), sd = sd(sub_parms_task1$b),
                                                 lb = lower_limits["b"], ub = upper_limits["b"])
    observed_correlation = cor(sub_parms_task1$b, sub_parms_task2$b)
    empirical_correlation <- append(empirical_correlation,observed_correlation)
  }

  if(grepl("non_dec", correlated_par)) {
    sub_parms_task2$non_dec = simulate_correlated_vars(sub_parms_task1$non_dec, correlation = correlation["non_dec"],
                                                       mean = mean(sub_parms_task1$non_dec), sd = sd(sub_parms_task1$non_dec),
                                                       lb = lower_limits["non_dec"], ub = upper_limits["non_dec"])
    observed_correlation = cor(sub_parms_task1$non_dec, sub_parms_task2$non_dec)
    empirical_correlation <- append(empirical_correlation,observed_correlation)
  }

  if(grepl("tau", correlated_par)) {
    sub_parms_task2$tau = simulate_correlated_vars(sub_parms_task1$tau, correlation = correlation["tau"],
                                                   mean = mean(sub_parms_task1$tau), sd = sd(sub_parms_task1$tau),
                                                   lb = lower_limits["tau"], ub = upper_limits["tau"])
    observed_correlation = cor(sub_parms_task1$tau, sub_parms_task2$tau)
    empirical_correlation <- append(empirical_correlation,observed_correlation)
  }

  if (grepl("A", correlated_par)) {
    sub_parms_task2$A = simulate_correlated_vars(sub_parms_task1$A, correlation = correlation["A"],
                                                 mean = mean(sub_parms_task1$A), sd = sd(sub_parms_task1$A),
                                                 lb = lower_limits["A"], ub = upper_limits["A"])
    observed_correlation = cor(sub_parms_task1$A, sub_parms_task2$A)
    empirical_correlation <- append(empirical_correlation,observed_correlation)
  }

  # name the generating and empirical correlations
  corr_par_names <- strsplit(correlated_par,"-")[[1]]
  corr_par_names1 <- paste0("true_corr.",corr_par_names)
  corr_par_names2 <- paste0("emp_corr.",corr_par_names)
  names(empirical_correlation) <- c(corr_par_names1,corr_par_names2)

  # Now regenerate the data with the UPDATED correlated parameters
  # Task 1 uses the original data (parameters were already used)
  sim_data_task1 <- data_prms_task1$synth_data
  
  # Task 2 needs to be regenerated with the correlated parameters
  # Prepare df_prms with the correlated parameters
  if (!("ID" %in% names(sub_parms_task2))) {
    sub_parms_task2$ID <- seq_len(nrow(sub_parms_task2))
  }
  
  # Regenerate task 2 data with correlated parameters
  data_prms_task2_new <- dRiftDM::simulate_data(
    object = dmc_model,
    n = n_trials,
    df_prms = sub_parms_task2,
    seed = if (!is.null(seed)) seed + 1000 else NULL,
    progress = 0
  )
  sim_data_task2 <- data_prms_task2_new$synth_data

  # merge data together
  sub_parms_task1$task <- 1
  sub_parms_task2$task <- 2

  sim_data_task1$task <- 1
  sim_data_task2$task <- 2

  sub_parms = rbind(sub_parms_task1, sub_parms_task2)
  sim_data = rbind(sim_data_task1, sim_data_task2)

  # collect information in list
  data_list <- list(
    sub_parms = data.table::data.table(sub_parms),
    sim_data = data.table::data.table(sim_data),
    empirical_correlation =  empirical_correlation
  )

  # return the data_list
  return(data_list)
}
