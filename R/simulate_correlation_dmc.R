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
simulate_correlation_dmc <- function(n_sub, n_trials, correlation, correlated_par, par_limits = NULL, verbose = 0) {
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

  names(lower_limits) <- dRiftDM::dmc_dm()$free_prms
  names(upper_limits) <- dRiftDM::dmc_dm()$free_prms

  # simulate parameters for each subject
  sub_parms_task1 = dRiftDM::simulate_values(
    lower = lower_limits,
    upper = upper_limits,
    k = n_sub
  )

  sub_parms_task2 = dRiftDM::simulate_values(
    lower = lower_limits,
    upper = upper_limits,
    k = n_sub
  )

  # rename columns with parameter names
  colnames(sub_parms_task1)[1:7] = dRiftDM::dmc_dm()$free_prms
  colnames(sub_parms_task2)[1:7] = dRiftDM::dmc_dm()$free_prms

  if ("muc" == correlated_par) {
    sub_parms_task2$muc = simulate_correlated_vars(sub_parms_task1$muc, correlation = correlation,
                                                   mean = mean(sub_parms_task1$muc), sd = sd(sub_parms_task1$muc),
                                                   lb = lower_limits["muc"], ub = upper_limits["muc"])
    observed_correlation = cor(sub_parms_task1$muc, sub_parms_task2$muc)
  } else if("b" == correlated_par) {
    sub_parms_task2$b = simulate_correlated_vars(sub_parms_task1$b, correlation = correlation,
                                                 mean = mean(sub_parms_task1$b), sd = sd(sub_parms_task1$b),
                                                 lb = lower_limits["b"], ub = upper_limits["b"])
    observerd_correlation = cor(sub_parms_task1$b, sub_parms_task2$b)
  } else if("non_dec" == correlated_par) {
    sub_parms_task2$non_dec = simulate_correlated_vars(sub_parms_task1$non_dec, correlation = correlation,
                                                       mean = mean(sub_parms_task1$non_dec), sd = sd(sub_parms_task1$non_dec),
                                                       lb = lower_limits["non_dec"], ub = upper_limits["non_dec"])
    observed_correlation = cor(sub_parms_task1$non_dec, sub_parms_task2$non_dec)
  } else if("tau" == correlated_par) {
    sub_parms_task2$tau = simulate_correlated_vars(sub_parms_task1$tau, correlation = correlation,
                                                   mean = mean(sub_parms_task1$tau), sd = sd(sub_parms_task1$tau),
                                                   lb = lower_limits["tau"], ub = upper_limits["tau"])
    observed_correlation = cor(sub_parms_task1$tau, sub_parms_task2$tau)
  } else if("A" == correlated_par) {
    sub_parms_task2$A = simulate_correlated_vars(sub_parms_task1$A, correlation = correlation,
                                                 mean = mean(sub_parms_task1$A), sd = sd(sub_parms_task1$A),
                                                 lb = lower_limits["A"], ub = upper_limits["A"])
    observed_correlation = cor(sub_parms_task1$A, sub_parms_task2$A)
  } else {
    stop("No valid parameter to correlate specified. Please specify one of the following: muc, b, non_dec, tau, A.")
  }

  # simulate data
  sim_data_task1 = dRiftDM::simulate_data(
    drift_dm_obj = dRiftDM::dmc_dm(),
    n = n_trials,
    df_prms = sub_parms_task1,
    verbose = verbose
  )

  sim_data_task2 = dRiftDM::simulate_data(
    drift_dm_obj = dRiftDM::dmc_dm(),
    n = n_trials,
    df_prms = sub_parms_task2,
    verbose = verbose
  )

  # merge data together
  sub_parms_task1$task = 1
  sub_parms_task2$task = 2

  sim_data_task1$task = 1
  sim_data_task2$task = 2

  sub_parms = rbind(sub_parms_task1, sub_parms_task2)
  sim_data = rbind(sim_data_task1, sim_data_task2)

  # collect information in list
  data_list <- list(
    sub_parms = data.table::data.table(sub_parms),
    sim_data = data.table::data.table(sim_data),
    empirical_correlation =  c(true_corr = correlation, emp_corr = observed_correlation)
  )

  # return the data_list
  return(data_list)
}
