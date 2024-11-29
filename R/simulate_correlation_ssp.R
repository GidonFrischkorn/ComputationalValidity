#' @title Simulate Correlated Data for two tasks from the DMC
#'
#' @param n_sub A single numeric value specifying for how many subjects data should be simulated.
#' @param n_trials A single numeric value specifying how many trials per condition should be simulated.
#' @param correlation A single numeric value specifying the correlation between the parameters of the two tasks.
#' @param correlated_par A character value specifying which parameters should be correlated.
#' @param par_limits A data.frame containing two variables: min & max with 7 values
#'   for each parameter of the DMC. You have to either include a third variable `par_name` labeling
#'   which row contains information for which parameters or name the rows of the data frame.
#'   Parameter names of the dmc parameters can be obtained via: `dRiftDM::dmc_dm()$free_prms`
#'
#' @export
simulate_correlation_ssp <- function(n_sub, n_trials, correlation, correlated_par, par_limits = NULL, verbose = 0) {
  # prepare lower and upper bounds for parameters
  if (is.null(par_limits)) {
    # default settings
    lower_limits <- c(.4, 0.15, 0.001, 1, 0.5)
    upper_limits <- c(.8, 0.50, 0.010, 4, 2)
  } else {
    if (all(dRiftDM::ssp_dm()$free_prms %in% rownames(par_limits))) {
      par_limits <- par_limits[dRiftDM::ssp_dm()$free_prms,]
      lower_limits <- par_limits$min
      upper_limits <- par_limits$max
    } else {
      stop("par_limits does not contain limits for some paramters of the DMC.")
    }
  }

  names(lower_limits) <- dRiftDM::ssp_dm()$free_prms
  names(upper_limits) <- dRiftDM::ssp_dm()$free_prms

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
  colnames(sub_parms_task1)[1:5] = dRiftDM::ssp_dm()$free_prms
  colnames(sub_parms_task2)[1:5] = dRiftDM::ssp_dm()$free_prms

  if ("p" == correlated_par) {
    sub_parms_task2$p = simulate_correlated_vars(sub_parms_task1$p, correlation = correlation,
                                                   mean = mean(sub_parms_task1$p), sd = sd(sub_parms_task1$p),
                                                   lb = lower_limits["p"], ub = upper_limits["p"])
    observed_correlation = cor(sub_parms_task1$p, sub_parms_task2$p)
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
  } else if("sd_0" == correlated_par) {
    sub_parms_task2$A = simulate_correlated_vars(sub_parms_task1$sd_0, correlation = correlation,
                                                 mean = mean(sub_parms_task1$sd_0), sd = sd(sub_parms_task1$sd_0),
                                                 lb = lower_limits["sd_0"], ub = upper_limits["sd_0"])
    observed_correlation = cor(sub_parms_task1$sd_0, sub_parms_task2$sd_0)
  } else {
    stop("No valid parameter to correlate specified. Please specify one of the following: p, b, non_dec, sd_0.")
  }

  # simulate data
  sim_data_task1 = dRiftDM::simulate_data(
    drift_dm_obj = dRiftDM::ssp_dm(),
    n = n_trials,
    df_prms = sub_parms_task1,
    verbose = verbose
  )

  sim_data_task2 = dRiftDM::simulate_data(
    drift_dm_obj = dRiftDM::ssp_dm(),
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
    empirical_correlation = c(true_corr = correlation, emp_corr = observed_correlation)
  )

  # return the data_list
  return(data_list)
}
