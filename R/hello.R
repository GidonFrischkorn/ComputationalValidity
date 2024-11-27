# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

sim_data_dmc <- function(nSub, nTrials, par_limits = NULL) {
  if (is.null(par_limits)) {
    lower_limits <- c(1.5, .4, 0.15,0.001,0.02,0.015,10)
    upper_limits <- c(5, .8, 0.5,0.01,0.12,0.40,10.01)
  }

  # simulate parameters for each subject
  sub_parms = dRiftDM::simulate_values(
    lower = lower_limits,
    upper = upper_limits,
    k = nSub
  )

  # rename columns with parameter names
  colnames(sub_parms)[1:7] = dRiftDM::dmc_dm()$free_prms

  #simulate data
  sim_data = simulate_data(
    drift_dm_obj = dmc_dm(),
    n = nTrials,
    df_prms = sub_parms,
    verbose = 0
  )

  # collect information in list
  data_list <- list(
    sub_parms = sub_parms,
    sim_data = sim_data
  )

  # return the data_list
  return(data_list)
}
