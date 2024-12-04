library(SimDesign)
library(ComputationalValidity)

# Set Up Design & Number of Replications per condition
nReplications <- 250

Design <- createDesign(
  sample_size = c(100),
  nTrials = c(50,100,200),
  correlation = "random",
  correlated_par = c("muc","A","tau","muc-A","muc-tau","A-tau","muc-A-tau")
)

# set up model to simulate from
dmc_model <- dRiftDM::dmc_dm()
dmc_model <- dRiftDM::set_free_prms(dmc_model, c("muc","b", "non_dec", "tau", "A"))
dmc_model <- dRiftDM::set_model_prms(dmc_model,
                                     new_prm_vals = c(
                                       muc = 4, b = 0.6, non_dec = 0.3,
                                       sd_non_dec = 0.002, tau = 0.04, a = 2, A = 0.1,
                                       alpha = 500
                                     ))

# set parameter limits
par_limits = data.frame(
  t(
    rbind(c(1.5, .4, 0.15,0.02,0.015),
          c(5, .8, 0.5,0.12,0.40))
  )
)
colnames(par_limits) <- c("min", "max")
rownames(par_limits) <- dmc_model$free_prms

# Specify functions for generating & analyzing the data
Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition)
  if(!is.null(fixed_objects)) {
    par_limits = fixed_objects$par_limits
  } else {
    par_limits <- NULL
  }

  if (correlation == "randomZ") {
    correlation_Z = runif(1, psych::fisherz(0), psych::fisherz(0.9))
    correlation = psych::fisherz2r(correlation_Z)
  } else if(correlation == "random") {
    correlation = runif(1, 0, 0.9)
  }

  dat <- simulate_correlation_dmc(n_sub = sample_size, n_trials = nTrials,
                                  correlation = correlation, correlated_par = correlated_par,
                                  par_limits = par_limits)
  dat
}

dat <- Generate(condition = Design[19,], fixed_objects = list(par_limits = par_limits))

Analyse <- function(condition, dat, fixed_objects) {
  ret <- analyze_correlation(dat)
  ret
}

ret <- Analyse(condition = Design[19,], dat = dat, fixed_objects = list(par_limits = par_limits))

# the summary will be done separately to give us more flexibility
Summarise <- function(condition, results, fixed_objects) {
}

# run simulation if there have been no results saved
if (!file.exists(here::here("output","res_DMC_correlation.rds")) |
   !dir.exists(here::here("output","Simulation_DMC_Correlations"))) {
  res <- runSimulation(design = Design, replications = nReplications,
                       generate = Generate, analyse = Analyse, summarise = Summarise,
                       fixed_objects = list(par_limits = par_limits),
                       save_details = list(
                         safe = TRUE,
                         out_rootdir = here::here("output"),
                         save_results_dirname = "Simulation_DMC_Correlations",
                         save_results_filename = "DMC_Correlation_Cond"),
                       save_results = TRUE,
                       parallel = TRUE,
                       ncores = parallel::detectCores()/2,
                       packages = c("ComputationalValidity","data.table","tidytable"))

  save(res, file = here::here("output","res_DMC_correlation.rds"))
}
