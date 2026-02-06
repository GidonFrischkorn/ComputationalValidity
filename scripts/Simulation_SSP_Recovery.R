library(SimDesign)
library(ComputationalValidity)
library(dRiftDM)

# Set Up Design & Number of Replications per condition
nReplications <- 250

Design <- createDesign(
  sample_size = c(25,50,100),
  nTrials = c(50,100,200)
)

# set up model object
ssp_model <- dRiftDM::ssp_dm(var_non_dec = FALSE, var_start = FALSE)

# Set parameters to estimate using flex_prms accessor
fp <- dRiftDM::flex_prms(ssp_model)
fp$prms_to_estimate <- c("b", "non_dec", "p", "sd_0")
dRiftDM::flex_prms(ssp_model) <- fp

# set parameter limits
par_limits = data.frame(
  t(
    rbind(c(.4, 0.15, 1, 0.5),
          c(.8, 0.50, 4, 2.0))
  )
)
colnames(par_limits) <- c("min", "max")
rownames(par_limits) <- dRiftDM::flex_prms(ssp_model)$prms_to_estimate

# Specify functions for generating & analyzing the data
Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition)
  if (!is.null(fixed_objects)) {
    par_limits = fixed_objects$par_limits
  } else {
    par_limits <- NULL
  }

  dat <- simulate_data_ssp(n_sub = sample_size, n_trials = nTrials, par_limits = par_limits)
  dat
}

dat <- Generate(condition = Design[1,], fixed_objects = list(par_limits = par_limits))

Analyse <- function(condition, dat, fixed_objects) {
  ret <- analyze_data(dat)
  ret
}

ret <- Analyse(condition = Design[1,], dat = dat, fixed_objects = list(par_limits = par_limits))

# the summary will be done separately to give us more flexibility
Summarise <- function(condition, results, fixed_objects) {
}

# run simulation if there have been no results saved
if (!file.exists(here::here("output","res_SSP_recovery.rds")) |
    !dir.exists(here::here("output","Simulation_SSP_Recovery"))) {
  res <- runSimulation(design = Design, replications = nReplications,
                       generate = Generate, analyse = Analyse, summarise = Summarise,
                       fixed_objects = list(par_limits = par_limits),
                       save_details = list(
                         safe = TRUE,
                         out_rootdir = "output",
                         save_results_dirname = "Simulation_SSP_Recovery",
                         save_results_filename = "SSP_Recovery_Cond"),
                       save_results = TRUE,
                       parallel = TRUE,
                       ncores = parallel::detectCores()/2,
                       packages = c("ComputationalValidity","data.table","tidytable"))

  save(res, file = here::here("output","res_SSP_recovery.rds"))
}
