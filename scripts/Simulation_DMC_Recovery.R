library(SimDesign)
library(ComputationalValidity)

# Set Up Design & Number of Replications per condition
nReplications <- 500

Design <- createDesign(
  sample_size = c(25,50,100),
  nTrials = c(50,100,200)
)

# set parameter limits
par_limits = data.frame(
  t(
    rbind(c(1.5, .4, 0.15,0.001, 0.02, 0.015, 500.00),
          c(5.0, .8, 0.50,0.010, 0.12, 0.400, 500.01))
  )
)
colnames(par_limits) <- c("min", "max")
rownames(par_limits) <- dRiftDM::dmc_dm()$free_prms

# Specify functions for generating & analyzing the data
Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition)
  if(!is.null(fixed_objects)) {
    par_limits = fixed_objects$par_limits
  } else {
    par_limits <- NULL
  }

  dat <- simulate_data_dmc(n_sub = sample_size, n_trials = nTrials)
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
if (!file.exists(here::here("output","res_DMC_recovery.rds")) |
   !dir.exists(here::here("output","Simulation_DMC_Recovery"))) {
  res <- runSimulation(design = Design, replications = nReplications,
                       generate = Generate, analyse = Analyse, summarise = Summarise,
                       fixed_objects = list(par_limits = par_limits),
                       save_details = list(
                         safe = TRUE,
                         out_rootdir = here::here(),
                         save_results_dirname = "output/Simulation_DMC_Recovery",
                         save_results_filename = "DMC_Recovery_Cond"),
                       save_results = TRUE,
                       parallel = TRUE,
                       ncores = parallel::detectCores(),
                       packages = c("ComputationalValidity","data.table","tidytable"))

  save(res, file = here::here("output","res_DMC_recovery.rds"))
}
