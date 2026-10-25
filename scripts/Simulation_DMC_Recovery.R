library(SimDesign)
library(ComputationalValidity)
library(dRiftDM)

# Set Up Design & Number of Replications per condition
nReplications <- 250

Design <- createDesign(
  sample_size = c(25,50,100),
  nTrials = c(50,100,200)
)

# set up model to simulate from
dmc_model <- dRiftDM::dmc_dm(var_non_dec = FALSE, var_start = FALSE)
print(dmc_model)

# set parameter limits for the five varying parameters.
# simulate_data_dmc() passes only these to dRiftDM::simulate_data(); the gamma
# shape parameter `a` keeps the dRiftDM default (2). An earlier version of this
# script carried a sixth row (a = 0, 0) that was never used.
# Ranges follow White, Servant & Logan (2018, Table 2) after conversion to
# dRiftDM units; the amplitude range (.015-.400) is wider than theirs
# (.119-.316). These are the limits that produced the package datasets.
par_limits = data.frame(
  t(
    rbind(c(1.5, .4, 0.15, 0.02, 0.015),
          c(4.0, .8, 0.50, 0.12, 0.400))
  )
)
colnames(par_limits) <- c("min", "max")
rownames(par_limits) <- c("muc", "b", "non_dec", "tau", "A")

# Specify functions for generating & analyzing the data
Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition)
  if (!is.null(fixed_objects)) {
    par_limits = fixed_objects$par_limits
  } else {
    par_limits <- NULL
  }

  dat <- simulate_data_dmc(n_sub = sample_size, n_trials = nTrials, par_limits = par_limits)
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
                         out_rootdir = "output",
                         save_results_dirname = "Simulation_DMC_Recovery",
                         save_results_filename = "DMC_Recovery_Cond"),
                       save_results = TRUE,
                       parallel = TRUE,
                       ncores = parallel::detectCores()-4,
                       packages = c("ComputationalValidity","data.table","tidytable"))

  save(res, file = here::here("output","res_DMC_recovery.rds"))
}
