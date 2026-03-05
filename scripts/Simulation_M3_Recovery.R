library(SimDesign)
library(ComputationalValidity)
library(bmm)

# Set Up Design & Number of Replications per condition
nReplications <- 250

Design <- createDesign(
  sample_size = c(25, 50, 100),
  nTrials = c(50, 100, 200)
)

# Set parameter limits for M3 (c, a, f)
par_limits <- data.frame(
  min = c(0.5, 0.5, 0.1),
  max = c(3.5, 3.0, 0.9),
  row.names = c("c", "a", "f")
)

# Specify functions for generating & analyzing the data
Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition)
  if (!is.null(fixed_objects)) {
    par_limits <- fixed_objects$par_limits
  } else {
    par_limits <- NULL
  }

  dat <- simulate_data_m3(n_sub = sample_size, n_trials = nTrials, par_limits = par_limits)
  dat
}

dat <- Generate(condition = Design[1, ], fixed_objects = list(par_limits = par_limits))

Analyse <- function(condition, dat, fixed_objects) {
  ret <- analyze_data_m3(dat)
  ret
}

ret <- Analyse(condition = Design[1, ], dat = dat, fixed_objects = list(par_limits = par_limits))

# the summary will be done separately to give us more flexibility
Summarise <- function(condition, results, fixed_objects) {
}

# run simulation if there have been no results saved
if (!file.exists(here::here("output", "res_M3_recovery.rds")) |
    !dir.exists(here::here("output", "Simulation_M3_Recovery"))) {
  res <- runSimulation(design = Design, replications = nReplications,
                       generate = Generate, analyse = Analyse, summarise = Summarise,
                       fixed_objects = list(par_limits = par_limits),
                       save_details = list(
                         safe = TRUE,
                         out_rootdir = "output",
                         save_results_dirname = "Simulation_M3_Recovery",
                         save_results_filename = "M3_Recovery_Cond"),
                       save_results = TRUE,
                       parallel = TRUE,
                       ncores = parallel::detectCores() - 4,
                       packages = c("ComputationalValidity", "bmm", "data.table", "tidytable"))

  save(res, file = here::here("output", "res_M3_recovery.rds"))
}
