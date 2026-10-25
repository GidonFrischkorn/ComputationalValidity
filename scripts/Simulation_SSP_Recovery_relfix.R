# SSP recovery simulation, re-run with the corrected split-half reliability
#
# Same design and parameter limits as scripts/Simulation_SSP_Recovery.R (the
# run behind the package datasets). Differences: the corrected random
# balanced split in R/get_reliability.R (the earlier row-order split
# overestimated accuracy reliabilities because dRiftDM lists correct trials
# before errors), trial-level data stored with every replication (element
# `trials`), fixed seeds, and the source package loaded with pkgload on the
# master and every worker. Results go to output/Simulation_SSP_Recovery_<tag>/
# and output/res_SSP_recovery_<tag>.rds (RUN_TAG, default "relfix"); the
# original results are untouched. Run through scripts/run_relfix_all.sh.

library(SimDesign)
library(dRiftDM)
pkg_path <- here::here()
pkgload::load_all(pkg_path, quiet = TRUE)

run_tag <- Sys.getenv("RUN_TAG", "relfix")
cat("Run tag:", run_tag, "\n")

nReplications <- 250

Design <- createDesign(
  sample_size = c(25, 50, 100),
  nTrials = c(50, 100, 200)
)

# limits for the four varying parameters (r stays at the dRiftDM default)
par_limits <- data.frame(
  min = c(.4, 0.15, 1, 0.5),
  max = c(.8, 0.50, 4, 2.0),
  row.names = c("b", "non_dec", "p", "sd_0")
)

Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition)
  simulate_data_ssp(n_sub = sample_size, n_trials = nTrials,
                    par_limits = fixed_objects$par_limits)
}

Analyse <- function(condition, dat, fixed_objects) {
  ret <- analyze_data(dat)
  ret$trials <- dat$sim_data
  ret
}

Summarise <- function(condition, results, fixed_objects) {
}

ncores <- as.integer(Sys.getenv("SIM_NCORES", "8"))
master_libs <- .libPaths()
cl <- parallel::makeCluster(ncores, rscript = file.path(pkg_path, "scripts", "Rscript45.sh"))
parallel::clusterExport(cl, c("pkg_path", "master_libs"))
invisible(parallel::clusterEvalQ(cl, {
  .libPaths(master_libs)
  library(dRiftDM); library(data.table); library(tidytable)
  pkgload::load_all(pkg_path, quiet = TRUE)
}))
print(parallel::clusterEvalQ(cl, c(R.version.string, .libPaths()[1]))[[1]])

out_dir <- here::here("output", paste0("Simulation_SSP_Recovery_", run_tag))
res_file <- here::here("output", paste0("res_SSP_recovery_", run_tag, ".rds"))
if (!file.exists(res_file) || !dir.exists(out_dir)) {
  res <- runSimulation(design = Design, replications = nReplications,
                       generate = Generate, analyse = Analyse, summarise = Summarise,
                       fixed_objects = list(par_limits = par_limits),
                       seed = 20262020 + seq_len(nrow(Design)),
                       save_details = list(
                         safe = TRUE,
                         out_rootdir = here::here("output"),
                         save_results_dirname = basename(out_dir),
                         save_results_filename = paste0("SSP_Recovery_", run_tag, "_Cond")),
                       save_results = TRUE,
                       cl = cl,
                       packages = c("data.table", "tidytable", "dRiftDM"))
  save(res, file = res_file)
  print(res[, c("sample_size", "nTrials", "REPLICATIONS", "SIM_TIME")])
}
parallel::stopCluster(cl)
