# DMC recovery simulation, re-run with a narrower amplitude range
#
# Differences from scripts/Simulation_DMC_Recovery.R (the run behind the package
# datasets), decided after the co-author meeting of 2026-10-20:
#   1. A is drawn from [.10, .32] instead of [.015, .400]. The narrow range
#      matches White, Servant & Logan (2018, Table 2) after conversion to dRiftDM
#      units (.119-.316); see local/R1_white2018_parameter_check.md.
#   2. Split-half reliabilities use the corrected random balanced split in
#      R/get_reliability.R (the earlier row-order split overestimated accuracy
#      reliabilities because dRiftDM lists correct trials before errors).
#   3. Trial-level data are stored with every replication (element `trials`).
#   4. Seeds are fixed per design row.
# Results go to output/Simulation_DMC_Recovery_narrowA/ and
# output/res_DMC_recovery_narrowA.rds. The original results are untouched.
#
# Run with R 4.5.2 (the only local R with dRiftDM) through
# scripts/run_DMC_narrowA.sh. The source package is loaded with pkgload on the
# master and on every worker, so the installed package is not used.

library(SimDesign)
library(dRiftDM)
pkg_path <- here::here()
pkgload::load_all(pkg_path, quiet = TRUE)

nReplications <- 250

Design <- createDesign(
  sample_size = c(25, 50, 100),
  nTrials = c(50, 100, 200)
)

# parameter limits for the five varying parameters (gamma shape `a` keeps the
# dRiftDM default of 2). The amplitude range and the run tag come from the
# environment so that further ranges can be run without copying the script:
#   RUN_TAG=narrowA A_MIN=0.10 A_MAX=0.32   (default; White et al. 2018 range)
#   RUN_TAG=A05-35  A_MIN=0.05 A_MAX=0.35   (slightly wider, 2026-10-20)
a_min <- as.numeric(Sys.getenv("A_MIN", "0.10"))
a_max <- as.numeric(Sys.getenv("A_MAX", "0.32"))
run_tag <- Sys.getenv("RUN_TAG", "narrowA")
cat("Run tag:", run_tag, " A range:", a_min, "-", a_max, "\n")
par_limits <- data.frame(
  min = c(1.5, .4, 0.15, 0.02, a_min),
  max = c(4.0, .8, 0.50, 0.12, a_max),
  row.names = c("muc", "b", "non_dec", "tau", "A")
)

Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition)
  simulate_data_dmc(n_sub = sample_size, n_trials = nTrials,
                    par_limits = fixed_objects$par_limits)
}

Analyse <- function(condition, dat, fixed_objects) {
  ret <- analyze_data(dat)
  ret$trials <- dat$sim_data
  ret
}

Summarise <- function(condition, results, fixed_objects) {
}

# cluster with the source package loaded on every worker
ncores <- as.integer(Sys.getenv("SIM_NCORES", "8"))
master_libs <- .libPaths()
# workers must run R 4.5.2 too; the framework's Rscript would start the default R
cl <- parallel::makeCluster(ncores, rscript = file.path(pkg_path, "scripts", "Rscript45.sh"))
parallel::clusterExport(cl, c("pkg_path", "master_libs"))
invisible(parallel::clusterEvalQ(cl, {
  .libPaths(master_libs)
  library(dRiftDM); library(data.table); library(tidytable)
  pkgload::load_all(pkg_path, quiet = TRUE)
}))
print(parallel::clusterEvalQ(cl, c(R.version.string, .libPaths()[1]))[[1]])

out_dir <- here::here("output", paste0("Simulation_DMC_Recovery_", run_tag))
res_file <- here::here("output", paste0("res_DMC_recovery_", run_tag, ".rds"))
if (!file.exists(res_file) || !dir.exists(out_dir)) {
  res <- runSimulation(design = Design, replications = nReplications,
                       generate = Generate, analyse = Analyse, summarise = Summarise,
                       fixed_objects = list(par_limits = par_limits),
                       seed = 20261020 + seq_len(nrow(Design)),
                       save_details = list(
                         safe = TRUE,
                         out_rootdir = here::here("output"),
                         save_results_dirname = basename(out_dir),
                         save_results_filename = paste0("DMC_Recovery_", run_tag, "_Cond")),
                       save_results = TRUE,
                       cl = cl,
                       packages = c("data.table", "tidytable", "dRiftDM"))
  save(res, file = res_file)
  print(res[, c("sample_size", "nTrials", "REPLICATIONS", "SIM_TIME")])
}
parallel::stopCluster(cl)
