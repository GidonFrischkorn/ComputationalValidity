# Download simulation data from OSF (https://osf.io/6qnrv/)
#
# PREREQUISITE: Open ComputationalValidity.Rproj in RStudio before running.
# The here() package must anchor to the repository root for paths to work.
#
# Run once after cloning to populate data/ and output/ from OSF.
# Requires the osfr package: install.packages("osfr")
#
# NOTE: M3 data files (m3_*.rda) are not on OSF — M3 analyses are not part of
# the current manuscript. To generate M3 data, run Simulation_M3_*.R and
# SaveData_M3_*.R scripts.

library(osfr)
library(here)

# Verify here() is anchored to the repository root
if (!file.exists(file.path(here(), "ComputationalValidity.Rproj"))) {
  stop(
    "here() is not pointing to the ComputationalValidity repository root.\n",
    "Current root: ", here(), "\n",
    "Please open ComputationalValidity.Rproj in RStudio and re-run this script.",
    call. = FALSE
  )
}

osf_project <- osf_retrieve_node("6qnrv")
osf_top     <- osf_ls_files(osf_project, n_max = Inf)

# Download data/ ---------------------------------------------------------------
osf_data_dir <- osf_top[osf_top$name == "data", ]
if (nrow(osf_data_dir) == 0) {
  warning("No 'data' folder found on OSF. Check the OSF project at https://osf.io/6qnrv/")
} else {
  osf_files <- osf_ls_files(osf_data_dir, n_max = Inf)
  if (!dir.exists(here("data"))) dir.create(here("data"), recursive = TRUE)
  osf_download(osf_files, path = here("data"), conflicts = "overwrite")
  message("Downloaded ", nrow(osf_files), " file(s) to data/")
}

# Download output/ -------------------------------------------------------------
osf_output_dir <- osf_top[osf_top$name == "output", ]
if (nrow(osf_output_dir) == 0) {
  message("No 'output' folder found on OSF — skipping output download.")
  message("To regenerate output files, run the Simulation_*.R and SaveData_*.R scripts.")
} else {
  osf_output_files <- osf_ls_files(osf_output_dir, n_max = Inf)
  if (!dir.exists(here("output"))) dir.create(here("output"), recursive = TRUE)
  osf_download(osf_output_files, path = here("output"), conflicts = "overwrite")
  message("Downloaded ", nrow(osf_output_files), " file(s) to output/")
}

message("\nDone. Verify your setup with: source('scripts/check_setup.R')")
