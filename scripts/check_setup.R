# =============================================================================
# Setup Verification Script
# =============================================================================
# Run this after cloning the repository and running download_data.R to confirm
# your environment is correctly configured before running any analysis scripts.
#
# PREREQUISITE: Open ComputationalValidity.Rproj in RStudio first.
# =============================================================================

library(here)

fails <- character(0)

# 1. here() root ---------------------------------------------------------------
if (!file.exists(file.path(here(), "ComputationalValidity.Rproj"))) {
  fails <- c(fails, paste0(
    "[FAIL] here() does not point to the repository root (current: ", here(), ").\n",
    "       FIX: Open ComputationalValidity.Rproj in RStudio and re-run."
  ))
}

# 2. Required packages ---------------------------------------------------------
required_pkgs <- c(
  "devtools", "here", "osfr",
  "data.table", "tidytable", "ggplot2", "patchwork",
  "pacman", "SimDesign", "dRiftDM", "bmm",
  "scales", "viridis", "stringr", "psychometric"
)
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  pkgs_str <- paste0('"', missing_pkgs, '"', collapse = ", ")
  fails <- c(fails, paste0(
    "[FAIL] Missing packages: ", paste(missing_pkgs, collapse = ", "), "\n",
    "       FIX: install.packages(c(", pkgs_str, "))"
  ))
}

# 3. Package loads from source -------------------------------------------------
load_err <- tryCatch(
  {
    devtools::load_all(".", quiet = TRUE)
    NULL
  },
  error = conditionMessage
)
if (!is.null(load_err)) {
  fails <- c(fails, paste0("[FAIL] devtools::load_all('.') failed: ", load_err))
}

# 4. Manuscript data files -----------------------------------------------------
manuscript_files <- c(
  "dmc_recovery_behavior.rda",    "dmc_recovery_ezDM.rda",
  "dmc_recovery_parRecovery.rda", "dmc_recovery_reliability.rda",
  "dmc_correlation_behavior.rda", "dmc_correlation_ezDM.rda",
  "dmc_correlation_recCorrs.rda", "dmc_correlation_reliability.rda",
  "ssp_recovery_behavior.rda",    "ssp_recovery_ezDM.rda",
  "ssp_recovery_parRecovery.rda", "ssp_recovery_reliability.rda",
  "ssp_correlation_behavior.rda", "ssp_correlation_ezDM.rda",
  "ssp_correlation_recCorrs.rda", "ssp_correlation_reliability.rda"
)
ms_present <- file.exists(file.path(here("data"), manuscript_files))
if (!all(ms_present)) {
  missing_ms <- paste(manuscript_files[!ms_present], collapse = ", ")
  fails <- c(fails, paste0(
    "[FAIL] Missing manuscript data files: ", missing_ms, "\n",
    "       FIX: Run scripts/download_data.R"
  ))
}

# 5. Output files (informational only) -----------------------------------------
output_files <- c(
  "res_DMC_recovery.rds", "res_DMC_correlation.rds",
  "res_SSP_recovery.rds", "res_SSP_correlation.rds"
)
out_present <- file.exists(file.path(here("output"), output_files))
if (!all(out_present)) {
  cat("[NOTE] Output files missing (", sum(!out_present),
      "of 4) — not needed to reproduce figures.\n")
  cat("       FIX: Run scripts/download_data.R or re-run Simulation_*.R scripts.\n")
}

# Result -----------------------------------------------------------------------
if (length(fails) == 0) {
  cat("Setup OK\n")
} else {
  cat(paste(fails, collapse = "\n\n"), "\n")
}
