# Key quantities side by side for every DMC amplitude-range run found:
# the original run (package datasets, A in [.015, .400]) and each
# output/Simulation_DMC_Recovery_<tag> / Simulation_DMC_Correlations_<tag>.
# Writes local/R1_DMC_amplitude_ranges.md and output/amplitude_ranges_key.csv.
# Reliability caveat: the original run's accuracy reliabilities come from the
# row-order split (artefact); the re-runs use the corrected split.

suppressPackageStartupMessages(library(data.table))
root <- here::here()

load_rda <- function(name) {
  e <- new.env(); load(file.path(root, "data", paste0(name, ".rda")), envir = e)
  as.data.table(get(name, envir = e))
}
relabel <- function(dt) {
  if ("indicator" %in% names(dt)) dt[, indicator := fcase(indicator == "comp", "congruent",
                                                            indicator == "incomp", "incongruent",
                                                            indicator == "diff", "difference",
                                                            default = as.character(indicator))]
  if ("measure" %in% names(dt)) dt[, measure := fcase(measure == "v", "drift", measure == "a", "boundary",
                                                        measure == "t0", "non_dec", default = as.character(measure))]
  dt
}
read_run <- function(dir, element, cond_cols) {
  files <- list.files(dir, pattern = "Cond-[0-9]+$", full.names = TRUE)
  rbindlist(lapply(files, function(f) {
    x <- qs2::qd_read(f)
    cond <- as.data.table(x$condition)[, ..cond_cols]
    res <- rbindlist(lapply(seq_along(x$results), function(i) {
      r <- as.data.table(x$results[[i]][[element]]); r[, nRep := i]; r }), fill = TRUE)
    cbind(res, cond[rep(1, nrow(res))])
  }), fill = TRUE)
}

# ---- original run -----------------------------------------------------------
old_rec <- load_rda("dmc_recovery_parRecovery")[, .(run = "original [.015,.400]",
  SampleSize = as.integer(sub("N = ", "", SampleSize)), nTrials = as.integer(as.character(nTrials)),
  genPar = as.character(genPar), measure = as.character(measure), indicator = as.character(indicator), rec)]
old_rel <- load_rda("dmc_recovery_reliability")[, .(run = "original [.015,.400]",
  SampleSize = as.integer(sub("N = ", "", SampleSize)), nTrials = as.integer(as.character(nTrials)),
  measure = as.character(measure), indicator = as.character(indicator), reliability)]
old_cor <- load_rda("dmc_correlation_recCorrs")[, .(run = "original [.015,.400]",
  nTrials = as.integer(as.character(nTrials)), correlated_par = as.character(correlated_par),
  measure = as.character(measure), indicator = as.character(indicator), correlation,
  emp_corr.A, emp_corr.muc, emp_corr.tau)]

# ---- re-runs ------------------------------------------------------------------
tags <- sub("^Simulation_DMC_Recovery_", "", list.files(file.path(root, "output"), pattern = "^Simulation_DMC_Recovery_"))
runs_rec <- list(old_rec); runs_rel <- list(old_rel); runs_cor <- list(old_cor)
for (tg in tags) {
  rd <- file.path(root, "output", paste0("Simulation_DMC_Recovery_", tg))
  cd <- file.path(root, "output", paste0("Simulation_DMC_Correlations_", tg))
  if (length(list.files(rd, pattern = "Cond-[0-9]+$")) == 0) next
  r <- relabel(read_run(rd, "recovery", c("sample_size", "nTrials")))
  setnames(r, "sample_size", "SampleSize")
  runs_rec[[length(runs_rec) + 1]] <- r[, .(run = tg, SampleSize, nTrials, genPar = as.character(genPar), measure, indicator, rec)]
  l <- relabel(read_run(rd, "reliability", c("sample_size", "nTrials")))
  setnames(l, "sample_size", "SampleSize")
  runs_rel[[length(runs_rel) + 1]] <- l[, .(run = tg, SampleSize, nTrials, measure, indicator, reliability)]
  if (length(list.files(cd, pattern = "Cond-[0-9]+$")) > 0) {
    cc <- relabel(read_run(cd, "correlations", c("nTrials", "correlated_par")))
    for (v in c("emp_corr.A", "emp_corr.muc", "emp_corr.tau")) if (!v %in% names(cc)) cc[, (v) := NA_real_]
    runs_cor[[length(runs_cor) + 1]] <- cc[, .(run = tg, nTrials, correlated_par = as.character(correlated_par),
                                               measure, indicator, correlation, emp_corr.A, emp_corr.muc, emp_corr.tau)]
  }
}
rec <- rbindlist(runs_rec); rel <- rbindlist(runs_rel); cor <- rbindlist(runs_cor, fill = TRUE)

# ---- key quantities (N = 100, 100 trials; transfer N = 200, 100 trials) --------
k1 <- rec[SampleSize == 100 & nTrials == 100 &
            ((genPar == "A" & measure %in% c("RT", "PC") & indicator %in% c("difference", "congruent", "incongruent")) |
             (genPar == "tau" & measure == "RT" & indicator == "difference") |
             (genPar == "AUC" & measure == "RT" & indicator == "difference") |
             (genPar == "b" & measure == "PC" & indicator == "difference") |
             (genPar == "A" & measure == "drift" & indicator == "difference")),
          .(value = mean(rec, na.rm = TRUE)), by = .(run, quantity = paste0("validity ", measure, " ", indicator, " for ", genPar))]
k2 <- rel[SampleSize == 100 & nTrials == 100 & measure %in% c("RT", "PC") & indicator %in% c("difference", "mean"),
          .(value = mean(reliability, na.rm = TRUE)), by = .(run, quantity = paste0("reliability ", measure, " ", indicator))]
k3 <- cor[nTrials == 100 & correlated_par %in% c("A", "tau") & measure == "RT" & indicator == "difference",
          .(value = mean(correlation, na.rm = TRUE),
            gen = mean(as.numeric(get(paste0("emp_corr.", correlated_par[1]))), na.rm = TRUE)),
          by = .(run, correlated_par)]
k3 <- rbind(k3[, .(run, quantity = paste0("transfer RT difference, ", correlated_par, " correlated"), value)],
            k3[, .(run, quantity = paste0("realised parameter correlation, ", correlated_par), value = gen)])
key <- rbind(k1, k2, k3)
key[, run := factor(run, levels = c("original [.015,.400]", setdiff(unique(run), "original [.015,.400]")))]
wide <- dcast(key, quantity ~ run, value.var = "value")
# AUC in the original run is the mean of the A and tau recoveries, not a true correlation
wide[quantity == "validity RT difference for AUC", `original [.015,.400]` := NA_real_]
fwrite(wide, file.path(root, "output", "amplitude_ranges_key.csv"))

fmt <- function(x) ifelse(is.na(x), "–", formatC(x, digits = 2, format = "f"))
md <- c("# DMC amplitude ranges: key quantities side by side",
        "",
        paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), " by scripts/Compare_DMC_Aranges.R. Validities and reliabilities at N = 100, 100 trials; transfer at N = 200, 100 trials; means over replications. The original run's PC reliabilities are the row-order-split artefact. AUC validity is not available for the original run (its stored AUC is the mean of the A and tau recoveries)."),
        "",
        paste0("| quantity | ", paste(names(wide)[-1], collapse = " | "), " |"),
        paste0("|---|", paste(rep("---", ncol(wide) - 1), collapse = "|"), "|"),
        apply(wide, 1, function(r) paste0("| ", r[1], " | ", paste(fmt(as.numeric(r[-1])), collapse = " | "), " |")))
writeLines(md, file.path(root, "local", "R1_DMC_amplitude_ranges.md"))
cat(paste(md, collapse = "\n"), "\n")
