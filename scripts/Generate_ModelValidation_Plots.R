# ==============================================================================
# Generate Model Validation Plots: Empirical Pattern Alignment
# ==============================================================================
# Demonstrates that DMC and SSP simulations produce behavioral patterns
# consistent with typical empirical conflict-task observations.
# All panels use the pre-computed behavioral data from dmc_recovery_behavior
# and ssp_recovery_behavior (N = 100, 100 trials/condition).
#
# Panel A: Distribution of RT congruency effects (incongruent - congruent RT).
# Panel B: Distributions of mean RT for congruent and incongruent conditions.
# Panel C: Distributions of mean accuracy for congruent and incongruent conditions.
# ==============================================================================

rm(list = ls())
graphics.off()

pacman::p_load(here, data.table, tidytable, ggplot2, patchwork, ComputationalValidity)

# ── Color palette (consistent with existing manuscript figures) ───────────────
col_dmc <- "#1565C0"
col_ssp <- "#C62828"

# ── Congruency condition colors ───────────────────────────────────────────────
col_cong   <- "gray40"   # congruent
col_incong <- "black"    # incongruent

# ── Theme (consistent with existing manuscript figures) ───────────────────────
theme_manuscript <- theme_bw(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.key = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    strip.text = element_text(size = 11, face = "bold")
  )

dir.create(here("figures", "manuscript"), showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# Load data (shared across all panels)
# ==============================================================================
cat("Loading existing simulation data...\n")
data(dmc_recovery_behavior, package = "ComputationalValidity")
data(ssp_recovery_behavior, package = "ComputationalValidity")

# Helper: extract a subset and tag with model label
extract_behavior <- function(dt, indicators, measure_type, model_label) {
  data.table(dt)[
    indicator %in% indicators & measure == measure_type &
      SampleSize == "N = 100" & nTrials == "100"
  ][, .(value, indicator, model = model_label)]
}

# ==============================================================================
# PANEL A: Distribution of RT congruency effects
# ==============================================================================
dmc_rt_diff <- extract_behavior(dmc_recovery_behavior, "difference", "RT", "DMC")[
  , value := value * 1000]  # s → ms
ssp_rt_diff <- extract_behavior(ssp_recovery_behavior, "difference", "RT", "SSP")[
  , value := value * 1000]

rt_diff_data <- rbind(dmc_rt_diff, ssp_rt_diff)
rt_diff_data[, model := factor(model, levels = c("DMC", "SSP"))]

cat(sprintf("DMC RT_diff: median = %.1f ms, range = [%.1f, %.1f]\n",
            median(dmc_rt_diff$value), min(dmc_rt_diff$value), max(dmc_rt_diff$value)))
cat(sprintf("SSP RT_diff: median = %.1f ms, range = [%.1f, %.1f]\n",
            median(ssp_rt_diff$value), min(ssp_rt_diff$value), max(ssp_rt_diff$value)))

p_dist <- ggplot(rt_diff_data, aes(x = value, fill = model, color = model)) +
  annotate("rect",
    xmin = 20, xmax = 100, ymin = -Inf, ymax = Inf,
    fill = "gray90", alpha = 1
  ) +
  geom_vline(xintercept = 0, linewidth = 0.6, linetype = "dashed", color = "gray40") +
  geom_histogram(alpha = 0.5, bins = 50, position = "identity") +
  facet_wrap(~model, ncol = 2) +
  scale_fill_manual(values  = c("DMC" = col_dmc, "SSP" = col_ssp), guide = "none") +
  scale_color_manual(values = c("DMC" = col_dmc, "SSP" = col_ssp), guide = "none") +
  labs(
    x = "RT Congruency Effect (ms)",
    y = "Count",
    title = "A   Distribution of RT Congruency Effects \n(N = 100, 100 trials/condition)"
  ) +
  theme_manuscript +
  theme(plot.title = element_text(face = "bold", size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ==============================================================================
# PANEL B: Mean RT by condition (congruent vs. incongruent)
# ==============================================================================
dmc_rt_cond <- extract_behavior(
  dmc_recovery_behavior, c("congruent", "incongruent"), "RT", "DMC")[, value := value * 1000]
ssp_rt_cond <- extract_behavior(
  ssp_recovery_behavior, c("congruent", "incongruent"), "RT", "SSP")[, value := value * 1000]

rt_cond_data <- rbind(dmc_rt_cond, ssp_rt_cond)
rt_cond_data[, model := factor(model, levels = c("DMC", "SSP"))]
rt_cond_data[, Cond := factor(indicator,
  levels = c("congruent", "incongruent"),
  labels = c("Congruent", "Incongruent")
)]

cat(sprintf("DMC RT: cong = %.1f ms, incong = %.1f ms\n",
  mean(dmc_rt_cond[indicator == "congruent", value]),
  mean(dmc_rt_cond[indicator == "incongruent", value])))
cat(sprintf("SSP RT: cong = %.1f ms, incong = %.1f ms\n",
  mean(ssp_rt_cond[indicator == "congruent", value]),
  mean(ssp_rt_cond[indicator == "incongruent", value])))

p_rt <- ggplot(rt_cond_data, aes(x = value, fill = Cond, color = Cond)) +
  geom_histogram(alpha = 0.5, bins = 50, position = "identity") +
  facet_wrap(~model, ncol = 2, scales = "free_x") +
  scale_fill_manual(
    values = c("Congruent" = col_dmc, "Incongruent" = col_ssp),
    name = NULL
  ) +
  scale_color_manual(
    values = c("Congruent" = col_dmc, "Incongruent" = col_ssp),
    name = NULL
  ) +
  labs(
    x = "Mean RT (ms)",
    y = "Count",
    title = "B   Mean RT by Condition"
  ) +
  theme_manuscript +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  )

# ==============================================================================
# PANEL C: Mean Accuracy by condition (congruent vs. incongruent)
# ==============================================================================
dmc_acc_cond <- extract_behavior(
  dmc_recovery_behavior, c("congruent", "incongruent"), "PC", "DMC")[, value := value * 100]
ssp_acc_cond <- extract_behavior(
  ssp_recovery_behavior, c("congruent", "incongruent"), "PC", "SSP")[, value := value * 100]

acc_cond_data <- rbind(dmc_acc_cond, ssp_acc_cond)
acc_cond_data[, model := factor(model, levels = c("DMC", "SSP"))]
acc_cond_data[, Cond := factor(indicator,
  levels = c("congruent", "incongruent"),
  labels = c("Congruent", "Incongruent")
)]

cat(sprintf("DMC Accuracy: cong = %.1f%%, incong = %.1f%%\n",
  mean(dmc_acc_cond[indicator == "congruent", value]),
  mean(dmc_acc_cond[indicator == "incongruent", value])))
cat(sprintf("SSP Accuracy: cong = %.1f%%, incong = %.1f%%\n",
  mean(ssp_acc_cond[indicator == "congruent", value]),
  mean(ssp_acc_cond[indicator == "incongruent", value])))

p_acc <- ggplot(acc_cond_data, aes(x = value, fill = Cond, color = Cond)) +
  geom_histogram(alpha = 0.5, bins = 50, position = "identity") +
  facet_wrap(~model, ncol = 2, scales = "free_x") +
  scale_fill_manual(
    values = c("Congruent" = col_dmc, "Incongruent" = col_ssp),
    name = NULL
  ) +
  scale_color_manual(
    values = c("Congruent" = col_dmc, "Incongruent" = col_ssp),
    name = NULL
  ) +
  labs(
    x = "Mean Accuracy (%)",
    y = "Count",
    title = "C   Mean Accuracy by Condition"
  ) +
  theme_manuscript +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  )

# ==============================================================================
# COMBINE and SAVE
# ==============================================================================
p_validation <- p_dist / p_rt / p_acc + plot_layout(heights = c(1, 1, 1))

ggsave(
  here("figures", "manuscript", "DMC_SSP_ModelValidation.png"),
  p_validation, width = 10, height = 12, dpi = 300, device = ragg::agg_png
)
ggsave(
  here("figures", "manuscript", "DMC_SSP_ModelValidation.tiff"),
  p_validation, width = 10, height = 12, dpi = 300, device = ragg::agg_tiff
)

message("Saved DMC_SSP_ModelValidation.{png,tiff} to figures/manuscript/")
