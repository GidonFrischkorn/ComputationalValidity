#' @title Model Architecture Figure: DMC and SSP
#'
#' Left column (DMC):
#'   Panel A  (top):    Mean evidence accumulation paths from z = a/2.
#'   Panel A2 (bottom): Automatic activation component µ_A(t) = e·A·(t/τ)·exp(−t/τ).
#'
#' Right column (SSP):
#'   Panel B  (top):    Mean evidence accumulation paths driven by time-varying
#'                      attentional weights (congruent: flankers aid; incongruent:
#'                      flankers oppose until spotlight narrows below crossover sd).
#'   Panel B2 (bottom): Gaussian spotlight profiles at four snapshot times
#'                      corresponding to the vertical dotted lines in Panel B.

library(ggplot2)
library(patchwork)
library(here)
library(ComputationalValidity)

# ── Shared parameters ──────────────────────────────────────────────────────────

a <- 1.0    # correct-response boundary (normalised)
z <- a / 2  # starting point = 0.5

t <- seq(0, 1.0, by = 0.001)

# ── DMC parameters ─────────────────────────────────────────────────────────────

muc <- 1.5   # controlled drift rate
A   <- 0.8   # automatic amplitude; peak µ_A = A
tau <- 0.05  # time constant; µ_A peaks at t = τ

# Automatic activation and its cumulative integral
auto_act <- exp(1) * A * (t / tau) * exp(-t / tau)
auto_int <- A * exp(1) * tau * (1 - (1 + t / tau) * exp(-t / tau))

mean_con_dmc <- z + muc * t + auto_int
mean_inc_dmc <- z + muc * t - auto_int

t_con_dmc <- t[min(which(mean_con_dmc >= a))]
t_inc_dmc <- t[min(which(mean_inc_dmc >= a))]
x_max_dmc <- t_inc_dmc * 1.06

# ── Panel A: DMC evidence accumulation ─────────────────────────────────────────

df_dmc <- data.frame(
  t         = rep(t, 2),
  evidence  = c(ifelse(t <= t_con_dmc, mean_con_dmc, NA_real_),
                ifelse(t <= t_inc_dmc, mean_inc_dmc, NA_real_)),
  condition = factor(rep(c("Congruent", "Incongruent"), each = length(t)),
                     levels = c("Congruent", "Incongruent"))
)

t_ref_hit <- (a - z) / muc
df_linear <- data.frame(
  t        = t[t <= t_ref_hit],
  evidence = z + muc * t[t <= t_ref_hit]
)

p_dmc <- ggplot(df_dmc, aes(x = t, y = evidence,
                             color = condition, linetype = condition)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "gray35") +
  geom_hline(yintercept = z, linewidth = 0.4, color = "gray65", linetype = "dotted") +
  geom_hline(yintercept = a, linewidth = 0.8, color = "gray20") +
  geom_line(data = df_linear, aes(x = t, y = evidence), inherit.aes = FALSE,
            color = "gray55", linetype = "dotdash", linewidth = 0.65) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  annotate("segment",
           x = t_con_dmc, xend = t_inc_dmc, y = 1.05, yend = 1.05,
           color = "gray20", linewidth = 0.8,
           arrow = arrow(ends = "both", type = "closed", length = unit(0.07, "in"))) +
  annotate("text",
           x = mean(c(t_con_dmc, t_inc_dmc)), y = 1.1,
           label = "RT difference", size = 3.5, color = "gray20", hjust = 0.5) +
  annotate("text", x = -0.01, y = a + 0.05, label = "a", size = 4.0, hjust = 1, vjust = 0, color = "gray20") +
  annotate("text", x = -0.01, y = z + 0.05, label = "z", size = 4.0, hjust = 1, vjust = 0, color = "gray55") +
  annotate("text", x = -0.01, y = 0 + 0.05, label = "0", size = 4.0, hjust = 1, vjust = 0, color = "gray35") +
  # µ_c label placed between the asymptote and the incongruent path
  annotate("text",
           x = x_max_dmc * 0.55, y = z + muc * x_max_dmc * 0.55 - 0.05,
           label = expression(mu[c]), size = 4.0, color = "gray52", hjust = 0) +
  scale_color_manual(
    values = c("Congruent" = "#1565C0", "Incongruent" = "#C62828"), name = NULL) +
  scale_linetype_manual(
    values = c("Congruent" = "solid", "Incongruent" = "dashed"), name = NULL) +
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1.00)) +
  coord_cartesian(xlim = c(0, x_max_dmc), ylim = c(-0.08, 1.1)) +
  labs(x = NULL, y = "Accumulated Evidence",
       title = "A   Diffusion Model for Conflict (DMC)") +
  theme_gf() +
  theme(legend.position   = c(0.8, 0.2),
        legend.background = element_blank(),
        legend.key        = element_blank(),
        plot.title        = element_text(face = "bold", size = 12),
        axis.text.x       = element_blank(),
        axis.ticks.x      = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin       = margin(5, 5, 0, 5))

# ── Panel A2: DMC automatic activation ─────────────────────────────────────────

df_auto <- data.frame(
  t    = rep(t[t <= x_max_dmc], 2),
  auto = c( auto_act[t <= x_max_dmc],
           -auto_act[t <= x_max_dmc]),
  condition = factor(
    rep(c("Congruent", "Incongruent"), each = sum(t <= x_max_dmc)),
    levels = c("Congruent", "Incongruent"))
)

y_pk  <-  max(A, muc) * 1.22
y_bot <- -max(A, muc) * 1.22

p_auto <- ggplot(df_auto, aes(x = t, y = auto,
                               color = condition, linetype = condition)) +
  geom_hline(yintercept = 0,    linewidth = 0.5, color = "gray50") +
  geom_line(linewidth = 1.0) +
  geom_vline(xintercept = tau, linewidth = 0.4, color = "gray60", linetype = "dotted") +
  annotate("segment", x = 0, xend = tau, y =  A, yend =  A,
           color = "gray55", linewidth = 0.4, linetype = "dotted") +
  annotate("segment", x = 0, xend = tau, y = -A, yend = -A,
           color = "gray55", linewidth = 0.4, linetype = "dotted") +
  annotate("text", x = -0.01, y =  A,   label = "A",
           size = 3.8, hjust = 1, vjust = 0, color = "gray20") +
  annotate("text", x = -0.01, y = -A,   label = "-A",
           size = 3.8, hjust = 1, vjust = 1, color = "gray20") +
  annotate("text", x = -0.01, y = 0,    label = "0",
           size = 3.8, hjust = 1, vjust = 0, color = "gray50") +
  annotate("text", x = tau, y = y_pk * 0.88, label = expression(tau),
           size = 4.0, hjust = 0.5, color = "gray40") +
  scale_color_manual(
    values = c("Congruent" = "#1565C0", "Incongruent" = "#C62828"), name = NULL) +
  scale_linetype_manual(
    values = c("Congruent" = "solid", "Incongruent" = "dashed"), name = NULL) +
  coord_cartesian(xlim = c(0, x_max_dmc), ylim = c(y_bot, y_pk)) +
  labs(x = "Time", y = expression(mu[A] * "(t)")) +
  theme_gf() +
  theme(legend.position = "none",
        plot.margin     = margin(0, 5, 5, 5))

# ── SSP parameters ─────────────────────────────────────────────────────────────

p_per  <- 3.0   # perceptual input strength
sd_0   <- 1.4   # initial spotlight spread
r_shr  <- 6.0   # shrinkage rate: sd(t) = max(sd_0 − r_shr·t, sd_min)
sd_min <- 0.10  # minimum spotlight size

target_pos  <- 0
flanker_pos <- c(-1, 1)

# Continuous spotlight over time
sd_t  <- pmax(sd_0 - r_shr * t, sd_min)
w_tgt <- dnorm(target_pos, 0, sd_t)
w_flk <- dnorm(flanker_pos[1], 0, sd_t) + dnorm(flanker_pos[2], 0, sd_t)

# Time-varying drift rates
mu_con_t <- p_per * (w_tgt + w_flk)
mu_inc_t <- p_per * (w_tgt - w_flk)

# Mean accumulation paths via numerical integration
dt_step      <- diff(t)[1]
mean_con_ssp <- z + c(0, cumsum(mu_con_t[-length(t)] * dt_step))
mean_inc_ssp <- z + c(0, cumsum(mu_inc_t[-length(t)] * dt_step))

# Boundary-crossing times
t_con_ssp <- t[min(which(mean_con_ssp >= a))]
t_inc_ssp <- if (any(mean_inc_ssp >= a)) t[min(which(mean_inc_ssp >= a))] else max(t)
x_max_ssp <- t_inc_ssp * 1.06

# Crossover time: when µ_inc changes sign (flanker → target dominance)
t_cross <- t[min(which(mu_inc_t >= 0))]

# Snapshot times: sd values → times via sd(t) = sd_0 − r_shr·t
sd_snaps    <- c(1.40, 0.85, 0.40, 0.12)
t_snaps     <- (sd_0 - sd_snaps) / r_shr
snap_labels <- c("t[1]", "t[2]", "t[3]", "t[4]")

# ── Panel B: SSP evidence accumulation ─────────────────────────────────────────

df_ssp_acc <- data.frame(
  t         = rep(t, 2),
  evidence  = c(ifelse(t <= t_con_ssp, mean_con_ssp, NA_real_),
                ifelse(t <= t_inc_ssp, mean_inc_ssp, NA_real_)),
  condition = factor(rep(c("Congruent", "Incongruent"), each = length(t)),
                     levels = c("Congruent", "Incongruent"))
)

df_snaps_vline <- data.frame(
  t     = t_snaps,
  label = snap_labels
)

p_ssp_acc <- ggplot(df_ssp_acc, aes(x = t, y = evidence,
                                     color = condition, linetype = condition)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "gray35") +
  geom_hline(yintercept = z, linewidth = 0.4, color = "gray65", linetype = "dotted") +
  geom_hline(yintercept = a, linewidth = 0.8, color = "gray20") +
  # Snapshot time markers (linking to spotlight panel below)
  geom_vline(data = df_snaps_vline, aes(xintercept = t),
             linewidth = 0.4, color = "gray75", linetype = "dotted",
             inherit.aes = FALSE) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  # RT-difference bracket
  annotate("segment",
           x = t_con_ssp, xend = t_inc_ssp, y = 1.05, yend = 1.05,
           color = "gray20", linewidth = 0.8,
           arrow = arrow(ends = "both", type = "closed", length = unit(0.07, "in"))) +
  annotate("text",
           x = mean(c(t_con_ssp, t_inc_ssp)), y = 1.10,
           label = "RT difference", size = 3.5, color = "gray20", hjust = 0.5) +
  # Boundary / starting-point labels
  annotate("text", x = -0.005, y = a + 0.05, label = "a", size = 4.0, hjust = 1, vjust = 0, color = "gray20") +
  annotate("text", x = -0.005, y = z + 0.05, label = "z", size = 4.0, hjust = 1, vjust = 0, color = "gray55") +
  annotate("text", x = -0.005, y = 0 + 0.05, label = "0", size = 4.0, hjust = 1, vjust = 0, color = "gray35") +
  scale_color_manual(
    values = c("Congruent" = "#1565C0", "Incongruent" = "#C62828"), name = NULL) +
  scale_linetype_manual(
    values = c("Congruent" = "solid", "Incongruent" = "dashed"), name = NULL) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.00)) +
  coord_cartesian(xlim = c(0, x_max_ssp), ylim = c(-0.08, 1.1)) +
  labs(x = NULL, y = "Accumulated Evidence",
       title = "B   Shrinking Spotlight Model (SSP)") +
  theme_gf() +
  theme(legend.position    = c(0.8, 0.2),
        legend.background  = element_blank(),
        legend.key         = element_blank(),
        plot.title         = element_text(face = "bold", size = 12),
        axis.text.x        = element_blank(),
        axis.ticks.x       = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin        = margin(5, 5, 0, 5))

# ── Panel B2: SSP spotlight profiles ───────────────────────────────────────────

x_pos <- seq(-2.0, 2.0, by = 0.01)

df_ssp_seq <- do.call(rbind, lapply(seq_along(sd_snaps), function(i) {
  sd_i      <- sd_snaps[i]
  w_target  <- dnorm(target_pos,      0, sd_i)
  w_flanker <- sum(dnorm(flanker_pos, 0, sd_i))
  net_sign  <- sign(w_target - w_flanker)
  data.frame(
    x = x_pos, weight = dnorm(x_pos, 0, sd_i),
    snap = snap_labels[i], snap_order = i, net_sign = net_sign
  )
}))
df_ssp_seq$snap <- factor(df_ssp_seq$snap, levels = snap_labels)

gauss_peak <- dnorm(0, 0, min(sd_snaps))

p_ssp_spot <- ggplot(df_ssp_seq, aes(x = x, y = weight)) +
  facet_wrap(~ snap, nrow = 1, labeller = label_parsed) +
  geom_vline(xintercept = target_pos,  linewidth = 0.8, color = "gray40") +
  geom_vline(xintercept = flanker_pos, linewidth = 0.8, color = "gray40",
             linetype = "dashed") +
  geom_line(linewidth = 1.1, color = "#1565C0") +
  # sd_0 bracket — first facet only
  geom_segment(
    data = data.frame(
      snap = factor(snap_labels[1], levels = snap_labels),
      x = -sd_snaps[1], xend = sd_snaps[1],
      y    = 0,
      yend = 0),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "#1565C0", linewidth = 0.8, inherit.aes = FALSE,
    arrow = arrow(ends = "both", type = "closed", length = unit(0.06, "in"))
  ) +
  geom_text(
    data = data.frame(
      snap = factor(snap_labels[1], levels = snap_labels),
      x = 0.5, y = -0.25),
    aes(x = x, y = y, label = "sd[0]"),
    parse = TRUE, size = 4.0, color = "#1565C0", hjust = 0.5,
    inherit.aes = FALSE
  ) +
  # sd bracket — second facet to show narrowing
  geom_segment(
    data = data.frame(
      snap = factor(snap_labels[2], levels = snap_labels),
      x = -sd_snaps[2], xend = sd_snaps[2],
      y    = 0,
      yend = 0),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "#1565C0", linewidth = 0.8, inherit.aes = FALSE,
    arrow = arrow(ends = "both", type = "closed", length = unit(0.06, "in"))
  ) +
  # r_d annotation — arrow from sd_0 to sd(t_2) showing shrinkage rate
  geom_segment(
    data = data.frame(
      snap = factor(snap_labels[2], levels = snap_labels),
      x = sd_snaps[1], xend = sd_snaps[2],
      y    = -0.15,
      yend = -0.15),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "#C62828", linewidth = 0.8, inherit.aes = FALSE,
    arrow = arrow(ends = "last", type = "closed", length = unit(0.07, "in"))
  ) +
  # faint reference line at sd_0 in second facet
  geom_segment(
    data = data.frame(
      snap = factor(snap_labels[2], levels = snap_labels),
      x = sd_snaps[1], xend = sd_snaps[1],
      y    = -0.20,
      yend = 0),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "gray55", linewidth = 0.4, linetype = "dotted", inherit.aes = FALSE
  ) +
  geom_text(
    data = data.frame(
      snap = factor(snap_labels[2], levels = snap_labels),
      x = mean(c(sd_snaps[1], sd_snaps[2])) + 0.15, y = -0.42),
    aes(x = x, y = y, label = "r[d]"),
    parse = TRUE, size = 5.0, color = "#C62828", hjust = 0.5,
    inherit.aes = FALSE
  ) +
  coord_cartesian(ylim = c(-0.52, gauss_peak * 1.06)) +
  labs(x = "Spatial Position", y = "Attentional Weight") +
  theme_gf() +
  theme(strip.background = element_rect(fill = "gray95", color = NA),
        strip.text       = element_text(size = 10),
        panel.spacing    = unit(0.3, "lines"),
        plot.margin      = margin(0, 5, 5, 5))

# ── Combine and save ───────────────────────────────────────────────────────────

left_col  <- wrap_plots(p_dmc,     p_auto,    ncol = 1, heights = c(3, 2))
right_col <- wrap_plots(p_ssp_acc, p_ssp_spot, ncol = 1, heights = c(3, 2))
p_arch    <- (left_col | right_col) + plot_layout(widths = c(1, 1.6))

ggsave(
  here("figures", "manuscript", "Model_Architecture.png"),
  p_arch, width = 14, height = 7, dpi = 300, device = ragg::agg_png
)
ggsave(
  here("figures", "manuscript", "Model_Architecture.tiff"),
  p_arch, width = 14, height = 7, dpi = 300, device = ragg::agg_tiff
)

message("Saved Model_Architecture.{png,tiff} to figures/manuscript/")
