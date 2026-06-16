#-------------------------------------------------------#
# Coffee & climate change replication
# 19_honestdid_figure.R
# Generates Figure for the supplementary material:
# HonestDiD (Rambachan & Roth 2023) sensitivity plot —
# traditional group, relative-magnitude restrictions.
#
# Reads: data/honestdid_trad.rds  (produced by 10_frontier_robustness.R)
# Writes: <paper>/figures/honestdid_sensitivity_trad.pdf
#         <paper>/figures/honestdid_sensitivity_trad.png
#-------------------------------------------------------#

source("R/01_setup.R")
pacman::p_load(ggplot2, scales)

# ── Paths ──────────────────────────────────────────────────────────────────────
paper_fig <- paste0(
  "/Users/jcmunoz/Library/CloudStorage/Dropbox/Apps/Overleaf/",
  "Paper_Climate_Change_Coffee_2026 JH/figures"
)

# ── Load pre-computed HonestDiD results ────────────────────────────────────────
h    <- readRDS(glue("{data}/honestdid_trad.rds"))
res  <- h$honest    # data.frame: Mbar, lb, ub
orig <- h$original  # list: lb, ub  (Mbar = 0, original CI)

cat("== honestdid_trad.rds structure ==\n")
cat("honest columns:", paste(names(res), collapse = ", "), "\n")
print(head(res))
cat("original lb:", orig$lb, " ub:", orig$ub, "\n")

# ── Build plotting data: combine original (Mbar=0) + sensitivity rows ──────────
# Robust to different column name conventions in HonestDiD versions
if ("lb" %in% names(res)) {
  lb_col <- "lb"; ub_col <- "ub"
} else if ("CI_lower_bound" %in% names(res)) {
  lb_col <- "CI_lower_bound"; ub_col <- "CI_upper_bound"
} else {
  stop("Unknown column names in HonestDiD output: ", paste(names(res), collapse=", "))
}

# Mbar column
mbar_col <- if ("Mbar" %in% names(res)) "Mbar" else names(res)[1]

plot_df <- rbind(
  data.frame(Mbar = 0, lb = orig$lb, ub = orig$ub, type = "Original CI"),
  data.frame(Mbar = res[[mbar_col]], lb = res[[lb_col]], ub = res[[ub_col]],
             type = "Robust CI")
)

# Restrict to Mbar 0, 0.5, 1.0, 1.5, 2.0 as in the manuscript text
plot_df <- plot_df[plot_df$Mbar <= 2.0, ]

cat("\n== Plot data ==\n")
print(plot_df)

# ── Figure ─────────────────────────────────────────────────────────────────────
p <- ggplot(plot_df, aes(x = Mbar, color = type, fill = type)) +
  geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.20, color = NA) +
  geom_line(aes(y = lb), linewidth = 0.8) +
  geom_line(aes(y = ub), linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  scale_x_continuous(
    name = expression(bar(M) ~ "(relative-magnitude restriction)"),
    breaks = seq(0, 2, by = 0.5),
    labels = c("0\n(Original)", "0.5", "1.0", "1.5", "2.0")
  ) +
  scale_y_continuous(
    name = expression("Average post-period effect (000s m"^2*")"),
    labels = label_number(accuracy = 0.1)
  ) +
  scale_color_manual(
    values = c("Original CI" = "#1f78b4", "Robust CI" = "#e31a1c"),
    name = NULL
  ) +
  scale_fill_manual(
    values = c("Original CI" = "#1f78b4", "Robust CI" = "#e31a1c"),
    name = NULL
  ) +
  labs(
    caption = paste0(
      "Notes: Confidence sets for the average post-period ATT under relative-magnitude ",
      "restrictions \\u0100{M}. The original CI (\\u0100{M}=0) assumes parallel trends ",
      "hold exactly. Robust CIs allow the post-period trend violation to be at most ",
      "\\u0100{M} times the largest pre-period deviation. Shaded bands show the 95% ",
      "confidence set. Traditional crop group. Clustered standard errors at the ",
      "rural-division (vereda) level."
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size = 10),
    panel.grid.minor  = element_blank(),
    plot.caption      = element_blank(),   # caption goes in LaTeX
    axis.title        = element_text(size = 10),
    plot.margin       = margin(6, 10, 6, 6)
  )

# ── Save ───────────────────────────────────────────────────────────────────────
out_pdf <- file.path(paper_fig, "honestdid_sensitivity_trad.pdf")
out_png <- file.path(paper_fig, "honestdid_sensitivity_trad.png")

ggsave(out_pdf, p, width = 6.5, height = 4.5, device = cairo_pdf)
ggsave(out_png, p, width = 6.5, height = 4.5, dpi = 300)

cat("\nFigure saved:\n  ", out_pdf, "\n  ", out_png, "\n")
