#-------------------------------------------------------#
# Coffee & climate change replication -- master script
# Runs the full (currently ported) analysis from the anonymized panel.
# Run from the repo root:  Rscript run_all.R
#-------------------------------------------------------#

# NOTE: 00_anonymize_panel.R is NOT run here. It requires the private raw
# panel (data_raw/) and is executed once by the data owner to produce
# data/panel_finca_regresiones_anon.rds, which is what this pipeline consumes.

scripts <- c(
  "R/02_baseline_did.R",          # Table:  did_baseline_finca_all       [DONE]
  "R/03_parallel_trends.R",       # Figure: parallel_trends_all_groups   [DONE]
  "R/04_seed_change.R",           # Table:  mechanism_seed_change_all    [DONE]
  "R/05_heterogeneity_climate.R", # Table:  heterogeneity_vul_clim_all   [DONE]
  "R/06_robustness_lanina.R",     # Table:  robustness_lanina_all        [DONE]
  "R/07_staggered_did.R",         # Tables: did_staggered_* (Callaway)    [DONE-trad]
  "R/08_robustness_extension.R",  # Table:  robustness_extension_all      [DONE]
  "R/09_placebos.R",              # Table:  did_placebo_finca_tamano_all  [DONE]
  "R/11_robustness_finagro.R",    # Table:  lm_deforestacion_num_credito  [DONE]
  "R/12_robustness_prodcycle.R",  # Table:  robustness_prod_cycle_all     [DONE]
  "R/13_diff_means.R"             # Table:  dif_means_treated_control_pre  [DONE]
  # R/10_frontier_robustness.R    # Goodman-Bacon + HonestDiD (heavy; run separately)
  # Not reproducible from the shared data:
  #   treated/control maps        -> need farm coordinates (privacy-restricted)
)

for (s in scripts) {
  message("\n==== Running ", s, " ====")
  source(s, echo = FALSE)
}
message("\nDone. Outputs in output/tables and output/figures.")
