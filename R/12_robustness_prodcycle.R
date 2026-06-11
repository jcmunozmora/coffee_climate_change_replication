#-------------------------------------------------------#
# Coffee & climate change replication
# 12_robustness_prodcycle.R
# Robustness by stage of the coffee productive cycle. Farms are split by whether
# they have at least one plot that reached renewal age (>=9 years) in 2009:
#   "End"       = has a renewal-age plot (renov == 1)
#   "Beginning" = does not (renov == 0)
# Reproduces Table `robustness_prod_cycle_all` (Traditional Beginning/End =
# cols 1-2, Technified = cols 3-4). Treatment = credit OR management visits.
#-------------------------------------------------------#

source("R/01_setup.R")

# Plot-level panel: flag farms with a renewal-age plot in 2009 ----------------
panel_lote <- readRDS(glue("{data_aux}/coffee_plots_forest_cover_full.rds"))
renov_lotes <- panel_lote %>%
  mutate(edad_renov = ifelse(year == year_corte - 1 & edad >= 9, 1, 0)) %>%
  group_by(id_lote) %>%
  summarise(renov = as.integer(max(edad_renov) == 1),
            cod_vereda = dplyr::first(cod_vereda), .groups = "drop") %>%
  dplyr::filter(renov == 1) %>%
  dplyr::select(id_lote, cod_vereda)
rm(panel_lote); gc()

# Map renewal-age plots to their farm via the plot<->farm bridge --------------
codigos <- readRDS(glue("{data_aux}/codigos_finca_lote_mpio_vereda.rds")) %>%
  dplyr::select(cod_finca, id_lote, cod_vereda)
renov_fincas <- renov_lotes %>%
  left_join(codigos, by = c("id_lote", "cod_vereda")) %>%
  distinct(cod_finca, cod_vereda) %>%
  mutate(renov = 1)

# Attach the renovation flag to the analysis panel ----------------------------
raw       <- read_panel_raw()
num_vis   <- compute_num_vis(raw, "num_cred_ges")
panel_reg <- prep_controls(raw) %>%
  left_join(renov_fincas, by = c("cod_finca", "cod_vereda")) %>%
  mutate(renov = ifelse(is.na(renov), 0, renov))

# One column per crop group x cycle stage -------------------------------------
cell <- function(group_fun, stage) {
  group_fun(panel_reg) %>%
    dplyr::filter(renov == stage) %>%
    build_treatment_het(num_vis, "tiene_cred_ges")
}
cols <- list(
  `Trad Beginning` = cell(is_traditional, 0), `Trad End` = cell(is_traditional, 1),
  `Tec Beginning`  = cell(is_technified, 0),  `Tec End`  = cell(is_technified, 1))

models <- lapply(cols, did_fe3)
saveRDS(lapply(models, broom::tidy), glue("{data}/robustness_prodcycle.rds"))

etable(models, keep = "%^int$", dict = c(int = "Treatment*Period"),
       extralines = list(
         "Mean of dep. var." = sapply(cols, mean_forest),
         "Farm mean size"    = sapply(cols, mean_size),
         "Productive cycle"  = c("Beginning", "End", "Beginning", "End")),
       tex = TRUE, replace = TRUE,
       file = glue("{out_tab}/robustness_prod_cycle_all.tex"),
       title = "Robustness check. Effect by stage of the coffee productive cycle.")

cat("\n== Productive cycle: Treatment*Period ==\n")
cat(sprintf("Traditional:  Beginning=%.2f  End=%.2f  (paper: -13.35, -10.70)\n",
            coef(models$`Trad Beginning`)["int"], coef(models$`Trad End`)["int"]))
cat(sprintf("Technified:   Beginning=%.2f  End=%.2f  (paper: -3.88, -4.42)\n",
            coef(models$`Tec Beginning`)["int"],  coef(models$`Tec End`)["int"]))
