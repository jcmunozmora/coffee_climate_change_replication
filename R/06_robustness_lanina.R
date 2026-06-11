#-------------------------------------------------------#
# Coffee & climate change replication
# 06_robustness_lanina.R
# Robustness: farms affected vs unaffected by the 2010-2011 "La Nina" rainfall
# shock (IDEAM). Reproduces Table `robustness_lanina_all`
# (Traditional cols 1-2, Technified cols 3-4; within each: Affected, Not).
# Treatment = credit OR business-management visits (cred_ges), as in the 2024 scripts.
#-------------------------------------------------------#

source("R/01_setup.R")

raw       <- read_panel_raw()
panel_reg <- prep_controls(raw)
num_vis   <- compute_num_vis(raw, "num_cred_ges")

# La Nina rainfall classification (joins by cod_finca) ------------------------
# The original script does NOT de-duplicate the IDEAM table, so the ~825 farms
# that appear twice are double-counted in the join. We keep that behaviour to
# match the published table exactly (relationship = "many-to-many").
nina <- readRDS(glue("{data_aux}/coffee_farms_la_nina_effects.rds")) %>%
  dplyr::select(cod_finca, alter_preci, categoria)

make_sample <- function(group_fun, treat_var) {
  panel_reg %>% group_fun() %>%
    build_treatment_het(num_vis, treat_var) %>%
    left_join(nina, by = "cod_finca", relationship = "many-to-many")
}
# Traditional uses credit OR management; technified uses credit advisory only
sample_trad <- make_sample(is_traditional, "tiene_cred_ges")
sample_tec  <- make_sample(is_technified,  "tiene_asesoria_cred")

# Affected = any major rainfall alteration (categoria != "Normal") ------------
split_run <- function(s) {
  list(affected = s %>% dplyr::filter(categoria != "Normal"),
       normal   = s %>% dplyr::filter(categoria == "Normal"))
}
sub_trad <- split_run(sample_trad)
sub_tec  <- split_run(sample_tec)

cols <- list(
  `Trad Yes` = sub_trad$affected, `Trad No` = sub_trad$normal,
  `Tec Yes`  = sub_tec$affected,  `Tec No`  = sub_tec$normal)

models <- lapply(cols, did_fe3)
saveRDS(lapply(models, broom::tidy), glue("{data}/robustness_lanina.rds"))

etable(models,
       keep = "%^int$",
       dict = c(int = "Treatment*Period"),
       extralines = list(
         "Mean of dep. var." = sapply(cols, mean_forest),
         "Farm mean size"    = sapply(cols, mean_size),
         "Affected by La Nina" = c("Yes", "No", "Yes", "No")),
       tex = TRUE, replace = TRUE,
       file = glue("{out_tab}/robustness_lanina_all.tex"),
       title = paste("Robustness check. Coffee farms affected by La Nina",
                     "rainfall shocks during 2010-2011."))

cat("\n== La Nina ATT (Treatment*Period) ==\n")
cat(sprintf("Traditional:  Affected=%.2f  Normal=%.2f  (paper: -13.00, -2.21)\n",
            coef(models$`Trad Yes`)["int"], coef(models$`Trad No`)["int"]))
cat(sprintf("Technified:   Affected=%.2f  Normal=%.2f  (paper: -3.67, -8.99)\n",
            coef(models$`Tec Yes`)["int"],  coef(models$`Tec No`)["int"]))
