#-------------------------------------------------------#
# Coffee & climate change replication
# 08_robustness_extension.R
# Robustness: split farms by whether their municipality had an above- vs
# below-average number of FNC extension workers (2008). Reproduces Table
# `robustness_extension_all` (Traditional 1-2, Technified 3-4; within each:
# above-average then below-average).
#-------------------------------------------------------#

source("R/01_setup.R")

raw       <- read_panel_raw()
panel_reg <- prep_controls(raw)
num_vis   <- compute_num_vis(raw, "num_cred_ges")

# Extension workers per municipality (2008), split at the mean -----------------
ext <- readRDS(glue("{data_aux}/extensionistas_mpios_2006-2008.rds")) %>%
  dplyr::filter(year == 2008)
mean_ext <- mean(ext$extensionistas)
ext <- ext %>%
  group_by(cod_mpio) %>%
  summarise(extensionistas = sum(extensionistas), .groups = "drop") %>%
  mutate(d_ext = as.integer(extensionistas > mean_ext)) %>%
  dplyr::select(cod_mpio, d_ext)

make_sample <- function(group_fun, treat_var) {
  panel_reg %>% group_fun() %>%
    build_treatment_het(num_vis, treat_var) %>%
    left_join(ext, by = "cod_mpio")
}
sample_trad <- make_sample(is_traditional, "tiene_cred_ges")
sample_tec  <- make_sample(is_technified,  "tiene_cred_ges")

split_run <- function(s) {
  list(above = s %>% dplyr::filter(d_ext == 1),
       below = s %>% dplyr::filter(d_ext == 0))
}
sub_trad <- split_run(sample_trad)
sub_tec  <- split_run(sample_tec)

cols <- list(`Trad Above` = sub_trad$above, `Trad Below` = sub_trad$below,
             `Tec Above`  = sub_tec$above,  `Tec Below`  = sub_tec$below)

models <- lapply(cols, did_fe3)
saveRDS(lapply(models, broom::tidy), glue("{data}/robustness_extension.rds"))

etable(models, keep = "%^int$", dict = c(int = "Treatment*Period"),
       extralines = list(
         "Mean of dep. var." = sapply(cols, mean_forest),
         "Farm mean size"    = sapply(cols, mean_size),
         "Extension workers" = c("Above avg", "Below avg", "Above avg", "Below avg")),
       tex = TRUE, replace = TRUE,
       file = glue("{out_tab}/robustness_extension_all.tex"),
       title = paste("Robustness check. Coffee farms in municipalities with",
                     "above- vs below-average FNC extension workers."))

cat("\n== Extension robustness ATT (Treatment*Period) ==\n")
cat(sprintf("Traditional:  Above=%.2f  Below=%.2f\n",
            coef(models$`Trad Above`)["int"], coef(models$`Trad Below`)["int"]))
cat(sprintf("Technified:   Above=%.2f  Below=%.2f\n",
            coef(models$`Tec Above`)["int"],  coef(models$`Tec Below`)["int"]))
