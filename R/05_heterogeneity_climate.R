#-------------------------------------------------------#
# Coffee & climate change replication
# 05_heterogeneity_climate.R
# Heterogeneity by farms' vulnerability to climate change (IDEAM index).
# Reproduces Table `heterogeneity_vul_clim_all` (Traditional cols 1-2,
# Technified cols 3-4; within each: Low/Medium then High/Very high).
# Treatment = credit OR business-management visits (cred_ges), as in the 2024 scripts.
#-------------------------------------------------------#

source("R/01_setup.R")

raw       <- read_panel_raw()
panel_reg <- prep_controls(raw)
num_vis   <- compute_num_vis(raw, "num_cred_ges")

# Climate-vulnerability classification (joins by cod_finca) -------------------
# The original script does not de-duplicate the IDEAM table; kept as-is (the few
# repeated farms are double-counted) to match the published table.
ind_clim <- readRDS(glue("{data_aux}/coffee_farms_vulnerability_index.rds")) %>%
  dplyr::select(cod_finca, vulnerability)

# Build the credit-treatment sample for a crop group, attach vulnerability ----
make_sample <- function(group_fun, treat_var) {
  panel_reg %>% group_fun() %>%
    build_treatment_het(num_vis, treat_var) %>%
    left_join(ind_clim, by = "cod_finca", relationship = "many-to-many")
}
# Traditional uses credit OR management; technified uses credit advisory only
sample_trad <- make_sample(is_traditional, "tiene_cred_ges")
sample_tec  <- make_sample(is_technified,  "tiene_asesoria_cred")

# Vulnerability bins ----------------------------------------------------------
high <- c("Alta", "Muy Alta")          # High or Very high
low  <- c("Baja", "Media")             # Low or Medium

split_run <- function(s) {
  list(low  = s %>% dplyr::filter(vulnerability %in% low),
       high = s %>% dplyr::filter(vulnerability %in% high))
}
sub_trad <- split_run(sample_trad)
sub_tec  <- split_run(sample_tec)

# Column order matches the published table: Low/Medium, High/Very high --------
cols <- list(
  `Trad Low`  = sub_trad$low,  `Trad High` = sub_trad$high,
  `Tec Low`   = sub_tec$low,   `Tec High`  = sub_tec$high)

models <- lapply(cols, did_fe3)
saveRDS(lapply(models, broom::tidy),
        glue("{data}/het_vul_clim.rds"))

# Table -----------------------------------------------------------------------
degree <- c("Low or Medium", "High or Very high",
            "Low or Medium", "High or Very high")
etable(models,
       keep = "%^int$",
       dict = c(int = "Treatment*Period"),
       extralines = list(
         "Mean of dep. var." = sapply(cols, mean_forest),
         "Farm mean size"    = sapply(cols, mean_size),
         "Vulnerability degree" = degree),
       tex = TRUE, replace = TRUE,
       file = glue("{out_tab}/heterogeneity_vul_clim_all.tex"),
       title = paste("Heterogeneity analysis. The environmental effect of",
                     "conditional credit visits on tree cover based on farms'",
                     "vulnerability to climate change."))

cat("\n== Heterogeneity ATT (Treatment*Period) ==\n")
cat(sprintf("Traditional:  Low/Med=%.2f  High/VHigh=%.2f  (paper: -15.01, -7.99)\n",
            coef(models$`Trad Low`)["int"], coef(models$`Trad High`)["int"]))
cat(sprintf("Technified:   Low/Med=%.2f  High/VHigh=%.2f  (paper: -2.69, -5.30)\n",
            coef(models$`Tec Low`)["int"],  coef(models$`Tec High`)["int"]))
