#-------------------------------------------------------#
# Coffee & climate change replication
# 04_seed_change.R
# Mechanism: do credit/management visits push farms to adopt improved seed
# varieties? Reproduces Table `mechanism_seed_change_all`
# (Traditional ext/int = cols 1-2, Technified ext/int = cols 3-4).
#
# change_seed = 1 for farms with NO improved varieties before 2010 that DO have
# improved varieties afterwards. Extensive margin uses the visit dummy
# (tiene_cred_ges); intensive margin uses the visit count (num_cred_ges).
# Specification: village-year FE only, clustered at the village level.
#-------------------------------------------------------#

source("R/01_setup.R")

raw     <- read_panel_raw()
num_vis <- compute_num_vis(raw, "num_cred_ges")

# Panel prep, keeping the ORIGINAL seed shares (prc_*_ori) needed to define the
# seed-change indicator, alongside the trend-interacted controls.
mean_controls <- raw %>%
  dplyr::filter(year >= 2005 & year <= 2010) %>%
  dplyr::select(cod_finca, all_of(trend_controls)) %>%
  group_by(cod_finca) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop")

panel_reg <- raw %>%
  dplyr::rename(total_mts_ori = total_mts,
                prc_castillo_ori = prc_castillo, prc_colombia_ori = prc_colombia,
                prc_tabi_ori = prc_tabi, prc_tipica_ori = prc_tipica,
                prc_caturra_ori = prc_caturra) %>%
  dplyr::select(-c(prc_cafe, ndensidad, edad, edad2)) %>%
  left_join(mean_controls, by = "cod_finca") %>%
  dplyr::filter(year >= 2005) %>%
  mutate(time_trend = year - 2004) %>%
  mutate(across(all_of(trend_controls), ~ .x * time_trend))

# Improved-variety status before and after the cutoff ------------------------
improved <- function(d, pre) {
  d %>%
    dplyr::filter(if (pre) year < year_corte else year >= year_corte) %>%
    group_by(cod_finca) %>%
    summarise(keep = sum(prc_castillo_ori + prc_tabi_ori + prc_colombia_ori,
                         na.rm = TRUE), .groups = "drop")
}
no_improved_pre <- improved(panel_reg, TRUE)  %>% dplyr::filter(keep == 0) %>% pull(cod_finca)
has_improved_post <- improved(panel_reg, FALSE) %>% dplyr::filter(keep > 0) %>% pull(cod_finca)

panel_reg <- panel_reg %>%
  mutate(seed  = as.integer(cod_finca %in% no_improved_pre),
         seed2 = as.integer(cod_finca %in% has_improved_post),
         change_seed = seed * seed2)

# Regressions -----------------------------------------------------------------
seed_controls <- controls[controls != "total_mts"]   # no farm-size control here
seed_fml <- function(rhs) as.formula(paste0(
  "change_seed ~ ", rhs, " + ", paste(seed_controls, collapse = " + "),
  " | vereda_year"))

fit_seed <- function(d) {
  d <- d %>% dplyr::filter(year >= 2005)
  list(ext = feols(seed_fml("tiene_cred_ges"), data = d, cluster = ~ cod_vereda),
       int = feols(seed_fml("num_cred_ges"),   data = d, cluster = ~ cod_vereda))
}

# Traditional: no treatment filtering; technified: baseline contamination
# filters applied (as in the original 03f / 03d scripts, respectively).
m_trad <- fit_seed(is_traditional(panel_reg))
m_tec  <- fit_seed(build_treatment(is_technified(panel_reg), num_vis))

models <- list(`Trad Ext` = m_trad$ext, `Trad Int` = m_trad$int,
               `Tec Ext`  = m_tec$ext,  `Tec Int`  = m_tec$int)
saveRDS(lapply(models, broom::tidy), glue("{data}/mechanism_seed_change.rds"))

etable(models,
       keep = "%tiene_cred_ges|num_cred_ges",
       dict = c(tiene_cred_ges = "Credit visits (Ext.)",
                num_cred_ges = "Credit visits (Int.)"),
       tex = TRUE, replace = TRUE,
       file = glue("{out_tab}/mechanism_seed_change_all.tex"),
       title = paste("The link between credit and business-management visits",
                     "and the adoption of improved seed varieties."))

cat("\n== Seed change: extensive-margin coefficient (visit dummy) ==\n")
cat(sprintf("Traditional ext = %.2f  (paper: 0.19)\n", coef(m_trad$ext)["tiene_cred_ges"]))
cat(sprintf("Technified  ext = %.2f  (paper: 0.16)\n", coef(m_tec$ext)["tiene_cred_ges"]))
