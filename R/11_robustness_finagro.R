#-------------------------------------------------------#
# Coffee & climate change replication
# 11_robustness_finagro.R
# Municipality-level link between granted coffee-renewal credits (FINAGRO) and
# total tree cover. Reproduces Table `lm_deforestacion_num_credito`
# (Traditional FE-only / +controls = cols 1-2, Technified = cols 3-4).
#-------------------------------------------------------#

source("R/01_setup.R")

# FINAGRO renewal credits per municipality-year (dpmp already anonymized to the
# municipality crosswalk) -----------------------------------------------------
cred_mpio <- readRDS(glue("{data_aux}/credito_2010_2014.rds")) %>%
  dplyr::rename(cod_mpio = dpmp, year = anio) %>%
  mutate(destino = str_to_lower(destino)) %>%
  dplyr::filter(grepl("caf", destino)) %>%
  dplyr::filter(grepl("renova", destino),
                !grepl("zoca", destino), !grepl("especial", destino)) %>%
  group_by(cod_mpio, year) %>%
  summarise(num_cred = sum(colocaciones_total),
            valor_cred = sum(colocaciones_valor), .groups = "drop")

raw     <- read_panel_raw()
num_vis <- compute_num_vis(raw, "num_cred_ges")

# Aggregate the baseline treatment sample to municipality-year ----------------
to_mpio <- function(group_fun) {
  group_fun(raw) %>%
    dplyr::filter(year >= 2005) %>%
    mutate(treatment = ifelse(tiene_cred_ges == 1, 1, 0)) %>%
    dplyr::filter(!(treatment == 0 & tiene_asesoria_cred == 1)) %>%
    dplyr::filter(!(treatment == 0 & tiene_gest_emp == 1)) %>%
    dplyr::filter(!(treatment == 0 & tiene_cred_ges == 1)) %>%
    dplyr::filter(!(cod_finca %in% num_vis$cod_finca)) %>%
    dplyr::select(cod_mpio, cod_vereda, cod_finca, year, forest_year, total_mts,
                  prc_cafe, ndensidad, edad, prc_castillo, prc_colombia, prc_tabi,
                  prc_tipica, sd_temp, sd_rain) %>%
    drop_na() %>%
    group_by(cod_mpio, year) %>%
    summarise(forest_sum = sum(forest_year, na.rm = TRUE),
              across(c(total_mts, prc_cafe, ndensidad, edad, prc_castillo,
                       prc_colombia, prc_tabi, prc_tipica, sd_temp, sd_rain),
                     ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(edad2 = edad * edad) %>%
    dplyr::filter(year >= 2010) %>%
    left_join(cred_mpio, by = c("cod_mpio", "year")) %>%
    mutate(num_cred = tidyr::replace_na(num_cred, 0),
           valor_cred = tidyr::replace_na(valor_cred, 0))
}

ctrl <- paste("total_mts + prc_cafe + ndensidad + edad + edad2 + prc_castillo +",
              "prc_colombia + prc_tabi + prc_tipica + sd_temp + sd_rain")
fit <- function(d) list(
  fe   = feols(forest_sum ~ num_cred | cod_mpio + year, data = d, cluster = ~ cod_mpio),
  ctrl = feols(as.formula(paste0("forest_sum ~ num_cred + ", ctrl,
                                 " | cod_mpio + year")), data = d, cluster = ~ cod_mpio))

m_trad <- fit(to_mpio(is_traditional))
m_tec  <- fit(to_mpio(is_technified))

models <- list(`Trad FE` = m_trad$fe, `Trad +ctrl` = m_trad$ctrl,
               `Tec FE` = m_tec$fe,  `Tec +ctrl` = m_tec$ctrl)
saveRDS(lapply(models, broom::tidy), glue("{data}/finagro_deforestation.rds"))
etable(models, keep = "%num_cred", dict = c(num_cred = "Number of granted credits"),
       tex = TRUE, replace = TRUE,
       file = glue("{out_tab}/lm_deforestacion_num_credito.tex"),
       title = "Granted coffee-renewal credits and municipal tree cover.")

cat("\n== Finagro: coef on num_cred (and N) ==\n")
cat(sprintf("Traditional: FE=%.2f  +ctrl=%.2f  obs=%d  (paper obs: 2,259)\n",
            coef(m_trad$fe)["num_cred"], coef(m_trad$ctrl)["num_cred"], m_trad$fe$nobs))
cat(sprintf("Technified:  FE=%.2f  +ctrl=%.2f  obs=%d  (paper obs: 2,861)\n",
            coef(m_tec$fe)["num_cred"], coef(m_tec$ctrl)["num_cred"], m_tec$fe$nobs))
