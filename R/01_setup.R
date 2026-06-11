#-------------------------------------------------------#
# Coffee & climate change replication
# 01_setup.R  --  shared paths, options and helpers
# Source this at the top of every analysis script.
#-------------------------------------------------------#

# Packages --------------------------------------------------------------------
# Analysis runs on fixest (feols). The original manuscript used lfe (felm);
# point estimates and clustered SEs are identical for these specifications.
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, glue, fixest, broom)

options(scipen = 999)

# Paths (relative to repo root) -----------------------------------------------
data      <- "data"          # anonymized analysis data
data_aux  <- "data/aux"      # anonymized auxiliary datasets
out_tab   <- "output/tables"
out_fig   <- "output/figures"

# Global analysis constants ---------------------------------------------------
year_corte <- 2010           # treatment cutoff

# Control set used across baseline specifications -----------------------------
controls <- c("total_mts", "prc_cafe", "ndensidad", "edad", "edad2",
              "prc_castillo", "prc_colombia", "prc_tabi", "prc_tipica",
              "sd_temp", "sd_rain")
# The 9 controls interacted with the time trend (everything but the SD weather)
trend_controls <- controls[1:9]

#-------------------------------------------------------#
# read_panel_raw(): read the anonymized panel and apply the
# sample restriction shared by every script (small producers
# <5ha), plus the treatment dummies and edad^2.
# Returns the FULL-year panel (needed to flag pre-2010 visits).
#-------------------------------------------------------#
read_panel_raw <- function(size = c("small", "med", "all")) {
  size <- match.arg(size)
  d <- readRDS(glue("{data}/panel_finca_regresiones_anon.rds")) %>%
    dplyr::select(-starts_with("area"))
  d <- switch(size,
              small = dplyr::filter(d, total_mts < 50),    # <5 ha (baseline)
              med   = dplyr::filter(d, total_mts >= 50),   # medium/large (placebo)
              all   = d)
  d %>%
    mutate(tiene_cred_ges = ifelse(tiene_asesoria_cred == 1 | tiene_gest_emp == 1, 1, 0),
           num_cred_ges   = num_asesoria_cred + ges_emp,
           edad2          = edad * edad)
}

#-------------------------------------------------------#
# compute_num_vis(): farms whose FIRST credit/management visit
# happened before 2010 (to be dropped from the treated pool).
# `var` is the count column that defines a "visit".
#-------------------------------------------------------#
compute_num_vis <- function(panel_finca, var = "num_cred_ges") {
  panel_finca %>%
    group_by(cod_finca, year) %>%
    summarise(n_vis = sum(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    group_by(cod_finca) %>%
    mutate(min_year = ifelse(n_vis > 0, year, NA)) %>%
    drop_na(min_year) %>%
    dplyr::filter(min_year == min(min_year)) %>%
    ungroup() %>%
    dplyr::filter(min_year < 2010)
}

#-------------------------------------------------------#
# prep_controls(): interact the pre-2010 mean of each control
# with a linear time trend (2005->1 ... 2014->10) and keep
# years >= 2005. Renames the raw farm size to total_mts_ori.
#-------------------------------------------------------#
prep_controls <- function(panel_finca) {
  mean_controls <- panel_finca %>%
    dplyr::filter(year >= 2005 & year <= 2010) %>%
    dplyr::select(cod_finca, all_of(trend_controls)) %>%
    group_by(cod_finca) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop")

  panel_finca %>%
    dplyr::rename(total_mts_ori = total_mts) %>%
    dplyr::select(-all_of(trend_controls[-1])) %>%   # drop all but total_mts
    left_join(mean_controls, by = "cod_finca") %>%
    dplyr::filter(year >= 2005) %>%
    mutate(time_trend = year - 2004) %>%             # 2005 -> 1, ..., 2014 -> 10
    mutate(across(all_of(trend_controls), ~ .x * time_trend))
}

#-------------------------------------------------------#
# load_panel(): the baseline composition (cred+management
# treatment). Returns the prepared panel, the pre-2010 visit
# list, and the raw full-year panel.
#-------------------------------------------------------#
load_panel <- function() {
  raw <- read_panel_raw()
  list(panel_reg   = prep_controls(raw),
       num_vis     = compute_num_vis(raw, "num_cred_ges"),
       panel_finca = raw)
}

#-------------------------------------------------------#
# build_treatment(): treatment/period/interaction plus the
# control-contamination filters used by the baseline DiD.
#-------------------------------------------------------#
build_treatment <- function(d, num_vis, treat_var = "tiene_cred_ges") {
  d %>%
    mutate(period    = ifelse(year >= year_corte, 1, 0),
           treatment = ifelse(.data[[treat_var]] == 1, 1, 0),
           int       = treatment * period) %>%
    dplyr::filter(!(treatment == 0 & tiene_asesoria_cred == 1)) %>%
    dplyr::filter(!(treatment == 0 & tiene_gest_emp == 1)) %>%
    dplyr::filter(!(treatment == 0 & tiene_cred_ges == 1)) %>%
    dplyr::filter(!(cod_finca %in% num_vis$cod_finca))
}

#-------------------------------------------------------#
# build_treatment_het(): treatment + the 3 baseline
# contamination filters, plus the edad2 = edad^2 re-derivation
# on the already-interacted edad (as the 2024 heterogeneity /
# robustness scripts do). The treatment *variable* differs by
# crop group in those scripts: traditional uses credit OR
# management visits (tiene_cred_ges); technified/young uses
# credit advisory only (tiene_asesoria_cred). The contamination
# filters and the pre-2010 visit list are cred_ges-based in both.
#-------------------------------------------------------#
build_treatment_het <- function(d, num_vis, treat_var = "tiene_cred_ges") {
  d %>%
    mutate(period    = ifelse(year >= year_corte, 1, 0),
           treatment = ifelse(.data[[treat_var]] == 1, 1, 0),
           int       = treatment * period,
           edad2     = edad * edad) %>%
    dplyr::filter(!(treatment == 0 & tiene_asesoria_cred == 1)) %>%
    dplyr::filter(!(treatment == 0 & tiene_gest_emp == 1)) %>%
    dplyr::filter(!(treatment == 0 & tiene_cred_ges == 1)) %>%
    dplyr::filter(!(cod_finca %in% num_vis$cod_finca))
}

# Group masks -----------------------------------------------------------------
is_traditional <- function(d) dplyr::filter(d, (trad_young > 0) | (trad_trad > 0))
is_technified  <- function(d) dplyr::filter(d,
  trad_trad == 0 & trad_young == 0 & old_young == 0 & old_old == 0)

# Formula + estimation --------------------------------------------------------
baseline_formula <- function(fe) {
  as.formula(paste0("forest_year ~ int + ", paste(controls, collapse = " + "),
                    " | ", fe))
}

# Standard farm + village-year DiD, clustered at the village level
did_fe3 <- function(d) {
  feols(baseline_formula("cod_finca + vereda_year"), data = d, cluster = ~ cod_vereda)
}

# Means for table footers -----------------------------------------------------
mean_forest <- function(d) {
  d %>% dplyr::filter(year >= 2005) %>%
    drop_na(forest_year, edad, sd_temp, sd_rain) %>%
    summarise(m = mean(forest_year)) %>% pull(m) %>% round(2)
}
mean_size <- function(d) {
  d %>% dplyr::filter(total_mts_ori > 0 & year >= 2005) %>%
    drop_na(forest_year, edad, sd_temp, sd_rain) %>%
    summarise(m = mean(total_mts_ori, na.rm = TRUE)) %>% pull(m) %>% round(2)
}
