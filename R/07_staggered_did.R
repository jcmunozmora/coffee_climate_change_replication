#-------------------------------------------------------#
# Coffee & climate change replication
# 07_staggered_did.R
# Staggered / modern DiD (Callaway & Sant'Anna 2021) using the actual year of
# the first credit/management visit as the treatment cohort, never-treated
# farms as the comparison group. Reproduces Tables `did_staggered_*`.
#
# NOTE: the technified group has millions of observations; att_gt with the
# bootstrap is very heavy there. Set RUN_TEC = TRUE to run it (expect a long
# runtime / high memory). The traditional group runs in a couple of minutes.
#-------------------------------------------------------#

source("R/01_setup.R")
pacman::p_load(did)
set.seed(123)

RUN_TEC <- isTRUE(as.logical(Sys.getenv("RUN_TEC", "FALSE")))

raw     <- read_panel_raw()
num_vis <- compute_num_vis(raw, "num_cred_ges")

# Build the staggered sample: treat = first visit year (0 = never treated) ----
make_staggered <- function(group_fun) {
  raw %>%
    dplyr::filter(year >= 2005) %>%
    mutate(year_cred = ifelse(num_cred_ges > 0, year, NA)) %>%
    group_by(cod_finca) %>%
    mutate(treat = ifelse(tiene_cred_ges == 1, suppressWarnings(min(year_cred, na.rm = TRUE)), 0),
           cod_vereda = max(cod_vereda, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(treat = ifelse(is.infinite(treat), NA, treat)) %>%
    drop_na(treat) %>%
    group_fun() %>%
    dplyr::filter(treat >= 2010 | treat == 0) %>%
    dplyr::filter(!(treat == 0 & tiene_asesoria_cred == 1)) %>%
    dplyr::filter(!(treat == 0 & tiene_gest_emp == 1)) %>%
    dplyr::filter(!(treat == 0 & tiene_cred_ges == 1)) %>%
    dplyr::filter(!(cod_finca %in% num_vis$cod_finca)) %>%
    drop_na(sd_temp, sd_rain, total_mts, prc_tipica, edad)
}

run_cs <- function(d, tag) {
  att <- did::att_gt(
    yname = "forest_year", tname = "year", idname = "cod_finca", gname = "treat",
    control_group = "nevertreated", bstrap = TRUE, clustervars = "cod_vereda",
    allow_unbalanced_panel = TRUE,
    xformla = ~ total_mts + prc_cafe + ndensidad + edad + edad2 + sd_temp + sd_rain,
    data = d)
  saveRDS(att, glue("{data}/staggered_att_gt_{tag}.rds"))
  aggs <- list(simple   = aggte(att, type = "simple"),
               dynamic  = aggte(att, type = "dynamic"),
               group    = aggte(att, type = "group"),
               calendar = aggte(att, type = "calendar"))
  saveRDS(lapply(aggs, function(a) a[c("overall.att", "overall.se",
                                       "egt", "att.egt", "se.egt")]),
          glue("{data}/staggered_agg_{tag}.rds"))
  cat(sprintf("[%s] simple overall ATT = %.2f (se %.2f)\n",
              tag, aggs$simple$overall.att, aggs$simple$overall.se))
  aggs
}

cat("== Staggered DiD (Callaway-Sant'Anna) ==\n")
trad <- make_staggered(is_traditional)
cat(sprintf("Traditional sample: %d obs, %d farms\n",
            nrow(trad), dplyr::n_distinct(trad$cod_finca)))
run_cs(trad, "trad")

if (RUN_TEC) {
  tec <- make_staggered(is_technified)
  cat(sprintf("Technified sample: %d obs, %d farms\n",
              nrow(tec), dplyr::n_distinct(tec$cod_finca)))
  run_cs(tec, "tec")
} else {
  cat("Technified group skipped (set RUN_TEC=TRUE to run; very heavy).\n")
}
