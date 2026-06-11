#-------------------------------------------------------#
# Coffee & climate change replication
# 07b_staggered_tec.R
# Technified-group staggered DiD (Callaway & Sant'Anna 2021) ONLY.
# Heavy run (~millions of obs + bootstrap), meant for a long terminal job.
#
# Progress tracking:
#   - every stage logs a timestamp, seconds elapsed since the previous stage,
#     total elapsed, and current R memory use;
#   - the problem size (cohorts x periods, number of ATT(g,t) cells) is printed
#     before the heavy step;
#   - PHASE 1 runs att_gt WITHOUT the bootstrap first -> fast point estimates +
#     event study are saved within minutes as a checkpoint;
#   - PHASE 2 then runs the bootstrap for simultaneous bands.
#   Each aggregation is saved as soon as it is computed.
#
# Run from the repo root:
#   nohup Rscript R/07b_staggered_tec.R > output/staggered_tec_log.txt 2>&1 &
#   tail -f output/staggered_tec_log.txt
#-------------------------------------------------------#

source("R/01_setup.R")
pacman::p_load(did)
set.seed(123)

# --- progress logger ---------------------------------------------------------
.t0   <- Sys.time()
.tlast <- .t0
log_step <- function(msg) {
  now <- Sys.time()
  since <- as.numeric(difftime(now, .tlast, units = "secs"))
  total <- as.numeric(difftime(now, .t0, units = "secs"))
  mem <- sum(gc()[, 2])          # Mb currently used by R
  cat(sprintf("[%s] +%6.1fs | total %6.1fs | mem %5.0f Mb | %s\n",
              format(now, "%H:%M:%S"), since, total, mem, msg))
  flush.console()
  .tlast <<- now
}

log_step("START - loading panel")
raw       <- read_panel_raw()
num_vis   <- compute_num_vis(raw, "num_cred_ges")
# Technified Callaway uses time-trend-INTERACTED controls (mean pre-2010 x
# trend), matching the original 04f script (unlike the traditional 04e, which
# uses raw controls). prep_controls() produces exactly those interacted columns.
panel_reg <- prep_controls(raw)
rm(raw); gc()
log_step("panel loaded and controls interacted")

tec <- panel_reg %>%
  mutate(year_cred = ifelse(num_cred_ges > 0, year, NA)) %>%
  group_by(cod_finca) %>%
  mutate(treat = ifelse(tiene_cred_ges == 1,
                        suppressWarnings(min(year_cred, na.rm = TRUE)), 0),
         cod_vereda = max(cod_vereda, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(treat = ifelse(is.infinite(treat), NA, treat)) %>%
  drop_na(treat) %>%
  is_technified() %>%
  dplyr::filter(treat >= 2010 | treat == 0) %>%
  dplyr::filter(!(treat == 0 & tiene_asesoria_cred == 1)) %>%
  dplyr::filter(!(treat == 0 & tiene_gest_emp == 1)) %>%
  dplyr::filter(!(treat == 0 & tiene_cred_ges == 1)) %>%
  dplyr::filter(!(cod_finca %in% num_vis$cod_finca)) %>%
  drop_na(sd_temp, sd_rain, total_mts, prc_tipica, edad) %>%
  dplyr::select(cod_finca, cod_vereda, year, treat, forest_year,
                total_mts, prc_cafe, ndensidad, edad, edad2, sd_temp, sd_rain)
rm(raw); gc()
log_step(sprintf("sample built: %d obs, %d farms",
                 nrow(tec), dplyr::n_distinct(tec$cod_finca)))

# --- problem-size diagnostics (so you can gauge the workload) ----------------
cohorts <- sort(unique(tec$treat[tec$treat > 0]))
periods <- sort(unique(tec$year))
cat("  cohorts (first-visit year):", paste(cohorts, collapse = ", "), "\n")
cat("  never-treated farms:",
    dplyr::n_distinct(tec$cod_finca[tec$treat == 0]), "\n")
cat("  ATT(g,t) cells to estimate ~",
    length(cohorts) * length(periods), "\n")
print(table(treat = tec$treat, year = tec$year))
log_step("diagnostics printed")

run_aggs <- function(att, tag) {
  for (ty in c("simple", "dynamic", "group", "calendar")) {
    a <- aggte(att, type = ty)
    saveRDS(a, glue("{data}/staggered_agg_tec_{tag}_{ty}.rds"))
    if (ty == "simple")
      cat(sprintf("    >> [%s] simple overall ATT = %.2f (se %.2f)\n",
                  tag, a$overall.att, a$overall.se))
    log_step(sprintf("[%s] %s aggregation saved", tag, ty))
  }
}

# --- PHASE 1: no bootstrap (fast checkpoint) ---------------------------------
log_step("PHASE 1: att_gt WITHOUT bootstrap (fast point estimates)")
att_fast <- did::att_gt(
  yname = "forest_year", tname = "year", idname = "cod_finca", gname = "treat",
  control_group = "nevertreated", bstrap = FALSE, clustervars = "cod_vereda",
  allow_unbalanced_panel = TRUE,
  xformla = ~ total_mts + prc_cafe + ndensidad + edad + edad2 + sd_temp + sd_rain,
  data = tec)
saveRDS(att_fast, glue("{data}/staggered_att_gt_tec_nobstrap.rds"))
log_step("PHASE 1 att_gt done")
run_aggs(att_fast, "nobstrap")
rm(att_fast); gc()

# --- PHASE 2: with bootstrap (simultaneous bands) ----------------------------
log_step("PHASE 2: att_gt WITH bootstrap (this is the slow part)")
att <- did::att_gt(
  yname = "forest_year", tname = "year", idname = "cod_finca", gname = "treat",
  control_group = "nevertreated", bstrap = TRUE, clustervars = "cod_vereda",
  allow_unbalanced_panel = TRUE,
  xformla = ~ total_mts + prc_cafe + ndensidad + edad + edad2 + sd_temp + sd_rain,
  data = tec)
saveRDS(att, glue("{data}/staggered_att_gt_tec.rds"))
log_step("PHASE 2 att_gt done")
run_aggs(att, "boot")
log_step("DONE - all results saved to data/staggered_*_tec*.rds")

#-------------------------------------------------------#
# Fallbacks if PHASE 2 runs out of memory / time:
#   - keep only PHASE 1 results (point estimates + event study), or
#   - base_period = "universal", or
#   - subsample never-treated controls:
#       keep <- tec %>% distinct(cod_finca, treat)
#       ctrl <- keep %>% filter(treat == 0) %>% slice_sample(prop = 0.25)
#       tec  <- tec %>% filter(treat > 0 | cod_finca %in% ctrl$cod_finca)
#-------------------------------------------------------#
