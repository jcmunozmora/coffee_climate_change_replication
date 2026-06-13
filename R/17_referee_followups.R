#-------------------------------------------------------#
# Coffee & climate change replication
# 17_referee_followups.R
# Three manuscript-TODO follow-ups + the wild-bootstrap p-value:
#   (A) Spillover test: add the vereda-year share of treated farms to baseline.
#   (B) LATE (Wald): rescale the ITT by the first-stage (visit -> improved seed).
#   (C) MDE for the staggered (C-S) estimates, from saved aggregations.
#   (D) Wild cluster bootstrap p-value for the traditional baseline coefficient.
#-------------------------------------------------------#

source("R/01_setup.R")

L <- load_panel(); panel_reg <- L$panel_reg; num_vis <- L$num_vis

samp <- function(gf, tv) gf(panel_reg) %>% build_treatment(num_vis, tv)
trad <- samp(is_traditional, "tiene_cred_ges")
tec  <- samp(is_technified,  "tiene_asesoria_cred")

cat("\n===== (A) SPILLOVER: vereda-year share of treated farms =====\n")
add_share <- function(d) d %>%
  group_by(cod_vereda, year) %>% mutate(share_treat = mean(treatment, na.rm = TRUE)) %>%
  ungroup()
spill <- function(d, tag) {
  d <- add_share(d)
  f <- as.formula(paste0("forest_year ~ int + share_treat + ",
                         paste(controls, collapse = " + "), " | cod_finca + vereda_year"))
  m <- feols(f, data = d, cluster = ~ cod_vereda)
  r <- broom::tidy(m)
  cat(sprintf("  %-12s int=%.2f (p=%.3f) | share_treat=%.2f (p=%.3f)\n", tag,
              r$estimate[r$term=="int"], r$p.value[r$term=="int"],
              r$estimate[r$term=="share_treat"], r$p.value[r$term=="share_treat"]))
}
spill(trad, "Traditional"); spill(tec, "Technified")

cat("\n===== (B) LATE (Wald = ITT / first stage) =====\n")
# first stage (visit -> improved-seed adoption) from the seed-change objects
seed <- readRDS(glue("{data}/mechanism_seed_change.rds"))
fs_trad <- seed[["Trad Ext"]]$estimate[seed[["Trad Ext"]]$term == "tiene_cred_ges"]
fs_tec  <- seed[["Tec Ext"]]$estimate[seed[["Tec Ext"]]$term == "tiene_cred_ges"]
itt_trad <- -9.91; itt_tec <- -4.23   # baseline col3 (validated)
cat(sprintf("  Traditional: ITT=%.2f / FS=%.2f  => LATE=%.1f thousand m2\n",
            itt_trad, fs_trad, itt_trad / fs_trad))
cat(sprintf("  Technified:  ITT=%.2f / FS=%.2f  => LATE=%.1f thousand m2\n",
            itt_tec, fs_tec, itt_tec / fs_tec))

cat("\n===== (C) MDE for the staggered C-S estimates =====\n")
# MDE at 5% size / 80% power ~ 2.8 * SE
for (g in c("trad", "tec")) {
  f <- glue("{data}/staggered_agg_tec_boot_simple.rds")
  if (g == "trad") f <- glue("{data}/staggered_agg_simple_trad.rds")
  if (file.exists(f)) {
    a <- readRDS(f)
    se <- a$overall.se
    cat(sprintf("  %-5s C-S simple SE=%.1f  => MDE(80%% power)=%.1f thousand m2 (baseline: %s)\n",
                g, se, 2.8 * se, ifelse(g == "trad", "-9.91", "-4.23")))
  } else cat(sprintf("  %-5s aggregation file not found: %s\n", g, f))
}

cat("\n===== (D) Wild cluster bootstrap p (traditional baseline) =====\n")
if (requireNamespace("fwildclusterboot", quietly = TRUE)) {
  pacman::p_load(fwildclusterboot)
  set.seed(123)
  d <- trad %>% drop_na(forest_year, all_of(controls))
  repeat { n0 <- nrow(d)
    d <- d %>% group_by(cod_finca) %>% dplyr::filter(n() > 1) %>% ungroup()
    d <- d %>% group_by(vereda_year) %>% dplyr::filter(n() > 1) %>% ungroup()
    if (nrow(d) == n0) break }
  m <- feols(baseline_formula("cod_finca + vereda_year"), data = d, cluster = ~ cod_vereda)
  bt <- tryCatch(fwildclusterboot::boottest(m, param = "int", clustid = "cod_vereda",
                                            B = 9999, type = "webb"),
                 error = function(e) {cat("  boottest error:", conditionMessage(e), "\n"); NULL})
  if (!is.null(bt)) cat(sprintf("  wild bootstrap (Webb, B=9999) p = %.4f | 95%% CI [%.2f, %.2f]\n",
                                bt$p_val, bt$conf_int[1], bt$conf_int[2]))
}
cat("\nDONE 17_referee_followups\n")
