#-------------------------------------------------------#
# Coffee & climate change replication
# 14_mean_reversion.R
# Referee bundle, item #1 (the binding objection): is the baseline DiD a
# treatment effect or mechanical mean reversion? Treated farms enter with
# +46-63% more tree cover than controls, so high-cover units regressing toward
# the mean could produce a negative DiD coefficient with no program effect.
#
# Two diagnostics, per crop group:
#   (A) Stratify the baseline DiD by pre-2010 (2005-2009) tree-cover quartile.
#       - reversion  => effect loads on the TOP quartile (most cover to lose),
#                       ~0 in low quartiles.
#       - treatment  => effect present across quartiles (incl. middle/low).
#   (B) Add baseline-cover-quartile x period interactions to the baseline.
#       If the Treatment*Period coefficient survives, differential reversion by
#       initial cover level is not what is driving the result.
#-------------------------------------------------------#

source("R/01_setup.R")

p <- load_panel()
panel_reg <- p$panel_reg
num_vis   <- p$num_vis

# Pre-2010 mean tree cover per farm (the level that would "revert") -----------
base_cover <- panel_reg %>%
  dplyr::filter(year >= 2005 & year <= 2009) %>%
  group_by(cod_finca) %>%
  summarise(base_forest = mean(forest_year, na.rm = TRUE), .groups = "drop")

prep_group <- function(group_filter, treat_var) {
  d <- panel_reg %>% group_filter() %>%
    build_treatment(num_vis, treat_var = treat_var) %>%
    left_join(base_cover, by = "cod_finca") %>%
    drop_na(base_forest)
  # within-group quartiles of baseline cover
  qs <- quantile(d$base_forest[!duplicated(d$cod_finca)],
                 probs = c(.25, .5, .75), na.rm = TRUE)
  d %>% mutate(qbin = cut(base_forest, breaks = c(-Inf, qs, Inf),
                          labels = c("Q1","Q2","Q3","Q4")))
}

# (A) stratified baseline DiD -------------------------------------------------
strat_did <- function(d) {
  lapply(levels(d$qbin), function(q) {
    dd <- dplyr::filter(d, qbin == q)
    m  <- tryCatch(did_fe3(dd), error = function(e) NULL)
    if (is.null(m)) return(c(q = q, beta = NA, se = NA, n = nrow(dd),
                             nfarm = dplyr::n_distinct(dd$cod_finca),
                             base = NA))
    ct <- broom::tidy(m); r <- ct[ct$term == "int", ]
    c(q = q, beta = round(r$estimate, 2), se = round(r$std.error, 2),
      n = nrow(dd), nfarm = dplyr::n_distinct(dd$cod_finca),
      base = round(mean(dd$base_forest), 0))
  }) %>% do.call(rbind, .) %>% as.data.frame()
}

# (B) baseline + cover-quartile x period control ------------------------------
control_spec <- function(d) {
  d <- d %>% mutate(period = as.integer(year >= year_corte))
  f <- as.formula(paste0(
    "forest_year ~ int + i(qbin, period, 'Q1') + ",
    paste(controls, collapse = " + "), " | cod_finca + vereda_year"))
  m <- feols(f, data = d, cluster = ~ cod_vereda)
  ct <- broom::tidy(m); r <- ct[ct$term == "int", ]
  c(beta = round(r$estimate, 2), se = round(r$std.error, 2),
    p = round(r$p.value, 4))
}

run_group <- function(tag, group_filter, treat_var) {
  cat(sprintf("\n===== %s =====\n", tag))
  d <- prep_group(group_filter, treat_var)
  cat("Baseline-cover quartile means (000 m2) and stratified Treatment*Period:\n")
  A <- strat_did(d)
  print(A)
  cat("\nBaseline with cover-quartile x period control (Treatment*Period):\n")
  B <- control_spec(d)
  print(B)
  list(strat = A, control = B)
}

res_trad <- run_group("TRADITIONAL (treat = cred_ges)", is_traditional, "tiene_cred_ges")
res_tec  <- run_group("TECHNIFIED (treat = asesoria_cred)", is_technified, "tiene_asesoria_cred")

saveRDS(list(traditional = res_trad, technified = res_tec),
        glue("{data}/mean_reversion.rds"))
cat("\nSaved -> data/mean_reversion.rds\n")
