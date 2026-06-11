#-------------------------------------------------------#
# 15_wild_bootstrap.R
# Referee bundle: the traditional group has ~959 treated farms; with few treated
# clusters, cluster-robust asymptotics over-reject. Re-do inference on the
# baseline col3 Treatment*Period coefficient with the wild cluster bootstrap
# (Webb weights), clustered at the vereda level. boottest cannot handle the
# singleton FEs that feols silently drops, so we iteratively prune singleton
# cod_finca / vereda_year cells until the design matrix is stable, then bootstrap.
#-------------------------------------------------------#

source("R/01_setup.R")
pacman::p_load(fwildclusterboot)
set.seed(123); dqrng::dqset.seed(123)

p <- load_panel(); panel_reg <- p$panel_reg; num_vis <- p$num_vis

prune_singletons <- function(d) {
  repeat {
    n0 <- nrow(d)
    d <- d %>% group_by(cod_finca)   %>% dplyr::filter(n() > 1) %>% ungroup()
    d <- d %>% group_by(vereda_year) %>% dplyr::filter(n() > 1) %>% ungroup()
    if (nrow(d) == n0) break
  }
  d
}

boot_trad <- function() {
  d <- panel_reg %>% is_traditional() %>%
    build_treatment(num_vis, treat_var = "tiene_cred_ges") %>%
    drop_na(forest_year, all_of(controls)) %>%
    prune_singletons()
  m <- feols(baseline_formula("cod_finca + vereda_year"), data = d,
             cluster = ~ cod_vereda)
  asy <- broom::tidy(m); asy <- asy[asy$term == "int", ]
  cat(sprintf("[TRADITIONAL pruned] n=%d farms=%d  beta=%.2f asy.se=%.2f asy.p=%.4f\n",
              nrow(d), dplyr::n_distinct(d$cod_finca),
              asy$estimate, asy$std.error, asy$p.value))
  bt <- tryCatch(boottest(m, param = "int", clustid = "cod_vereda",
                          B = 9999, type = "webb"),
                 error = function(e) { cat("boottest error:", conditionMessage(e), "\n"); NULL })
  out <- list(asy = asy)
  if (!is.null(bt)) {
    cat(sprintf("   wild-bootstrap (Webb, B=9999): p=%.4f  95%% CI=[%.2f, %.2f]\n",
                bt$p_val, bt$conf_int[1], bt$conf_int[2]))
    out$boot <- list(p = bt$p_val, ci = as.numeric(bt$conf_int))
  }
  out
}

res <- boot_trad()
saveRDS(res, glue("{data}/wild_bootstrap.rds"))
cat("Saved -> data/wild_bootstrap.rds\n")
