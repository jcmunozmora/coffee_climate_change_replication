#-------------------------------------------------------#
# 16_like_for_like.R
# Referee bundle: is the baseline TWFE effect identified off clean variation,
# or off the fixed-effect/control structure? Referee D's diagnostic.
#
# Compare, per group, the Treatment*Period coefficient under three nested
# conditioning sets, ending at the conditioning the Callaway-Sant'Anna (C-S)
# estimator can actually support (farm + year FE, covariates via xformla, NO
# vereda-year FE, NO post-treatment seed-share controls):
#   (1) baseline col3      : full controls + cod_finca + vereda_year   [-9.91 / -4.23]
#   (2) drop seed shares   : controls minus prc_{castillo,colombia,tabi,tipica}
#   (3) C-S-matched TWFE    : C-S covariates only + cod_finca + year
# Then line them up against the C-S simple ATT. If the TWFE estimate marches
# toward the C-S (~0, imprecise) as the FE/controls are stripped, the headline
# is driven by the FE structure, not clean treated-vs-never-treated variation.
#-------------------------------------------------------#

source("R/01_setup.R")

p <- load_panel(); panel_reg <- p$panel_reg; num_vis <- p$num_vis

cs_cov <- c("total_mts","prc_cafe","ndensidad","edad","edad2","sd_temp","sd_rain")
ctrl_noseed <- setdiff(controls, c("prc_castillo","prc_colombia","prc_tabi","prc_tipica"))

fit_one <- function(d, rhs, fe) {
  f <- as.formula(paste0("forest_year ~ int + ", paste(rhs, collapse=" + "),
                         " | ", fe))
  m <- feols(f, data = d, cluster = ~ cod_vereda)
  r <- broom::tidy(m); r <- r[r$term == "int", ]
  c(beta = round(r$estimate,2), se = round(r$std.error,2), p = round(r$p.value,4))
}

run_group <- function(tag, group_filter, treat_var) {
  d <- panel_reg %>% group_filter() %>% build_treatment(num_vis, treat_var = treat_var)
  cat(sprintf("\n===== %s (n=%d, farms=%d) =====\n", tag, nrow(d),
              dplyr::n_distinct(d$cod_finca)))
  s1 <- fit_one(d, controls,    "cod_finca + vereda_year")  # baseline col3
  s2 <- fit_one(d, controls,    "cod_finca + year")          # full controls, year FE
  s3 <- fit_one(d, ctrl_noseed, "cod_finca + year")          # drop seed-share mediators
  s4 <- fit_one(d, cs_cov,      "cod_finca + year")          # C-S-matched conditioning
  tab <- rbind(`(1) baseline col3 [finca+vereda_year, full ctrl]` = s1,
               `(2) finca+year, full ctrl`                        = s2,
               `(3) finca+year, drop seed shares`                 = s3,
               `(4) C-S-matched [finca+year, C-S covs]`           = s4)
  print(tab); tab
}

t_trad <- run_group("TRADITIONAL", is_traditional, "tiene_cred_ges")
t_tec  <- run_group("TECHNIFIED",  is_technified,  "tiene_asesoria_cred")

cs_trad <- readRDS(glue("{data}/staggered_agg_trad.rds"))$simple
cat(sprintf("\nC-S simple ATT (traditional) = %.2f (se %.2f)\n",
            cs_trad$overall.att, cs_trad$overall.se))

saveRDS(list(traditional = t_trad, technified = t_tec,
             cs_trad = cs_trad), glue("{data}/like_for_like.rds"))
cat("\nSaved -> data/like_for_like.rds\n")
