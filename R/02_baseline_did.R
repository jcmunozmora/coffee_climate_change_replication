#-------------------------------------------------------#
# Coffee & climate change replication
# 02_baseline_did.R
# Baseline DiD: effect of conditional credit/management visits on tree cover.
# Reproduces Table `did_baseline_finca_all` (Traditional cols 1-3, Technified 4-6).
#-------------------------------------------------------#

source("R/01_setup.R")

p <- load_panel()
panel_reg <- p$panel_reg
num_vis    <- p$num_vis

# Group 1: Traditional crops (trad-trad, trad-young) --------------------------
reg_trad <- panel_reg %>%
  dplyr::filter((trad_young > 0) | (trad_trad > 0)) %>%
  build_treatment(num_vis)

# Group 2: Technified crops (young-*) -----------------------------------------
reg_tec <- panel_reg %>%
  dplyr::filter(trad_trad == 0 & trad_young == 0 & old_young == 0 & old_old == 0) %>%
  build_treatment(num_vis)

# Three specifications per group ----------------------------------------------
fes <- c(c1 = "cod_finca",
         c2 = "cod_finca + municipio_year",
         c3 = "cod_finca + vereda_year")

fit_all <- function(d) lapply(fes, function(fe)
  feols(baseline_formula(fe), data = d, cluster = ~ cod_vereda))

did_trad <- fit_all(reg_trad)
did_tec  <- fit_all(reg_tec)

# Save lightweight coefficient tables (full feols objects carry millions of
# residuals; the tidy summaries are all downstream tables need) ---------------
tidy_models <- function(lst) lapply(lst, broom::tidy)
saveRDS(tidy_models(did_trad), glue("{data}/did_baseline_trad.rds"))
saveRDS(tidy_models(did_tec),  glue("{data}/did_baseline_tec.rds"))

# Means for table footer ------------------------------------------------------
group_means <- function(d) {
  dd <- d %>% drop_na(forest_year, edad, sd_temp, sd_rain)
  list(forest = round(mean(dd$forest_year), 2),
       size   = round(mean(dd$total_mts_ori[dd$total_mts_ori > 0], na.rm = TRUE), 2))
}
m_trad <- group_means(reg_trad)
m_tec  <- group_means(reg_tec)

# Regression table ------------------------------------------------------------
etable(c(did_trad, did_tec),
       dict = c(int = "Treatment*Period", forest_year = "Tree cover (000 m2)"),
       keep = "%^int$",
       extralines = list("Mean of dep. var." = c(rep(m_trad$forest, 3), rep(m_tec$forest, 3)),
                         "Farm mean size"    = c(rep(m_trad$size, 3),   rep(m_tec$size, 3))),
       tex = TRUE,
       file = glue("{out_tab}/did_baseline_finca_all.tex"),
       replace = TRUE,
       title = "Baseline results. The effect of conditional credit visits on tree cover")

cat("\n== Baseline ATT (Treatment*Period) ==\n")
cat(sprintf("Traditional:  c1=%.2f  c2=%.2f  c3=%.2f\n",
            coef(did_trad$c1)["int"], coef(did_trad$c2)["int"], coef(did_trad$c3)["int"]))
cat(sprintf("Technified:   c1=%.2f  c2=%.2f  c3=%.2f\n",
            coef(did_tec$c1)["int"],  coef(did_tec$c2)["int"],  coef(did_tec$c3)["int"]))
cat("Table written to", glue("{out_tab}/did_baseline_finca_all.tex"), "\n")
