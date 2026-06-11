#-------------------------------------------------------#
# Coffee & climate change replication
# 13_diff_means.R
# Pre-treatment (<=2009) balance table: mean characteristics of treated vs
# control farms. Reproduces Table `dif_means_treated_control_pre_all`.
# Treatment = credit OR management visits (tiene_cred_ges).
#-------------------------------------------------------#

source("R/01_setup.R")

raw <- readRDS(glue("{data}/panel_finca_regresiones_anon.rds")) %>%
  dplyr::select(-starts_with("area")) %>%
  dplyr::filter(total_mts < 50) %>%
  mutate(across(starts_with("prc"), ~ .x * 100),
         prc_nores = prc_caturra + prc_tipica,   # pest-vulnerable varieties
         tiene_cred_ges = ifelse(tiene_asesoria_cred == 1 | tiene_gest_emp == 1, 1, 0),
         num_cred_ges   = num_asesoria_cred + ges_emp) %>%
  dplyr::filter(year >= 2005)

num_vis <- compute_num_vis(raw, "num_cred_ges")

make_sample <- function(group_fun) {
  group_fun(raw) %>%
    mutate(treatment = ifelse(tiene_cred_ges == 1, 1, 0)) %>%
    dplyr::filter(!(treatment == 0 & tiene_cred_ges == 1)) %>%
    dplyr::filter(!(cod_finca %in% num_vis$cod_finca))
}

vars <- c(total_mts = "Farm size",
          prc_cafe = "Share sown with coffee (%)",
          ndensidad = "Crop density (trees/ha)",
          edad = "Coffee trees age (years)",
          prc_nores = "Share pest-vulnerable seeds (%)",
          temperature = "Temperature (Celsius)",
          rain = "Rainfall (m of water)",
          forest_year = "Farm tree cover")

balance <- function(d) {
  d <- d %>% dplyr::filter(year <= 2009)
  purrr::map_dfr(names(vars), function(v) {
    tt <- t.test(d[[v]] ~ d$treatment)   # estimate[1]=control(0), estimate[2]=treated(1)
    data.frame(Variable   = vars[[v]],
               Diff_means  = round(tt$estimate[2] - tt$estimate[1], 2),
               Mean_treated = round(tt$estimate[2], 2),
               Mean_control = round(tt$estimate[1], 2),
               P_value     = round(tt$p.value, 2), row.names = NULL)
  })
}

bal_trad <- balance(make_sample(is_traditional))
bal_tec  <- balance(make_sample(is_technified))
saveRDS(list(traditional = bal_trad, technified = bal_tec),
        glue("{data}/diff_means_balance.rds"))

writeLines(c(
  "Traditional crops", utils::capture.output(print(bal_trad, row.names = FALSE)),
  "", "Technified crops", utils::capture.output(print(bal_tec, row.names = FALSE))),
  glue("{out_tab}/dif_means_treated_control_pre_all.txt"))

cat("\n== Pre-treatment balance (traditional) ==\n")
print(bal_trad, row.names = FALSE)
cat(sprintf("\nFarm size: diff=%.2f (treated %.2f vs control %.2f)  paper: 3.47 (21.59 vs 18.12)\n",
            bal_trad$Diff_means[1], bal_trad$Mean_treated[1], bal_trad$Mean_control[1]))
