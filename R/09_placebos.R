#-------------------------------------------------------#
# Coffee & climate change replication
# 09_placebos.R
# Placebo by farm size. Credit/management visits should move tree cover on the
# SMALL farms in the baseline sample but NOT on medium/large farms (which are
# not the policy target). Reproduces Table `did_placebo_finca_tamano_all`
# (Traditional small/med-large = cols 1-2, Technified = cols 3-4).
#
# Memory note: each size band is read and processed one at a time (the
# technified small band alone has ~2.8M rows), with gc() between bands.
#-------------------------------------------------------#

source("R/01_setup.R")

# Traditional uses credit OR management; technified uses credit advisory only
# (matching the 2024 placebo scripts 07b / 07e).
run_band <- function(size) {
  raw <- read_panel_raw(size)
  nv  <- compute_num_vis(raw, "num_cred_ges")
  pr  <- prep_controls(raw)
  out <- list(
    trad = did_fe3(build_treatment(is_traditional(pr), nv, "tiene_cred_ges")),
    tec  = did_fe3(build_treatment(is_technified(pr),  nv, "tiene_asesoria_cred")),
    trad_s = build_treatment(is_traditional(pr), nv, "tiene_cred_ges"),
    tec_s  = build_treatment(is_technified(pr),  nv, "tiene_asesoria_cred"))
  rm(raw, pr); gc()
  out
}

small <- run_band("small")
med   <- run_band("med")

cols <- list(`Trad Small` = small$trad_s, `Trad Med` = med$trad_s,
             `Tec Small`  = small$tec_s,  `Tec Med`  = med$tec_s)
models <- list(`Trad Small` = small$trad, `Trad Med` = med$trad,
               `Tec Small`  = small$tec,  `Tec Med`  = med$tec)
saveRDS(lapply(models, broom::tidy), glue("{data}/placebo_size.rds"))

etable(models, keep = "%^int$", dict = c(int = "Treatment*Period"),
       extralines = list(
         "Mean of dep. var." = sapply(cols, mean_forest),
         "Farm mean size"    = sapply(cols, mean_size),
         "Farm size band"    = c("Small", "Med. and large",
                                 "Small", "Med. and large")),
       tex = TRUE, replace = TRUE,
       file = glue("{out_tab}/did_placebo_finca_tamano_all.tex"),
       title = paste("Placebo test. Effect of credit/management visits on tree",
                     "cover by farm size band."))

cat("\n== Placebo by size: Treatment*Period ==\n")
cat(sprintf("Traditional:  Small=%.2f  Med/large=%.2f  (paper: -9.91, 9.34)\n",
            coef(models$`Trad Small`)["int"], coef(models$`Trad Med`)["int"]))
cat(sprintf("Technified:   Small=%.2f  Med/large=%.2f  (paper: -4.64, 15.25)\n",
            coef(models$`Tec Small`)["int"],  coef(models$`Tec Med`)["int"]))
