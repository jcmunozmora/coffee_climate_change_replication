#-------------------------------------------------------#
# Coffee & climate change replication
# 10_frontier_robustness.R
# Modern-DiD robustness routines recommended in the econometric audit:
#   (A) Goodman-Bacon (2021) decomposition of the TWFE estimand.
#   (B) Rambachan-Roth (2023) HonestDiD sensitivity to parallel-trends
#       violations, applied to the baseline event study.
# Run on the traditional group (tractable N). The same code runs on the
# technified group by swapping is_traditional -> is_technified.
#-------------------------------------------------------#

source("R/01_setup.R")
pacman::p_load(bacondecomp, HonestDiD)

L         <- load_panel()
panel_reg <- L$panel_reg
num_vis   <- L$num_vis
raw       <- L$panel_finca

#-------------------------------------------------------#
# A. Goodman-Bacon decomposition ----
# How much of the TWFE estimate comes from "clean" (treated vs never-treated)
# comparisons vs "forbidden" (already-treated as control) comparisons?
#-------------------------------------------------------#
stag <- raw %>%
  dplyr::filter(year >= 2005) %>%
  mutate(year_cred = ifelse(num_cred_ges > 0, year, NA)) %>%
  group_by(cod_finca) %>%
  mutate(treat = ifelse(tiene_cred_ges == 1,
                        suppressWarnings(min(year_cred, na.rm = TRUE)), 0)) %>%
  ungroup() %>%
  mutate(treat = ifelse(is.infinite(treat), NA, treat)) %>%
  drop_na(treat) %>%
  is_traditional() %>%
  dplyr::filter(treat >= 2010 | treat == 0) %>%
  dplyr::filter(!(treat == 0 & tiene_cred_ges == 1)) %>%
  dplyr::filter(!(cod_finca %in% num_vis$cod_finca)) %>%
  mutate(did_post = as.integer(treat > 0 & year >= treat)) %>%
  drop_na(forest_year)

# bacon() needs a balanced panel: keep farms observed in all years
balanced <- stag %>% count(cod_finca) %>%
  dplyr::filter(n == max(n)) %>% pull(cod_finca)
stag_bal <- stag %>% dplyr::filter(cod_finca %in% balanced)

cat("== Goodman-Bacon decomposition (traditional) ==\n")
bacon_out <- tryCatch(
  bacon(forest_year ~ did_post, data = as.data.frame(stag_bal),
        id_var = "cod_finca", time_var = "year"),
  error = function(e) {cat("bacon error:", conditionMessage(e), "\n"); NULL})

if (!is.null(bacon_out)) {
  w <- aggregate(weight ~ type, data = bacon_out, FUN = sum)
  est <- with(bacon_out, weighted.mean(estimate, weight))
  print(w)
  cat(sprintf("Weighted TWFE estimand = %.2f\n", est))
  saveRDS(bacon_out, glue("{data}/goodman_bacon_trad.rds"))
}

#-------------------------------------------------------#
# B. HonestDiD sensitivity (Rambachan-Roth 2023) ----
# How large a deviation from parallel trends (relative to the max pre-period
# deviation, M-bar) is needed before the post-period effect loses significance?
#-------------------------------------------------------#
rel_map <- data.frame(
  col  = c("rm4", "rm3", "rm2", "r0", "rp1", "rp2", "rp3", "rp4"),
  year = c(2006, 2007, 2008, 2010, 2011, 2012, 2013, 2014))

d <- build_treatment(is_traditional(panel_reg), num_vis)
for (i in seq_len(nrow(rel_map))) d[[rel_map$col[i]]] <- as.integer(d$year == rel_map$year[i])
terms <- rel_map$col
es <- feols(as.formula(paste0(
  "forest_year ~ ", paste(sprintf("treatment:%s", terms), collapse = " + "),
  " + ", paste(terms, collapse = " + "), " + ", paste(controls, collapse = " + "),
  " | cod_finca + vereda_year")), data = d, cluster = ~ cod_vereda)

idx     <- grep("^treatment:", names(coef(es)))
betahat <- as.numeric(coef(es)[idx])
sigma   <- as.matrix(vcov(es)[idx, idx])
n_pre   <- 3   # rel times -4,-3,-2
n_post  <- 5   # rel times 0,1,2,3,4

cat("\n== HonestDiD relative-magnitudes sensitivity (traditional) ==\n")
honest <- tryCatch(
  HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = betahat, sigma = sigma,
    numPrePeriods = n_pre, numPostPeriods = n_post,
    Mbarvec = seq(0.5, 2, by = 0.5)),
  error = function(e) {cat("HonestDiD error:", conditionMessage(e), "\n"); NULL})

if (!is.null(honest)) {
  orig <- HonestDiD::constructOriginalCS(betahat = betahat, sigma = sigma,
                                         numPrePeriods = n_pre, numPostPeriods = n_post)
  print(honest)
  cat(sprintf("Original CS: [%.2f, %.2f]\n", orig$lb, orig$ub))
  saveRDS(list(honest = honest, original = orig), glue("{data}/honestdid_trad.rds"))
  brk <- honest[honest$lb <= 0 & honest$ub >= 0, ]
  if (nrow(brk)) cat(sprintf("Breakdown: CI first includes 0 at Mbar = %.2f\n",
                             min(brk$Mbar)))
}
