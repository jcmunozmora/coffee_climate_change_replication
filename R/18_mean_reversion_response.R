#-------------------------------------------------------#
# Coffee & climate change replication
# 18_mean_reversion_response.R
# Direct response to the mean-reversion objection (cf. R/14). Treated farms
# enter with more tree cover, so high-cover units regressing toward the mean
# could mimic a negative DiD. Three tests that separate a treatment effect from
# mechanical reversion:
#
#  (A) Event study WITHIN the top baseline-cover quartile (where the effect
#      concentrates). Mechanical reversion implies high-cover treated farms are
#      already declining BEFORE 2010 -> downward pre-trend. A flat pre-trend
#      followed by a post-2010 drop is a treatment effect, not reversion.
#  (B) Reversion among CONTROLS only: regress tree cover on (top-quartile x
#      post) for control farms. If high-cover controls do NOT lose cover after
#      2010, there is no mechanical reversion for the within-quartile DiD to
#      pick up.
#  (C) Continuous baseline-cover x period control (a deliberately conservative
#      bound that also absorbs any genuinely larger effect among high-cover
#      farms, i.e. likely an OVER-correction).
#-------------------------------------------------------#

source("R/01_setup.R")
L <- load_panel(); panel_reg <- L$panel_reg; num_vis <- L$num_vis

base_cover <- panel_reg %>%
  dplyr::filter(year >= 2005 & year <= 2009) %>%
  group_by(cod_finca) %>%
  summarise(base_forest = mean(forest_year, na.rm = TRUE), .groups = "drop")

prep <- function(gf, tv) {
  d <- gf(panel_reg) %>% build_treatment(num_vis, tv) %>%
    left_join(base_cover, by = "cod_finca") %>% drop_na(base_forest)
  qs <- quantile(d$base_forest[!duplicated(d$cod_finca)], c(.25,.5,.75), na.rm = TRUE)
  d %>% mutate(topq = as.integer(base_forest >= qs[3]),       # Q4
               base_c = base_forest - mean(base_forest, na.rm = TRUE))
}
trad <- prep(is_traditional, "tiene_cred_ges")
tec  <- prep(is_technified,  "tiene_asesoria_cred")

rel_map <- data.frame(col = c("rm4","rm3","rm2","r0","rp1","rp2","rp3","rp4"),
                      year = c(2006,2007,2008,2010,2011,2012,2013,2014),
                      rel  = c(-4,-3,-2,0,1,2,3,4))

# (A) event study within the top baseline-cover quartile -----------------------
event_q4 <- function(d, tag) {
  d <- d %>% dplyr::filter(topq == 1)
  for (i in seq_len(nrow(rel_map))) d[[rel_map$col[i]]] <- as.integer(d$year == rel_map$year[i])
  fml <- as.formula(paste0("forest_year ~ ",
    paste(sprintf("treatment:%s", rel_map$col), collapse=" + "), " + ",
    paste(rel_map$col, collapse=" + "), " + ", paste(controls, collapse=" + "),
    " | cod_finca + vereda_year"))
  m <- feols(fml, data = d, cluster = ~ cod_vereda)
  ct <- broom::tidy(m); ct <- ct[grepl("^treatment:", ct$term), ]
  ct$rel <- rel_map$rel[match(sub("treatment:", "", ct$term), rel_map$col)]
  pre  <- ct[ct$rel < 0, ]; post <- ct[ct$rel >= 0, ]
  cat(sprintf("\n[A] %s, top cover quartile (Q4):\n", tag))
  cat(sprintf("    pre-period coefs: %s  (max |t| = %.2f)\n",
              paste(sprintf("%+.1f", pre$estimate), collapse=" "),
              max(abs(pre$statistic))))
  cat(sprintf("    post-period coefs: %s\n",
              paste(sprintf("%+.1f", post$estimate), collapse=" ")))
}

# (B) reversion among controls only -------------------------------------------
control_reversion <- function(d, tag) {
  d <- d %>% dplyr::filter(treatment == 0)
  m <- feols(forest_year ~ i(topq, period, ref = 0) | cod_finca + year,
             data = d, cluster = ~ cod_vereda)
  r <- broom::tidy(m)
  cat(sprintf("[B] %s, CONTROLS only -- top-quartile x post = %.2f (p=%.3f)\n",
              tag, r$estimate[1], r$p.value[1]))
}

# (C) conservative continuous baseline-cover x period bound -------------------
cover_trend_bound <- function(d, tag) {
  m <- feols(forest_year ~ int + base_c:period + .[controls] | cod_finca + vereda_year,
             data = d, cluster = ~ cod_vereda)
  r <- broom::tidy(m); r <- r[r$term == "int", ]
  cat(sprintf("[C] %s -- Treatment*Period with baseline-cover x period control = %.2f (p=%.3f)\n",
              tag, r$estimate, r$p.value))
}

for (x in list(list(trad,"Traditional"), list(tec,"Technified"))) {
  event_q4(x[[1]], x[[2]]); control_reversion(x[[1]], x[[2]]); cover_trend_bound(x[[1]], x[[2]])
}
cat("\nDONE 18_mean_reversion_response\n")
