#-------------------------------------------------------#
# Coffee & climate change replication
# 03_parallel_trends.R
# Event-study test of the parallel-trends assumption for both crop groups.
# Reproduces Figure `parallel_trends_all_groups`.
# Window: relative year -4 .. +4, with -1 (2009) as the omitted reference.
# Treatment = credit OR management visits (tiene_cred_ges), as in the baseline.
#-------------------------------------------------------#

source("R/01_setup.R")

L         <- load_panel()
panel_reg <- L$panel_reg
num_vis   <- L$num_vis

# Event-study estimation for one crop-group sample ----------------------------
# Builds the relative-time dummies and interacts them with treatment, then
# extracts the treatment x relative-time coefficients (the dynamic ATTs).
# map a safe column name -> (relative time, calendar year)
rel_map <- data.frame(
  col  = c("rm4", "rm3", "rm2", "r0", "rp1", "rp2", "rp3", "rp4"),
  rel  = c(-4, -3, -2, 0, 1, 2, 3, 4),
  year = c(2006, 2007, 2008, 2010, 2011, 2012, 2013, 2014))

event_study <- function(d) {
  for (i in seq_len(nrow(rel_map))) {
    d[[rel_map$col[i]]] <- as.integer(d$year == rel_map$year[i])
  }
  terms <- rel_map$col
  fml <- as.formula(paste0(
    "forest_year ~ ",
    paste(sprintf("treatment:%s", terms), collapse = " + "), " + ",
    paste(terms, collapse = " + "), " + ",
    paste(controls, collapse = " + "),
    " | cod_finca + vereda_year"))
  m <- feols(fml, data = d, cluster = ~ cod_vereda)
  ct <- broom::tidy(m, conf.int = TRUE)
  ct <- ct[grepl("^treatment:", ct$term), ]
  ct$rel <- rel_map$rel[match(sub("treatment:", "", ct$term), rel_map$col)]
  # add the omitted reference period (-1) at zero
  rbind(
    data.frame(term = "ref", estimate = 0, std.error = 0,
               statistic = 0, p.value = 1, conf.low = 0, conf.high = 0, rel = -1),
    ct[, c("term", "estimate", "std.error", "statistic",
           "p.value", "conf.low", "conf.high", "rel")]
  )
}

es_trad <- event_study(build_treatment(is_traditional(panel_reg), num_vis))
es_tec  <- event_study(build_treatment(is_technified(panel_reg),  num_vis))

es_trad$group <- "Traditional crops"
es_tec$group  <- "Technified crops"
es <- rbind(es_trad, es_tec)
saveRDS(es, glue("{data}/parallel_trends.rds"))

# Figure ----------------------------------------------------------------------
p <- ggplot(es, aes(rel, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", colour = "grey50") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  facet_wrap(~ group, scales = "free_y") +
  scale_x_continuous(breaks = -4:4) +
  labs(x = "Years relative to first credit/management visit (2010)",
       y = "Effect on tree cover (000 m2)") +
  theme_bw()
ggsave(glue("{out_fig}/parallel_trends_all_groups.jpeg"),
       p, width = 9, height = 4, dpi = 300)

# Pre-trend diagnostic --------------------------------------------------------
cat("\n== Event study: pre-period coefficients (should be ~0, insignificant) ==\n")
for (g in unique(es$group)) {
  pre <- es[es$group == g & es$rel < 0 & es$rel != -1, ]
  cat(sprintf("%-18s pre-coefs: %s | max |t| = %.2f\n", g,
              paste(sprintf("%+.2f", pre$estimate), collapse = " "),
              max(abs(pre$statistic), na.rm = TRUE)))
}
cat("Figure written to", glue("{out_fig}/parallel_trends_all_groups.jpeg"), "\n")
