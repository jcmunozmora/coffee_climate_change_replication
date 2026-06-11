# Data Availability & Replication Statement

Helo et al. (2026), *The effect of conditional credit and business-management
counseling visits on tree cover in Colombian coffee farms.*

## Summary

All code and **anonymized** data needed to reproduce the paper's econometric
results are provided in this repository. The analysis runs end to end from an
anonymized, farm-level analysis panel plus eight anonymized auxiliary datasets.
The raw data-construction stage (georeferenced SICA coffee-grower registry,
Hansen satellite forest-cover rasters, ERA5/IDEAM climate layers) is **not**
distributed: it relies on confidential, georeferenced records.

## What is reproducible

Running `Rscript run_all.R` (plus the separate `R/10_frontier_robustness.R`)
reproduces, to the second decimal, the following exhibits:

| Exhibit | Script |
|---|---|
| Baseline DiD — `did_baseline_finca_all` | `R/02_baseline_did.R` |
| Parallel-trends event study — `parallel_trends_all_groups` | `R/03_parallel_trends.R` |
| Seed-change mechanism — `mechanism_seed_change_all` | `R/04_seed_change.R` |
| Climate-vulnerability heterogeneity — `heterogeneity_vul_clim_all` | `R/05_heterogeneity_climate.R` |
| La Niña robustness — `robustness_lanina_all` | `R/06_robustness_lanina.R` |
| Staggered DiD (Callaway–Sant'Anna) — `did_staggered_*` | `R/07_staggered_did.R` |
| Extension-agent robustness — `robustness_extension_all` | `R/08_robustness_extension.R` |
| Placebo by farm size — `did_placebo_finca_tamano_all` | `R/09_placebos.R` |
| Deforestation–credit (Finagro) — `lm_deforestacion_num_credito` | `R/11_robustness_finagro.R` |
| Productive-cycle robustness — `robustness_prod_cycle_all` | `R/12_robustness_prodcycle.R` |
| Pre-treatment balance — `dif_means_treated_control_pre_all` | `R/13_diff_means.R` |
| Goodman-Bacon decomposition + HonestDiD sensitivity | `R/10_frontier_robustness.R` |

The technified-group staggered estimator (`RUN_TEC=TRUE` in
`R/07_staggered_did.R`) is computationally heavy (millions of observations with
the bootstrap) and is left off by default.

## Anonymization

Identifiers were replaced with random sequential IDs from a **single master
crosswalk** built over the union of codes across the panel and every auxiliary
dataset, so each farm / village / municipality / plot keeps the same anonymous
ID everywhere and all cross-file joins survive (`R/00_anonymize_data.R`):

- `cod_finca` (farms), `cod_vereda` (villages), `cod_mpio` (municipalities),
  `id_lote` (plots) → random permutations of `1..N`.
- `vereda_year`, `municipio_year` → regenerated group IDs (used only as fixed
  effects).
- Text geographic identifiers (municipality / department / committee names) and
  all coordinates are dropped.

The real↔anonymous crosswalk and all raw inputs are kept in `data_raw/`, which
is git-ignored and never distributed. Anonymization preserves every estimate
exactly (verified against the published baseline and robustness tables).

## What is restricted

- **Treated/control maps** (`mapa_treatment_*`, buffers) require farm
  coordinates and are therefore not reproducible from the distributed data.
- The **raw data-construction pipeline** (satellite processing, SICA registry,
  panel assembly) is omitted for confidentiality.

These materials are available from the authors under a data-use agreement with
the Federación Nacional de Cafeteros de Colombia (FNC).

## Software

R (≥ 4.3) with `tidyverse`, `glue`, `fixest`, `broom`, `did`, `bacondecomp`,
`HonestDiD`, `haven`, `foreign` (auto-installed via `pacman`). The original
manuscript used `lfe::felm`; this repository uses `fixest::feols`, which yields
identical point estimates and village-clustered standard errors.

## Sources

SICA (FNC), Hansen et al. (2013) Global Forest Change, IDEAM climate-vulnerability
and La Niña data, and FINAGRO agricultural-credit records.
