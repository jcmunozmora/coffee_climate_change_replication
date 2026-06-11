# Coffee & Climate Change — Replication

Replication code and **anonymized** data for Helo et al. (2026), *The effect of
conditional credit and business-management counseling visits on tree cover in
Colombian coffee farms* (difference-in-differences, treatment cutoff 2010).

## What this repository contains

A self-contained pipeline that reproduces the paper's results **starting from an
anonymized farm-level panel** plus a set of anonymized auxiliary datasets. The
raw data-construction stage (satellite forest cover, georeferenced SICA
coffee-grower registry, climate rasters) is **not** included: it relies on
confidential, georeferenced records and is therefore excluded by design.

```
.
├── R/
│   ├── 00_anonymize_data.R    # one-time, run by data owner (needs private raw data)
│   ├── 01_setup.R             # paths, packages, shared panel-prep helpers
│   └── 02_baseline_did.R      # Table: baseline DiD  [reproduced]
├── data/
│   ├── panel_finca_regresiones_anon.rds   # main analysis panel
│   └── aux/                                # anonymized auxiliary datasets
├── output/{tables,figures}/   # generated exhibits
├── run_all.R                  # master script
└── docs/                      # notes
```

## Data & anonymization

The analysis panel is `data/panel_finca_regresiones_anon.rds`
(7,492,740 obs × 57 vars; small producers <5 ha; 2000–2014).

Identifiers were anonymized with **independent random sequential IDs**
(`R/00_anonymize_panel.R`):

| Original (real DANE codes) | Anonymized |
|---|---|
| `cod_finca` (688,044 farms)    | random 1…N |
| `cod_vereda` (15,703 villages) | random 1…N |
| `cod_mpio` (598 municipalities)| random 1…N |
| `vereda_year`, `municipio_year`| regenerated group ids (used only as fixed effects) |

A **single master crosswalk** is built from the union of codes across the panel
**and all auxiliary datasets**, so each farm/village/municipality keeps the same
anonymous id everywhere and the cross-file joins (panel ↔ La Niña / vulnerability
/ extension agents / Finagro credit / census) survive anonymization. Text
geographic identifiers (municipality/department/committee names) are dropped.

The real↔anon crosswalk (`data_raw/id_crosswalk.rds`) and all raw inputs are kept
in `data_raw/`, which is **git-ignored and never distributed**.

### Auxiliary datasets (`data/aux/`, anonymized)

| File | Join key | Feeds |
|---|---|---|
| `coffee_farms_la_nina_effects.rds`       | `cod_finca` | La Niña robustness |
| `coffee_farms_vulnerability_index.rds`   | `cod_finca` | climate-vulnerability heterogeneity |
| `extensionistas_mpios_2006-2008.rds`     | `cod_mpio`  | extension-agent robustness |
| `credito_2010_2014.rds` (Finagro)        | `cod_mpio` (`dpmp`) | deforestation–credit |
| `fincas_sica_cenicafe.rds`               | `cod_finca` | Cenicafé placebos |
| `codigos_finca_lote_mpio_vereda.rds`     | `id_lote`,`cod_vereda` | production-cycle robustness |
| `censo_hogares_vivienda.rds`             | `cod_finca` | census balance |

> Note: the vulnerability `.dbf` stored the 9–10 digit farm codes as doubles with
> floating-point noise; `00_anonymize_data.R` rounds codes to integers before
> mapping, which restores the intended 100% farm-level join coverage.

**Validation.** Anonymization preserves the estimates exactly. The baseline ATT
(`Treatment*Period`) reproduces the paper to the second decimal:

| Spec (FE)                  | Traditional | Technified |
|----------------------------|:-----------:|:----------:|
| (1) Farm                   | −13.18      | −7.10      |
| (2) Farm + Municipality×Yr | −11.54      | −4.44      |
| (3) Farm + Village×Yr      | −9.91       | −4.23      |

### Replication notes (non-obvious choices, faithful to the originals)

- **Treatment definition varies by crop group in the 2024 robustness/heterogeneity
  scripts.** Traditional crops use *credit OR business-management* visits
  (`tiene_cred_ges`); technified/young crops use *credit advisory only*
  (`tiene_asesoria_cred`). The contamination filters and the pre-2010 visit list
  are `cred_ges`-based in both. Reproducing this asymmetry is required to match
  the published tables.
- **`edad2` is re-derived after the time-trend interaction** in the
  heterogeneity / robustness scripts (`edad2 = edad^2` on the interacted `edad`),
  unlike the baseline. Replicated for fidelity.
- **The IDEAM auxiliary tables are not de-duplicated** before joining, so a few
  hundred farms are double-counted (`relationship = "many-to-many"`). Kept as-is
  to match the published coefficients exactly.

## How to run

```bash
Rscript run_all.R
```

Requires R (≥4.3) with `tidyverse`, `glue`, `fixest`, `broom` (auto-installed via
`pacman`). The original manuscript used `lfe::felm`; this repo uses
`fixest::feols`, which yields identical point estimates and clustered standard
errors (clustered at the village `cod_vereda` level).

## Status / roadmap

**Reproducible now (validated against the paper, to the second decimal):**
- [x] Baseline DiD — `did_baseline_finca_all` (−13.18/−11.54/−9.91 · −7.10/−4.44/−4.23)
- [x] Parallel-trends event study — `parallel_trends_all_groups` (figure)
- [x] Seed-change mechanism — `mechanism_seed_change_all` (ext. 0.19 trad · 0.16 tec)
- [x] Climate-vulnerability heterogeneity — `heterogeneity_vul_clim_all` (−15.01/−7.99/−2.69/−5.30)
- [x] La Niña robustness — `robustness_lanina_all` (−13.00/−2.21/−3.67/−8.99)
- [x] Extension-agent robustness — `robustness_extension_all` (−24.06/−6.57/−5.54/−3.20)
- [x] Placebo by farm size — `did_placebo_finca_tamano_all` (−9.91/9.34/−4.64/15.25)
- [x] Deforestation–credit (Finagro) — `lm_deforestacion_num_credito` (−3.75/−3.72/−291.52/−248.85)
- [x] Pre-treatment balance — `dif_means_treated_control_pre_all` (all variables match)
- [x] Productive-cycle robustness — `robustness_prod_cycle_all` (−13.24/−10.73/−4.13/−3.66 vs paper −13.35/−10.70/−3.88/−4.42)
- [x] Staggered DiD, Callaway–Sant'Anna (technified) — `did_staggered_*_young`
      (simple ATT −16.42, matches the paper exactly; `R/07b_staggered_tec.R`,
      ~11 min, ~2 GB). The paper reports the staggered estimator for the
      technified group only; `R/07_staggered_did.R` also runs a traditional
      version (raw controls, not a published exhibit).

**Frontier robustness routines (audit add-ons, `R/10_frontier_robustness.R`):**
Goodman-Bacon decomposition + Rambachan-Roth HonestDiD sensitivity. Implemented
and structurally validated; computationally heavy on this N — run as a separate
long job rather than inside `run_all.R`.

**To be ported next — data already anonymized and available in `data/`:**

_Panel only:_
- [ ] Staggered DiD (Callaway–Sant'Anna, Borusyak) — `did_staggered_*`
- [ ] Placebos (farm size, visit type) — `did_placebo_*`
- [ ] Combined-visit specification — `did_combinado_finca_all`

_Not reproducible from the shared anonymized data:_
- [ ] Treated–control maps — need farm coordinates (privacy-restricted).

**Excluded by design (not distributable):** the data-construction stage
(`00_Scripts/` 00–01, plus `03a`), which needs the georeferenced SICA registry,
and treated/control **maps**, which require farm coordinates.
```
```
