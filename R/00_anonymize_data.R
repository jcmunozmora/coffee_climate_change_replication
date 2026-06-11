#-------------------------------------------------------#
# Coffee & climate change replication
# 00_anonymize_data.R
# Master anonymizer for the analysis panel AND the auxiliary datasets.
#
# Strategy (sequential random IDs, shared across ALL files):
#   A single MASTER crosswalk is built from the UNION of every code that appears
#   in any file, so that a farm/village/municipality/plot keeps the SAME
#   anonymous id everywhere. This is what makes the cross-file joins
#   (panel <-> la nina / vulnerability / extension / finagro / census) survive
#   anonymization.
#     cod_finca, cod_vereda, cod_mpio, id_lote -> random permutations of 1..N
#     vereda_year, municipio_year              -> regenerated group ids (FE only)
#   Text geographic identifiers (municipality/department/committee names,
#   coordinates) are dropped.
#
# Inputs : data_raw/panel_finca_regresiones.rds      (private)
#          data_raw/aux/*.rds | *.dbf | *.dta         (private)
# Outputs: data/panel_finca_regresiones_anon.rds      (distributed)
#          data/aux/*.rds                              (distributed)
#          data_raw/id_crosswalk.rds                   (PRIVATE, never shipped)
#-------------------------------------------------------#

suppressMessages({
  library(dplyr)
  library(haven)
  library(foreign)
})

set.seed(20260610)

raw      <- "data_raw"
raw_aux  <- "data_raw/aux"
out      <- "data"
out_aux  <- "data/aux"
dir.create(out_aux, showWarnings = FALSE, recursive = TRUE)

#-------------------------------------------------------#
# 1. Read raw files ----
#-------------------------------------------------------#
panel       <- readRDS(file.path(raw, "panel_finca_regresiones.rds"))
la_nina     <- readRDS(file.path(raw_aux, "coffee_farms_la_nina_effects.rds"))
vuln        <- foreign::read.dbf(file.path(raw_aux,
                  "coffee_farms_vulnerability_index.dbf"), as.is = TRUE)
extension   <- readRDS(file.path(raw_aux, "extensionistas_mpios_2006-2008.rds"))
finagro     <- readRDS(file.path(raw_aux, "credito_2010_2014.rds"))
cenicafe    <- readRDS(file.path(raw_aux, "fincas_sica_cenicafe.rds"))
codigos     <- readRDS(file.path(raw_aux, "codigos_finca_lote_mpio_vereda.rds"))
plots       <- readRDS(file.path(raw_aux, "coffee_plots_forest_cover_full.rds"))
censo       <- haven::read_dta(file.path(raw_aux, "censo_hogares_vivienda.dta"))
censo       <- censo %>% mutate(across(everything(), as.numeric))  # drop labels

#-------------------------------------------------------#
# 2. Build MASTER maps from the union of all codes ----
#-------------------------------------------------------#
# round(): the .dbf stores the large farm codes as doubles with floating-point
# noise, so codes that are really equal compare unequal. Rounding to the nearest
# integer collapses that noise and keeps codes consistent across files.
u <- function(...) sort(unique(round(as.numeric(unlist(list(...))))))

finca_u  <- u(panel$cod_finca, la_nina$cod_finca, vuln$cod_fnc,
              cenicafe$cod_finca, codigos$cod_finca, censo$cod_finca)
vereda_u <- u(panel$cod_vereda, la_nina$cod_vereda, vuln$cod_vrd,
              cenicafe$cod_vereda, codigos$cod_vereda, plots$cod_vereda)
mpio_u   <- u(panel$cod_mpio, la_nina$codmpio, vuln$codmpio, codigos$codmpio,
              extension$codmpio, censo$cod_mpio, as.integer(finagro$dpmp))
lote_u   <- u(codigos$id_lote, plots$id_lote)

map <- function(keys) {
  tibble(orig = keys, anon = sample.int(length(keys), length(keys)))
}
m_finca  <- map(finca_u)
m_vereda <- map(vereda_u)
m_mpio   <- map(mpio_u)
m_lote   <- map(lote_u)

ap <- function(x, m) m$anon[match(round(as.numeric(x)), m$orig)]  # apply a map

#-------------------------------------------------------#
# 3. Anonymize the panel ----
#-------------------------------------------------------#
panel <- panel %>%
  mutate(cod_finca  = ap(cod_finca,  m_finca),
         cod_vereda = ap(cod_vereda, m_vereda),
         cod_mpio   = ap(cod_mpio,   m_mpio)) %>%
  group_by(cod_vereda, year) %>% mutate(vereda_year    = cur_group_id()) %>% ungroup() %>%
  group_by(cod_mpio,   year) %>% mutate(municipio_year = cur_group_id()) %>% ungroup()
saveRDS(panel, file.path(out, "panel_finca_regresiones_anon.rds"))

#-------------------------------------------------------#
# 4. Anonymize the auxiliary datasets ----
#    (map id columns; drop text geographic identifiers)
#-------------------------------------------------------#

# 4a. La Nina shocks (joins panel by cod_finca) -------------------------------
la_nina <- la_nina %>%
  mutate(cod_finca  = ap(cod_finca,  m_finca),
         cod_vereda = ap(cod_vereda, m_vereda),
         cod_mpio   = ap(codmpio,    m_mpio)) %>%
  select(cod_mpio, cod_vereda, cod_finca, treatment, group, alter_preci, categoria)
saveRDS(la_nina, file.path(out_aux, "coffee_farms_la_nina_effects.rds"))

# 4b. Climate vulnerability index (joins panel by cod_finca) ------------------
vuln <- vuln %>%
  transmute(cod_mpio   = ap(codmpio, m_mpio),
            cod_vereda = ap(cod_vrd,  m_vereda),
            cod_finca  = ap(cod_fnc,  m_finca),
            treatment  = tretmnt, group = group, vulnerability = vlnrbld)
saveRDS(vuln, file.path(out_aux, "coffee_farms_vulnerability_index.rds"))

# 4c. Extension agents (municipality level; joins panel by cod_mpio) ----------
extension <- extension %>%
  transmute(cod_mpio = ap(codmpio, m_mpio), year = year,
            coordinadores = coordinadores, extensionistas = extensionistas)
saveRDS(extension, file.path(out_aux, "extensionistas_mpios_2006-2008.rds"))

# 4d. Finagro credit (municipality level; dpmp -> cod_mpio) -------------------
finagro <- finagro %>%
  transmute(dpmp = ap(dpmp, m_mpio), nivel_ruralidad = nivel_ruralidad,
            anio = anio, cadena = cadena, destino = destino,
            colocaciones_total = colocaciones_total,
            colocaciones_valor = colocaciones_valor)
saveRDS(finagro, file.path(out_aux, "credito_2010_2014.rds"))

# 4e. Cenicafe / program participation (joins panel by cod_finca) -------------
cenicafe <- cenicafe %>%
  mutate(cod_finca  = ap(cod_finca,  m_finca),
         cod_vereda = ap(cod_vereda, m_vereda))
saveRDS(cenicafe, file.path(out_aux, "fincas_sica_cenicafe.rds"))

# 4f. Plot<->farm<->village codes (joins by id_lote + cod_vereda) -------------
codigos <- codigos %>%
  transmute(cod_mpio   = ap(codmpio,    m_mpio),
            cod_vereda = ap(cod_vereda, m_vereda),
            cod_finca  = ap(cod_finca,  m_finca),
            id_lote    = ap(id_lote,    m_lote))
saveRDS(codigos, file.path(out_aux, "codigos_finca_lote_mpio_vereda.rds"))

# 4g. Household census (joins panel by cod_finca) -----------------------------
censo <- censo %>%
  mutate(cod_finca = ap(cod_finca, m_finca),
         cod_mpio  = ap(cod_mpio,  m_mpio)) %>%
  select(-any_of("cod_dpto"))
saveRDS(censo, file.path(out_aux, "censo_hogares_vivienda.rds"))

# 4h. Plot-level forest-cover panel (joins by id_lote + cod_vereda) -----------
plots <- plots %>%
  mutate(cod_vereda = ap(cod_vereda, m_vereda),
         id_lote    = ap(id_lote,    m_lote))
saveRDS(plots, file.path(out_aux, "coffee_plots_forest_cover_full.rds"))

#-------------------------------------------------------#
# 5. Save the master crosswalk (KEEP PRIVATE) ----
#-------------------------------------------------------#
saveRDS(list(finca = m_finca, vereda = m_vereda, mpio = m_mpio, lote = m_lote),
        file.path(raw, "id_crosswalk.rds"))

cat("Done.\n")
cat(sprintf("  master map sizes -> finca:%s vereda:%s mpio:%s lote:%s\n",
            nrow(m_finca), nrow(m_vereda), nrow(m_mpio), nrow(m_lote)))
cat("  anonymized panel + 7 aux datasets written to data/ and data/aux/\n")
