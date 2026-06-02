# clean_pisa_data.R
# Cleans PISA weighted country means (from fetch_pisa.R) into the standard
# dashboard schema.
#
# Input:  data/raw/pisa/pisa_wt_means_all.rds
# Output: data/clean/pisa_means.rds
#
# Schema: source_code · plot · level · gender · country_iso3 · year · value
#
# Cross-cycle comparability caveat (same spirit as UDISE old-vs-NEP handling):
# PISA rescaled the Reading domain after 2018, and COVID-era disruptions caused
# large score drops in many countries in 2022. Scores are plotted across all
# cycles, but the app should carry a footnote that year-to-year comparisons
# must be interpreted with caution. Do NOT use these to claim "Country X improved
# its reading score by N points from 2015 to 2022."
#
# India: not in any PISA cycle — correctly absent from output.

library(dplyr)
library(tidyr)
library(readr)

PISA_IN   <- "data/raw/pisa"
CLEAN_OUT <- "data/clean"

# ── Country code mapping ──────────────────────────────────────────────────────
# PISA uses mostly ISO3 country codes, with the following exceptions:

# Codes that need remapping to ISO3
PISA_REMAP <- c(
  "TAP" = "TWN",   # Chinese Taipei → Taiwan
  "KSV" = "XKX",   # Kosovo → World Bank code for Kosovo
  "ROM" = "ROU"    # Old Romania code used in 2000 cycle
)

# Sub-national / regional entities — exclude (not sovereign countries).
# All Q* codes are sub-national except QAT (Qatar).
# Also exclude YUG (Yugoslavia, dissolved).
PISA_SUBNATIONAL <- c(
  "QCN",  # Shanghai-China
  "QHP",  # Himachal Pradesh, India
  "QTN",  # Tamil Nadu, India
  "QVE",  # Miranda, Venezuela
  "QRS",  # Perm, Russian Federation
  "QCH",  # B-S-J-G (China)
  "QES",  # Spain Regions
  "QAR",  # Buenos Aires, Argentina
  "QUC",  # Massachusetts, USA
  "QUE",  # North Carolina, USA
  "QUD",  # Puerto Rico, USA
  "QAZ",  # Baku, Azerbaijan
  "QCI",  # B-S-J-Z (China)
  "QMR",  # Moscow Region, Russia
  "QRT",  # Tatarstan, Russia
  "QUR",  # Ukrainian regions
  "YUG"   # Yugoslavia (dissolved)
)

# ── Load raw means ────────────────────────────────────────────────────────────
raw_path <- file.path(PISA_IN, "pisa_wt_means_all.rds")
if (!file.exists(raw_path)) stop("Run fetch_pisa.R first: ", raw_path, " not found.")

pisa_raw <- readRDS(raw_path)

message("Raw rows: ", nrow(pisa_raw))
message("Raw country codes: ", n_distinct(pisa_raw$country))

# ── Apply country code fixes ──────────────────────────────────────────────────
pisa_clean <- pisa_raw |>
  mutate(
    country_code = as.character(country),
    # Remap non-standard codes to ISO3
    country_iso3 = recode(country_code, !!!PISA_REMAP, .default = country_code)
  ) |>
  filter(!country_code %in% PISA_SUBNATIONAL)

# Report what was dropped and remapped
dropped <- pisa_raw |>
  filter(as.character(country) %in% PISA_SUBNATIONAL) |>
  distinct(country) |> pull(country)
remapped <- PISA_REMAP[names(PISA_REMAP) %in% unique(pisa_clean$country_code)]

if (length(dropped) > 0)
  message("Excluded sub-nationals: ", paste(dropped, collapse = ", "))
if (length(remapped) > 0)
  message("Remapped to ISO3: ", paste(names(remapped), "->", remapped, collapse = "; "))

# ── Pivot to long format and apply dashboard schema ───────────────────────────
pisa_long <- pisa_clean |>
  select(year, country_iso3, mean_math, mean_read, mean_sci) |>
  pivot_longer(
    cols      = c(mean_math, mean_read, mean_sci),
    names_to  = "domain",
    values_to = "value"
  ) |>
  filter(!is.na(value)) |>
  mutate(
    plot = recode(domain,
                  "mean_math" = "PISA_MATH",
                  "mean_read" = "PISA_READ",
                  "mean_sci"  = "PISA_SCI"),
    source_code = recode(domain,
                         "mean_math" = "PISA_MEAN_MATH",
                         "mean_read" = "PISA_MEAN_READ",
                         "mean_sci"  = "PISA_MEAN_SCI"),
    level  = "Secondary",   # PISA tests 15-year-olds (ISCED 2–3)
    gender = "Total",
    year   = as.integer(year)
  ) |>
  select(source_code, plot, level, gender, country_iso3, year, value) |>
  arrange(plot, country_iso3, year)

# ── Diagnostics ───────────────────────────────────────────────────────────────
message("\nTotal rows: ", nrow(pisa_long))
message("India present: ", any(pisa_long$country_iso3 == "IND"))

message("\nCoverage by plot:")
pisa_long |>
  group_by(plot) |>
  summarise(
    countries = n_distinct(country_iso3),
    cycles    = n_distinct(year),
    years     = paste(sort(unique(year)), collapse = ", "),
    .groups   = "drop"
  ) |>
  print()

message("\nSample — India neighbours in Math 2022:")
pisa_long |>
  filter(plot == "PISA_MATH", year == 2022) |>
  arrange(value) |>
  slice(c(1:3, (n()-2):n())) |>
  print()

# ── Save ──────────────────────────────────────────────────────────────────────
saveRDS(pisa_long, file.path(CLEAN_OUT, "pisa_means.rds"))
message("\nSaved: data/clean/pisa_means.rds")
