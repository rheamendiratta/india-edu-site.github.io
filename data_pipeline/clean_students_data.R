# clean_students_data.R
# Merges WB + UDISE data into app-ready RDS files for World > Students app
# Output files (copy these into shiny/world/students/data/ before deploying):
#   data/clean/students_wb.rds     — WB data, all countries, long format
#   data/clean/students_udise.rds  — UDISE data, India only, long format

library(dplyr)
library(readr)
library(arrow)
library(fs)

CLEAN_OUT <- "data/clean"
dir_create(CLEAN_OUT)

# ── Aggregate codes to exclude (WB regions, income groups etc) ─────────────────
WB_AGGREGATES <- c(
  "WLD", "EAS", "ECS", "LCN", "MEA", "NAC", "SAS", "SSF",
  "AFE", "AFW", "ARB", "CEB", "EAP", "ECA", "EMU",
  "HIC", "HPC", "IBD", "IBT", "IDA", "IDX", "LAC",
  "LDC", "LIC", "LMC", "LMY", "LTE", "MIC", "MNA",
  "OED", "OSS", "PRE", "PSS", "PST", "SSA",
  "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC"
)

# ── Indicator metadata ─────────────────────────────────────────────────────────
# Maps source_code -> plot, level, gender for easy filtering in the app

indicator_meta <- tribble(
  ~source_code,                  ~plot,   ~level,           ~gender,
  # GER
  "WB_WDI_SE_PRE_ENRR",          "GER",   "Pre-primary",    "Total",
  "WB_WDI_SE_PRE_ENRR_FE",       "GER",   "Pre-primary",    "Female",
  "WB_WDI_SE_PRE_ENRR_MA",       "GER",   "Pre-primary",    "Male",
  "WB_WDI_SE_PRM_ENRR",          "GER",   "Primary",        "Total",
  "WB_WDI_SE_PRM_ENRR_FE",       "GER",   "Primary",        "Female",
  "WB_WDI_SE_PRM_ENRR_MA",       "GER",   "Primary",        "Male",
  "WB_WDI_SE_SEC_ENRR",          "GER",   "Secondary",      "Total",
  "WB_WDI_SE_SEC_ENRR_FE",       "GER",   "Secondary",      "Female",
  "WB_WDI_SE_SEC_ENRR_MA",       "GER",   "Secondary",      "Male",
  "WB_WDI_SE_TER_ENRR",          "GER",   "Tertiary",       "Total",
  "WB_WDI_SE_TER_ENRR_FE",       "GER",   "Tertiary",       "Female",
  "WB_WDI_SE_TER_ENRR_MA",       "GER",   "Tertiary",       "Male",
  # NER
  "WB_WDI_SE_PRM_NENR",          "NER",   "Primary",        "Total",
  "WB_WDI_SE_PRM_NENR_FE",       "NER",   "Primary",        "Female",
  "WB_WDI_SE_PRM_NENR_MA",       "NER",   "Primary",        "Male",
  "WB_WDI_SE_SEC_NENR",          "NER",   "Secondary",      "Total",
  "WB_WDI_SE_SEC_NENR_FE",       "NER",   "Secondary",      "Female",
  "WB_WDI_SE_SEC_NENR_MA",       "NER",   "Secondary",      "Male",
  # Repetition Rate
  "WB_WDI_SE_PRM_REPT_ZS",       "REP",   "Primary",        "Total",
  "WB_WDI_SE_PRM_REPT_FE_ZS",    "REP",   "Primary",        "Female",
  "WB_WDI_SE_PRM_REPT_MA_ZS",    "REP",   "Primary",        "Male",
  # Persistence
  "WB_WDI_SE_PRM_PRSL_ZS",       "PERS",  "Primary",        "Total",
  "WB_WDI_SE_PRM_PRSL_FE_ZS",    "PERS",  "Primary",        "Female",
  "WB_WDI_SE_PRM_PRSL_MA_ZS",    "PERS",  "Primary",        "Male"
)

# ── UDISE level -> WB level mapping ───────────────────────────────────────────
# Used to align UDISE overlay with the selected WB level in the app
udise_to_wb_level <- c(
  "Foundational"   = NA,            # no WB equivalent
  "Preparatory"    = "Primary",
  "Elementary"     = NA,            # combined, no direct WB equivalent
  "Middle"         = NA,            # Upper Primary, no WB equivalent
  "Secondary"      = "Secondary",
  "Higher Secondary" = NA           # no WB equivalent
)

# ── Load & clean WB data ───────────────────────────────────────────────────────
message("Loading WB data...")
wb_raw <- read_csv("data/raw/wb/students_raw.csv", show_col_types = FALSE)

students_wb <- wb_raw |>
  filter(!country_iso3 %in% WB_AGGREGATES) |>
  filter(!is.na(value), !is.na(year), !is.na(country_iso3)) |>
  inner_join(indicator_meta, by = "source_code") |>
  select(
    source_code,
    plot = plot.y,        # use the one from indicator_meta
    level,
    gender,
    country_iso3,
    year,
    value
  ) |>
  arrange(plot, level, gender, country_iso3, year)

message("WB rows: ", nrow(students_wb))
message("WB countries: ", n_distinct(students_wb$country_iso3))
message("India in WB: ", any(students_wb$country_iso3 == "IND"))

# ── Load & clean UDISE data ────────────────────────────────────────────────────
message("\nLoading UDISE data...")
udise_raw <- readRDS("data/raw/udise/udise_students.rds")

# Map UDISE indicator IDs to plot codes and add wb_level for matching
students_udise <- udise_raw |>
  mutate(
    plot = case_when(
      indicator_id == "4010" ~ "GER",
      indicator_id == "4011" ~ "NER",
      indicator_id == "4016" ~ "REP",
      indicator_id == "4033" ~ "PERS",
    ),
    # wb_level: which WB level this UDISE level corresponds to (NA = no WB match)
    wb_level = udise_to_wb_level[level]
  ) |>
  select(
    indicator_id,
    label,
    plot,
    udise_level = level,
    wb_level,
    gender,
    acad_year,
    year,
    value
  ) |>
  arrange(plot, udise_level, gender, year)

message("UDISE rows: ", nrow(students_udise))
message("UDISE years: ", min(students_udise$year), " to ", max(students_udise$year))

# ── Save ───────────────────────────────────────────────────────────────────────
saveRDS(students_wb,    file.path(CLEAN_OUT, "students_wb.rds"))
saveRDS(students_udise, file.path(CLEAN_OUT, "students_udise.rds"))

message("\n── Clean complete ──")
message("Saved: data/clean/students_wb.rds")
message("Saved: data/clean/students_udise.rds")

# ── Coverage check ─────────────────────────────────────────────────────────────
message("\nWB coverage by plot/level/gender:")
students_wb |>
  group_by(plot, level, gender) |>
  summarise(
    countries = n_distinct(country_iso3),
    min_year  = min(year),
    max_year  = max(year),
    india     = any(country_iso3 == "IND"),
    .groups   = "drop"
  ) |>
  print(n = 30)

message("\nUDISE coverage by plot/level/gender:")
students_udise |>
  group_by(plot, udise_level, wb_level, gender) |>
  summarise(
    min_year = min(year),
    max_year = max(year),
    .groups  = "drop"
  ) |>
  print(n = 40)
